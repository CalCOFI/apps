# ctd-viz server — linked selection across the map + table; the plot follows.
#
# the selection unit is a station occupation (ord_occ within the loaded cruise).
# a single store, rv$sel_occ, is the source of truth. two writers (map / table)
# set it; the map + table updaters push it back to their views and the transect
# plot re-renders from it. loops are broken by a setequal() no-op guard in every
# writer. the map updater runs for EVERY source — clicking a maplibre feature
# does not auto-highlight it — while the table updater skips source == "table"
# (DT already shows the clicked rows). rv$sel_source carries that one distinction.
#
# the map writer is a transect picker: click a station to anchor, click another
# to select every station between them along the cruise track (ord_occ order).

server <- function(input, output, session) {

  rv <- reactiveValues(
    cruise_key   = NULL,           # loaded cruise
    all_casts    = NULL,           # df: every ctd_cast row for the cruise (per scan)
    map_casts    = NULL,           # sf: one row per ord_occ (station occupation)
    cruise_stats = NULL,           # tibble: cruise-level summary stats
    sel_occ      = character(0),   # selected ord_occ — THE selection store
    sel_anchor   = NULL,           # in-progress transect start (map picker)
    sel_source   = NULL)           # "map" | "table" | "reset"

  cruise_segments <- reactive({
    req(rv$map_casts)
    if (nrow(rv$map_casts) < 2) return(NULL)
    compute_segments(st_drop_geometry(rv$map_casts))
  })

  # canonical displayed cast table — drives the row-index <-> ord_occ mapping
  casts_tbl <- reactive({
    req(rv$map_casts)
    st_drop_geometry(rv$map_casts) |>
      transmute(
        occ          = ord_occ,
        site         = site_key,
        line, sta,
        datetime_utc = datetime_utc,
        cast_dir     = cast_dir,
        max_depth_m  = max_depth_m,
        n_depths     = n_depths) |>
      arrange(occ)
  })

  # transect labels (A, B, C, …) for the current selection, in cruise-track
  # order — one source of truth shared by the map markers and the plot
  sel_labels <- reactive({
    lab_map <- assign_occ_labels(rv$sel_occ)
    tibble(ord_occ = names(lab_map), label = unname(lab_map))
  })

  # --- load cruise — auto-fires on startup and on each cruise dropdown change
  observeEvent(input$sel_cruise, {
    req(input$sel_cruise)
    ck <- input$sel_cruise

    withProgress(message = "Loading cruise…", {
      d_all <- tbl(con, "ctd_cast") |>
        filter(cruise_key == !!ck) |>
        select(ctd_cast_uuid, ord_occ, cast_dir, datetime_utc,
               lat_dec, lon_dec, site_key, line, sta) |>
        collect()
      setProgress(0.4, detail = "stations…")

      # one map point per station occupation
      d_map <- d_all |>
        arrange(ord_occ, cast_dir, datetime_utc) |>
        distinct(ord_occ, .keep_all = TRUE)

      # per-occupation depth summary -> rich hover tooltip
      setProgress(0.6, detail = "depths…")
      occ_dep <- occ_depth_summary(con, ck)
      d_map <- d_map |>
        left_join(occ_dep, by = "ord_occ") |>
        mutate(
          n_depths = ifelse(is.na(n_depths), 0L, n_depths),
          label    = "",          # per-selection letter, filled via set_source
          tooltip  = paste0(
            "<b>Line / Sta:</b> ", line, " / ", sta, "<br>",
            "<b>Date:</b> ", format(datetime_utc, "%Y-%m-%d %H:%M"), " UTC<br>",
            "<b>Lon, Lat:</b> ", round(lon_dec, 4), ", ",
            round(lat_dec, 4), "<br>",
            "<b>Max depth:</b> ",
            ifelse(is.na(max_depth_m), "—", paste0(max_depth_m, " m")),
            " &middot; <b>", n_depths, "</b> retained depths"))
      map_casts_sf <- st_as_sf(
        d_map, coords = c("lon_dec", "lat_dec"), crs = 4326, remove = FALSE)

      setProgress(0.85, detail = "stats…")
      stats <- cruise_stats(con, ck)

      # no freezeReactiveValue on tbl_casts_rows_selected here: the table
      # updater below (selectRows on rv$sel_occ) clears the stale DT
      # selection, and each writer's setequal guard absorbs the echo. the
      # freeze invalidated the table writer mid-flush, aborting it before
      # its ignoreInit flag set — which then ate the first real selection.
      rv$cruise_key   <- ck
      rv$all_casts    <- d_all
      rv$map_casts    <- map_casts_sf
      rv$cruise_stats <- stats
      rv$sel_anchor   <- NULL
      rv$sel_source   <- "reset"
      rv$sel_occ      <- character(0)
    })
  })

  # --- render map ---------------------------------------------------------
  output$map_cruise <- renderMaplibre({
    req(rv$map_casts)
    mc   <- rv$map_casts
    segs <- cruise_segments()

    m <- maplibre(style = carto_style("voyager")) |>
      add_navigation_control() |>
      add_scale_control(position = "bottom-left") |>
      fit_bounds(mc) |>
      # GEBCO seafloor depth, drawn beneath the segments + casts. starts
      # visible or hidden per the sidebar toggle; the observer below flips
      # it live via a proxy, with no full re-render.
      add_image_source(
        id = "bathy-src", data = bathy_rast_map, colors = bathy_pal) |>
      add_raster_layer(
        id = "bathy", source = "bathy-src", raster_opacity = 0.6,
        visibility = if (isTRUE(input$chk_bathy)) "visible" else "none")

    if (!is.null(segs) && nrow(segs) > 0) {
      m <- m |>
        add_line_layer(
          id = "segments", source = segs,
          line_color = "#888888", line_width = 1.5, line_opacity = 0.6) |>
        add_line_layer(
          id = "sel-segments", source = segs,
          line_color = "#ff2d95", line_width = 3, line_opacity = 0)
    }

    m |>
      add_circle_layer(
        id = "casts", source = mc,
        circle_color = "#0077cc", circle_radius = 5, circle_opacity = 0.85,
        circle_stroke_color = "white", circle_stroke_width = 1,
        tooltip = "tooltip") |>
      add_circle_layer(            # selected casts — pink, drawn on top
        id = "sel-casts", source = mc,
        circle_color = "#ff2d95", circle_radius = 7, circle_opacity = 0,
        circle_stroke_color = "#ffffff", circle_stroke_width = 2) |>
      add_symbol_layer(            # lettered labels on the selected casts
        id = "sel-labels", source = mc,
        text_field = get_column("label"),   # data-driven, not the literal "label"
        text_size = 15, text_color = "#7a0046",
        text_halo_color = "#ffffff", text_halo_width = 1.6,
        text_offset = c(0, -1.3), text_allow_overlap = TRUE)
  })

  # toggle the bathymetry raster live, without re-rendering the whole map
  observeEvent(input$chk_bathy, {
    maplibre_proxy("map_cruise") |>
      set_layout_property(
        "bathy", "visibility",
        if (isTRUE(input$chk_bathy)) "visible" else "none")
  }, ignoreInit = TRUE)

  # === selection writers: each input -> the shared sel_occ store ===========

  # (1) map click — transect picker: anchor on the first click, then select
  #     every occupation between the anchor and the second click
  observeEvent(input$map_cruise_feature_click, {
    occ <- input$map_cruise_feature_click$properties$ord_occ
    req(occ, rv$map_casts)
    occ_all <- sort(rv$map_casts$ord_occ)            # cruise-track order

    if (is.null(rv$sel_anchor)) {
      rv$sel_anchor <- occ
      rv$sel_source <- "map"
      rv$sel_occ    <- occ
    } else {
      i  <- range(match(c(rv$sel_anchor, occ), occ_all))
      rng <- occ_all[i[1]:i[2]]
      rv$sel_anchor <- NULL                          # range complete
      if (!setequal(rng, rv$sel_occ)) {
        rv$sel_source <- "map"
        rv$sel_occ    <- rng
      }
    }
  }, ignoreInit = TRUE)

  # (2) table rows -> their occupations
  observeEvent(input$tbl_casts_rows_selected, {
    sel <- casts_tbl()$occ[input$tbl_casts_rows_selected]
    sel <- sel[!is.na(sel)]
    if (setequal(sel, rv$sel_occ)) return()
    rv$sel_anchor <- NULL          # a table pick ends any in-progress map range
    rv$sel_source <- "table"
    rv$sel_occ    <- sel
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # the transect plot is display-only — it re-renders from rv$sel_occ but does
  # not write back. (it had a plotly box-select writer; with stations clearly
  # lettered on both map and plot it was redundant, and a drag-to-zoom on the
  # plot unexpectedly re-filtered the map. dragmode is "zoom" now.)

  # === selection updaters: the store -> each view (skip the writing view) ==

  # store -> map proxy. runs for every source (including "map"): clicking a
  # maplibre feature does not auto-highlight it, so the proxy must (re)apply
  # the pink styling here regardless of who wrote the store. proxy ops don't
  # fire click events, so this can't loop.
  observeEvent(rv$sel_occ, {
    proxy  <- maplibre_proxy("map_cruise")
    mc_lab <- rv$map_casts
    mc_lab$label <- ""
    if (length(rv$sel_occ) == 0) {
      proxy |>
        set_paint_property("casts",        "circle-opacity", 0.85) |>
        set_paint_property("segments",     "line-opacity",   0.6) |>
        set_paint_property("sel-casts",    "circle-opacity", 0) |>
        set_paint_property("sel-segments", "line-opacity",   0) |>
        set_source("sel-labels", mc_lab)
    } else {
      sl  <- sel_labels()
      mc_lab$label[match(sl$ord_occ, mc_lab$ord_occ)] <- sl$label
      lit <- list("literal", as.list(rv$sel_occ))
      proxy |>
        set_paint_property("casts",    "circle-opacity", 0.3) |>
        set_paint_property("segments", "line-opacity",   0.3) |>
        set_filter("sel-casts", list("in", list("get", "ord_occ"), lit)) |>
        set_paint_property("sel-casts", "circle-opacity", 1) |>
        set_filter("sel-segments", list(
          "all",
          list("in", list("get", "occ_beg"), lit),
          list("in", list("get", "occ_end"), lit))) |>
        set_paint_property("sel-segments", "line-opacity", 1) |>
        set_source("sel-labels", mc_lab)
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # store -> table proxy
  observeEvent(rv$sel_occ, {
    if (identical(rv$sel_source, "table")) return()
    rows <- which(casts_tbl()$occ %in% rv$sel_occ)
    DT::dataTableProxy("tbl_casts") |> DT::selectRows(rows)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # store -> plot: output$plot_transect re-renders on rv$sel_occ (below)

  # --- reset selection — the button only appears once a selection is
  #     started (an anchor dropped on the map) or made, never on first view
  output$ui_reset_sel <- renderUI({
    if (length(rv$sel_occ) == 0 && is.null(rv$sel_anchor)) return(NULL)
    actionButton(
      "btn_reset_sel", "Reset selection",
      width = "100%", class = "btn-outline-secondary")
  })

  observeEvent(input$btn_reset_sel, {
    rv$sel_anchor <- NULL
    rv$sel_source <- "reset"
    rv$sel_occ    <- character(0)
  })

  output$txt_sel_count <- renderText({
    n <- length(rv$sel_occ)
    if (n == 0) return("No casts selected.")
    if (!is.null(rv$sel_anchor) && n == 1)
      return("1 cast anchored — click another on the map to complete the transect.")
    glue("{n} cast(s) selected.")
  })

  # === Table subtab ========================================================

  output$tbl_casts <- DT::renderDT(
    {
      d <- casts_tbl()
      req(nrow(d) > 0)
      d
    },
    selection = "multiple", rownames = FALSE, filter = "top",
    options = list(
      pageLength = 8, scrollX = TRUE, order = list(list(0, "asc"))))
  # tbl_casts drives the linked selection — keep it (and its DT proxy) live
  # even while the Values subtab is the one on screen
  outputOptions(output, "tbl_casts", suspendWhenHidden = FALSE)

  # measurements (ctd_thin) for the selected occupations + chosen variable,
  # filtered to the Max depth control
  sel_meas_data <- reactive({
    req(rv$cruise_key, input$sel_meas_type)
    occ <- rv$sel_occ
    if (length(occ) == 0) return(NULL)

    occ_casts <- rv$all_casts |> filter(ord_occ %in% occ)
    uuids <- unique(occ_casts$ctd_cast_uuid)
    if (length(uuids) == 0) return(NULL)

    d <- tbl(con, "ctd_thin") |>
      filter(
        cruise_key       == !!rv$cruise_key,
        measurement_type == !!input$sel_meas_type,
        ctd_cast_uuid %in% !!uuids,
        depth_m <= !!input$sl_max_depth) |>
      select(ctd_cast_uuid, depth_m, measurement_value,
             measurement_qual, retained_reason) |>
      collect()
    if (nrow(d) == 0) return(NULL)

    occ_xy <- occ_casts |>
      distinct(ctd_cast_uuid, .keep_all = TRUE) |>
      select(ctd_cast_uuid, ord_occ, datetime_utc, lat_dec, lon_dec)
    d <- d |> left_join(occ_xy, by = "ctd_cast_uuid")

    # cumulative transect distance, one value per occupation (ord_occ order)
    occ_pos <- d |>
      group_by(ord_occ) |>
      summarize(
        lon          = first(lon_dec),
        lat          = first(lat_dec),
        datetime_utc = first(datetime_utc),
        .groups      = "drop") |>
      arrange(ord_occ)
    occ_pos$dist_km <- if (nrow(occ_pos) > 1) {
      c(0, cumsum(geosphere::distHaversine(
        cbind(occ_pos$lon[-nrow(occ_pos)], occ_pos$lat[-nrow(occ_pos)]),
        cbind(occ_pos$lon[-1],             occ_pos$lat[-1])) / 1000))
    } else 0

    d |>
      left_join(occ_pos |> select(ord_occ, dist_km), by = "ord_occ") |>
      left_join(sel_labels(), by = "ord_occ")
  })

  output$txt_values_caption <- renderText({
    d  <- sel_meas_data()
    mt <- input$sel_meas_type
    if (is.null(d))
      glue("Values — select casts to list {mt} measurements ",
           "(≤ {input$sl_max_depth} m)")
    else
      glue("Values — {mt} ≤ {input$sl_max_depth} m: {nrow(d)} rows ",
           "across {length(unique(d$ord_occ))} selected cast(s)")
  })

  output$tbl_values <- DT::renderDT(
    {
      d <- sel_meas_data()
      req(!is.null(d))
      d |>
        transmute(
          label        = label,
          occ          = ord_occ,
          datetime_utc = datetime_utc,
          depth_m      = depth_m,
          value        = round(measurement_value, 4),
          qual         = measurement_qual,
          retained     = retained_reason) |>
        arrange(occ, depth_m)
    },
    selection = "none", rownames = FALSE, filter = "none",
    options = list(pageLength = 12, scrollX = TRUE))

  # === Plot subtab — transect of the selected occupations ==================

  # an empty plotly shown whenever there's nothing to draw — a clean
  # "select casts" message instead of a blank panel
  transect_placeholder <- function(msg) {
    # explicit empty scatter trace avoids plotly's "no trace type
    # specified" build warning
    plotly::plot_ly(
      x = numeric(0), y = numeric(0), type = "scatter", mode = "markers") |>
      plotly::layout(
        annotations = list(
          text = msg, showarrow = FALSE,
          xref = "paper", yref = "paper", x = 0.5, y = 0.5,
          font = list(color = "#888", size = 14)),
        xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
  }

  output$plot_transect <- renderPlotly({
    d  <- sel_meas_data()
    mt <- input$sel_meas_type

    if (is.null(d) || length(unique(d$ord_occ)) < 2)
      return(transect_placeholder(paste(
        "Select 2 or more casts (on the map or in the Casts tab)",
        "to draw a transect.")))

    meas_lab <- meas_types$label[meas_types$measurement_type == mt]
    if (length(meas_lab) == 0) meas_lab <- mt

    # bathymetry is always shown — one position per occupation
    occ_pos <- d |>
      group_by(ord_occ) |>
      summarize(
        lon     = first(lon_dec),
        lat     = first(lat_dec),
        dist_km = first(dist_km),
        .groups = "drop") |>
      arrange(dist_km)
    bathy <- get_transect_bathy(occ_pos$lon, occ_pos$lat, occ_pos$dist_km)

    p <- build_transect_plotly(
      meas_data  = d,
      bathy_data = bathy,
      meas_label = meas_lab,
      max_depth  = input$sl_max_depth,
      cruise_key = rv$cruise_key)
    if (is.null(p))
      return(transect_placeholder(
        "Too few measurements to interpolate a transect."))
    p
  })

  # === Cruise Stats subtab =================================================

  output$tbl_stats <- DT::renderDT(
    {
      req(rv$cruise_stats)
      rv$cruise_stats
    },
    selection = "none", rownames = FALSE,
    colnames  = c("Metric", "Value"),
    options   = list(dom = "t", pageLength = 25))

  stat_val <- function(metric) {
    s <- rv$cruise_stats
    if (is.null(s)) return("—")
    v <- s$value[s$metric == metric]
    if (length(v) == 0) "—" else as.character(v)
  }
  output$vb_casts    <- renderText(stat_val("Casts (station occupations)"))
  output$vb_selected <- renderText(as.character(length(rv$sel_occ)))
  output$vb_gap      <- renderText(stat_val("Median depth gap, ctd_thin (m)"))
}
