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
#
# cast_seq (numeric ord_occ) is used end-to-end for visible labels: map markers,
# plot annotations, the Casts table, and the Measurements table.
#
# cruises and casts are auto-filtered to those carrying the chosen measurement:
# the cruise dropdown updates on meas change, and the loader pulls only the
# ctd_cast rows whose ctd_cast_uuid appears in ctd_thin for (cruise, meas).
# this keeps the map + tables consistent with the plot.

server <- function(input, output, session) {

  rv <- reactiveValues(
    cruise_key = NULL,           # loaded cruise
    all_casts  = NULL,           # df: every ctd_cast row for the cruise (per scan)
    map_casts  = NULL,           # sf: one row per ord_occ (station occupation)
    sel_occ    = character(0),   # selected ord_occ — THE selection store
    sel_anchor = NULL,           # in-progress transect start (map picker)
    sel_source = NULL)           # "map" | "table" | "reset"

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
        cast_seq    = cast_seq,
        ord_occ     = ord_occ,
        line, sta,
        dtime_pt    = format(dtime_pt, "%Y-%m-%d %H:%M:%S"),
        max_depth_m = max_depth_m,
        n_depths    = n_depths) |>
      arrange(ord_occ)
  })

  # transect labels (cast_seq numbers) for the current selection, in cruise-
  # track order — one source of truth shared by the map markers and the plot
  sel_labels <- reactive({
    lab_map <- assign_occ_labels(rv$sel_occ)
    tibble(ord_occ = names(lab_map), label = unname(lab_map))
  })

  # --- cruise dropdown auto-filter ----------------------------------------
  # when the user picks a different measurement, narrow the cruise list to
  # only those cruises that actually carry that variable in ctd_thin. keep
  # the current cruise if it's still valid; otherwise jump to the newest
  # cruise that is.
  observeEvent(input$sel_meas_type, {
    mt <- input$sel_meas_type
    req(mt)
    valid_keys <- meas_to_cruises$cruise_key[meas_to_cruises$measurement_type == mt]
    filtered   <- cruise_choices |>
      filter(cruise_key %in% valid_keys)
    vec        <- setNames(filtered$cruise_key, filtered$label)
    current    <- input$sel_cruise %||% default_cruise
    new_sel    <- if (current %in% valid_keys) current else filtered$cruise_key[1]
    updateSelectInput(session, "sel_cruise",
                      choices = vec, selected = new_sel)
  })

  # --- combined cruise + measurement loader -------------------------------
  # fires when either input changes. pulls the filtered ctd_cast subset (only
  # casts that have data for the chosen measurement), then sets up rv$* for
  # the map / table / plot. selection is reset on any (cruise|meas) change.
  observeEvent(list(input$sel_cruise, input$sel_meas_type), {
    ck <- input$sel_cruise
    mt <- input$sel_meas_type
    req(ck, mt)

    withProgress(message = "Loading cruise…", {
      setProgress(0.15, detail = "casts with measurement…")
      uuids_with_meas <- dbGetQuery(con, glue("
        SELECT DISTINCT ctd_cast_uuid FROM ctd_thin
        WHERE cruise_key = '{ck}' AND measurement_type = '{mt}'"))$ctd_cast_uuid

      if (length(uuids_with_meas) == 0) {
        # defensive: dropdown filter should prevent this; clear state cleanly
        rv$cruise_key <- ck
        rv$all_casts  <- NULL
        rv$map_casts  <- NULL
        rv$sel_anchor <- NULL
        rv$sel_source <- "reset"
        rv$sel_occ    <- character(0)
        return()
      }

      d_all <- tbl(con, "ctd_cast") |>
        filter(
          cruise_key    == !!ck,
          ctd_cast_uuid %in% !!uuids_with_meas) |>
        select(ctd_cast_uuid, ord_occ, cast_seq, cast_dir,
               datetime_utc, dtime_pt,
               lat_dec, lon_dec, site_key, line, sta) |>
        collect()
      setProgress(0.5, detail = "stations…")

      # one map point per station occupation
      d_map <- d_all |>
        arrange(ord_occ, cast_dir, datetime_utc) |>
        distinct(ord_occ, .keep_all = TRUE)

      # per-occupation depth summary -> rich hover tooltip
      setProgress(0.75, detail = "depths…")
      occ_dep <- occ_depth_summary(con, ck)
      d_map <- d_map |>
        left_join(occ_dep, by = "ord_occ") |>
        mutate(
          n_depths = ifelse(is.na(n_depths), 0L, n_depths),
          label    = "",          # per-selection cast_seq, filled via set_source
          tooltip  = paste0(
            "<b>Cast sequence*:</b> ",
            ifelse(is.na(cast_seq), "—", cast_seq), "<br>",
            "<b>Line / Sta:</b> ", line, " / ", sta, "<br>",
            "<b>Date (Pacific):</b> ",
            format(dtime_pt, "%Y-%m-%d %H:%M:%S"), "<br>",
            "<b>Lon, Lat:</b> ", round(lon_dec, 4), ", ",
            round(lat_dec, 4), "<br>",
            "<b>Max depth:</b> ",
            ifelse(is.na(max_depth_m), "—", paste0(max_depth_m, " m")),
            " &middot; <b>", n_depths, "</b> retained depths"))
      map_casts_sf <- st_as_sf(
        d_map, coords = c("lon_dec", "lat_dec"), crs = 4326, remove = FALSE)

      rv$cruise_key <- ck
      rv$all_casts  <- d_all
      rv$map_casts  <- map_casts_sf
      rv$sel_anchor <- NULL
      rv$sel_source <- "reset"
      rv$sel_occ    <- character(0)
    })
  }, ignoreNULL = FALSE)

  # --- render map ---------------------------------------------------------
  output$map_cruise <- renderMaplibre({
    req(rv$map_casts)
    mc   <- rv$map_casts
    segs <- cruise_segments()

    # single-point sf gives a degenerate bbox -> maplibre zooms to world.
    # pad the bbox manually for the 1-cast case so the view stays regional.
    # fit_bounds() only auto-converts when its input inherits from "sf"
    # (sfc / numeric inputs pass through), so hand it the c(xmin, ymin,
    # xmax, ymax) numeric directly rather than an sfc polygon.
    fit_target <- if (nrow(mc) == 1) {
      lon <- mc$lon_dec[1]
      lat <- mc$lat_dec[1]
      pad <- 0.5   # degrees ~ ~55 km, enough for a regional context
      c(lon - pad, lat - pad, lon + pad, lat + pad)
    } else mc

    # initial basemap follows the dark/light toggle's current value; subsequent
    # toggles are handled by the set_style observer below (preserves layers,
    # avoids a full re-render). isolate so the map isn't re-rendered when the
    # user flips the theme.
    init_style <- isolate(
      if (identical(input$dark_toggle, "light")) "voyager" else "dark-matter")

    m <- maplibre(style = carto_style(init_style)) |>
      add_navigation_control() |>
      add_fullscreen_control() |>
      add_scale_control(position = "bottom-left") |>
      fit_bounds(fit_target) |>
      # GEBCO seafloor depth, drawn beneath the segments + casts. starts
      # visible or hidden per the gear toggle; the observer below flips
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
      add_symbol_layer(            # cast_seq labels on the selected casts
        id = "sel-labels", source = mc,
        text_field = get_column("label"),   # data-driven, not the literal "label"
        text_size = 13, text_color = "#7a0046",
        text_halo_color = "#ffffff", text_halo_width = 1.6,
        text_offset = c(0, -1.4), text_allow_overlap = TRUE)
  })

  # toggle the bathymetry raster live, without re-rendering the whole map
  observeEvent(input$chk_bathy, {
    maplibre_proxy("map_cruise") |>
      set_layout_property(
        "bathy", "visibility",
        if (isTRUE(input$chk_bathy)) "visible" else "none")
  }, ignoreInit = TRUE)

  # dark / light toggle -> basemap swap. set_style with preserve_layers = TRUE
  # (default) keeps the cast circles, segment lines, bathy raster, and labels
  # we added on top of the basemap.
  observeEvent(input$dark_toggle, {
    style <- if (identical(input$dark_toggle, "dark")) "dark-matter" else "voyager"
    maplibre_proxy("map_cruise") |>
      set_style(carto_style(style))
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
    sel <- casts_tbl()$ord_occ[input$tbl_casts_rows_selected]
    sel <- sel[!is.na(sel)]
    if (setequal(sel, rv$sel_occ)) return()
    rv$sel_anchor <- NULL          # a table pick ends any in-progress map range
    rv$sel_source <- "table"
    rv$sel_occ    <- sel
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # === selection updaters: the store -> each view (skip the writing view) ==

  # store -> map proxy. runs for every source (including "map"): clicking a
  # maplibre feature does not auto-highlight it, so the proxy must (re)apply
  # the pink styling here regardless of who wrote the store. proxy ops don't
  # fire click events, so this can't loop.
  observeEvent(rv$sel_occ, {
    proxy  <- maplibre_proxy("map_cruise")
    mc_lab <- rv$map_casts
    if (is.null(mc_lab)) return()
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
    if (is.null(rv$map_casts)) return()
    rows <- which(casts_tbl()$ord_occ %in% rv$sel_occ)
    DT::dataTableProxy("tbl_casts") |> DT::selectRows(rows)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # store -> plot: output$plot_transect re-renders on rv$sel_occ (below)

  # --- selection bar — alternates between click-to-select instructions
  #     (no selection / right after Reset) and [Reset · count · Download]
  #     once the user starts a selection (anchored or completed).
  output$ui_sel_bar <- renderUI({
    if (length(rv$sel_occ) == 0 && is.null(rv$sel_anchor)) {
      return(div(
        class = "small text-muted",
        "Click rows to select; the map and plot follow ",
        "(selecting on the map updates the table in turn)."))
    }
    div(
      class = "d-flex align-items-center gap-2 flex-wrap",
      actionButton(
        "btn_reset_sel", "Reset selection",
        class = "btn-outline-secondary btn-sm"),
      span(
        class = "small text-muted",
        textOutput("txt_sel_count", inline = TRUE)),
      div(
        class = "ms-auto",
        uiOutput("ui_download", inline = TRUE)))
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

  # === Casts subtab ========================================================

  output$tbl_casts <- DT::renderDT(
    {
      d <- casts_tbl()
      req(nrow(d) > 0)
      d |> select(-ord_occ)        # ord_occ is the join key; cast_seq is shown
    },
    selection = "multiple", rownames = FALSE, filter = "top",
    options = list(
      # 't' = table; 'l' = length picker, 'i' = info, 'p' = pagination —
      # all at the bottom. dropping 'f' kills the global Search (the per-
      # column filter row from filter = "top" is the search now).
      dom        = "tlip",
      pageLength = 8, scrollX = TRUE, order = list(list(0, "asc"))))
  # tbl_casts drives the linked selection — keep it (and its DT proxy) live
  # even while the Measurements subtab is the one on screen
  outputOptions(output, "tbl_casts", suspendWhenHidden = FALSE)

  # measurements (ctd_thin) for the selected occupations + chosen variable.
  # the depth cap (input$sl_max_depth) is applied downstream in the plot /
  # table renders, not here — so this reactive holds the full data range and
  # the auto-set observer below can read data_max without depending on the
  # slider (which would loop).
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
        ctd_cast_uuid %in% !!uuids) |>
      select(ctd_cast_uuid, depth_m, measurement_value, measurement_qual) |>
      collect()
    if (nrow(d) == 0) return(NULL)

    occ_xy <- occ_casts |>
      distinct(ctd_cast_uuid, .keep_all = TRUE) |>
      select(ctd_cast_uuid, ord_occ, cast_seq, dtime_pt, lat_dec, lon_dec)
    d <- d |> left_join(occ_xy, by = "ctd_cast_uuid")

    # cumulative transect distance, one value per occupation (ord_occ order)
    occ_pos <- d |>
      group_by(ord_occ) |>
      summarize(
        lon      = first(lon_dec),
        lat      = first(lat_dec),
        dtime_pt = first(dtime_pt),
        .groups  = "drop") |>
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

  # auto-set the Max-depth slider on every selection change: snap it to the
  # deepest sample + small pad (rounded to the 50 m step), so the plot fills
  # with data by default. depends only on the selection (not the slider),
  # so it can't loop. user can still drag deeper (exposes bathymetry) or
  # shallower (clips the data).
  observe({
    d <- sel_meas_data()
    if (is.null(d) || nrow(d) == 0) return()
    data_max <- max(d$depth_m, na.rm = TRUE)
    data_pad <- max(20, data_max * 0.05)
    new_val  <- min(5000, max(50, ceiling((data_max + data_pad) / 50) * 50))
    current  <- isolate(input$sl_max_depth)
    if (is.null(current) || !isTRUE(current == new_val))
      updateSliderInput(session, "sl_max_depth", value = new_val)
  })

  output$txt_values_caption <- renderText({
    d  <- sel_meas_data()
    mt <- input$sel_meas_type
    if (is.null(d)) {
      glue("Measurements — select casts to list {mt} measurements")
    } else {
      d_show <- d |> filter(depth_m <= !!input$sl_max_depth)
      glue("Measurements — {mt}: {nrow(d_show)} rows ",
           "across {length(unique(d_show$ord_occ))} selected cast(s)",
           if (input$sl_max_depth < max(d$depth_m, na.rm = TRUE))
             glue(" (capped at {input$sl_max_depth} m)")
           else
             "")
    }
  })

  output$tbl_values <- DT::renderDT(
    {
      d <- sel_meas_data()
      req(!is.null(d))
      d |>
        filter(depth_m <= !!input$sl_max_depth) |>
        transmute(
          cast_seq = cast_seq,
          dtime_pt = format(dtime_pt, "%Y-%m-%d %H:%M:%S"),
          depth_m  = depth_m,
          value    = round(measurement_value, 4),
          qual     = measurement_qual) |>
        arrange(cast_seq, depth_m)
    },
    selection = "none", rownames = FALSE, filter = "none",
    options = list(pageLength = 12, scrollX = TRUE))

  # === Download ============================================================
  # joined measurements + cast metadata for the current selection. enabled
  # only when at least one cast is selected. lives in the Casts tab header.
  output$ui_download <- renderUI({
    if (length(rv$sel_occ) == 0) return(NULL)
    downloadButton(
      "dl_data", "Download CSV",
      class = "btn-primary btn-sm")
  })

  output$dl_data <- downloadHandler(
    filename = function() {
      glue("ctd-viz_{rv$cruise_key}_{input$sel_meas_type}_",
           "{format(Sys.time(), '%Y%m%d-%H%M%S')}.csv")
    },
    content = function(file) {
      d <- sel_meas_data()
      if (is.null(d) || nrow(d) == 0) {
        readr::write_csv(tibble(), file)
        return()
      }
      mt <- input$sel_meas_type
      out <- d |>
        filter(depth_m <= !!input$sl_max_depth) |>
        transmute(
          cruise_key       = rv$cruise_key,
          cast_seq         = cast_seq,
          ord_occ          = ord_occ,
          dtime_pt         = dtime_pt,
          lat_dec          = lat_dec,
          lon_dec          = lon_dec,
          dist_km          = dist_km,
          depth_m          = depth_m,
          measurement_type = mt,
          value            = measurement_value,
          qual             = measurement_qual) |>
        arrange(cast_seq, depth_m)
      readr::write_csv(out, file)
    })

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

    if (is.null(d) || nrow(d) == 0)
      return(transect_placeholder(
        "Select a cast (on the map or in the Casts tab) to draw a profile."))

    meas_lab <- meas_types$label[meas_types$measurement_type == mt]
    if (length(meas_lab) == 0) meas_lab <- mt

    n_casts <- length(unique(d$ord_occ))

    # 1 cast -> profile plot (value on x, depth on y)
    if (n_casts == 1) {
      pt          <- d[1, ]
      bathy_depth <- tryCatch(
        max(terra::extract(
          bathy_rast, cbind(pt$lon_dec, pt$lat_dec),
          method = "bilinear")[, 1], 0, na.rm = TRUE),
        error = function(e) NA_real_)
      p <- build_profile_plotly(
        meas_data   = d,
        meas_label  = meas_lab,
        max_depth   = input$sl_max_depth,
        cruise_key  = rv$cruise_key,
        bathy_depth = bathy_depth)
      if (is.null(p))
        return(transect_placeholder(
          "Too few measurements to draw a profile."))
      return(p)
    }

    # ≥ 2 casts -> ODV-style transect (bathymetry as a clipped silhouette)
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

  # === Tour ================================================================
  # auto-shown the first time a visitor lands (gated client-side via
  # localStorage); the help icon in the header re-opens it anytime. wrapped
  # in tryCatch so a conductor/JS hiccup doesn't take the session down.
  start_tour <- function() {
    tryCatch(tour$init()$start(),
             error = function(e) message("tour failed to start: ", conditionMessage(e)))
  }

  observeEvent(input$tour_seen, once = TRUE, ignoreNULL = TRUE, {
    if (isTRUE(input$tour_seen)) return()
    start_tour()
    session$sendCustomMessage("ctdviz_tour_seen", TRUE)
  })

  observeEvent(input$btn_help, {
    start_tour()
  })
}
