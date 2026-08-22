# ctd-viz server — linked selection across the map + table; the plot follows.
#
# the selection unit is a station occupation (ord_occ within the loaded cruise).
# a single store, rv$sel_occ, is the source of truth, and the map is its only
# writer — a transect picker: click a station to anchor, click another to
# select every station between them along the cruise track (ord_occ order).
# the map + table updaters push it back to their views and the transect plot
# re-renders from it. loops are broken by a setequal() no-op guard.
#
# the Casts table is a read-only view of the selection, not a second writer:
# it narrows to just the selected stations once one exists (casts_tbl_shown()),
# and clicking a row there just glows that cast on the map rather than
# changing rv$sel_occ. an earlier version let table clicks write the store
# too, but a table redraw (from the map-filter narrowing the list) could
# itself report a spurious "nothing selected" click and cancel an
# in-progress map anchor before a second map click completed the range.
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
    cruise_key  = NULL,          # loaded cruise
    all_casts   = NULL,          # df: every ctd_cast row for the cruise (per scan)
    map_casts   = NULL,          # sf: one row per ord_occ (station occupation)
    sel_occ     = character(0),  # selected ord_occ — THE selection store
    sel_anchor  = NULL,          # in-progress transect start (map picker)
    sel_source  = NULL,          # "map" | "reset" | "restore"
    pending_sel = NULL)          # cast selection awaiting restore (url bookmark)

  # === URL bookmarking =====================================================
  # shareable links (store = "url", enabled in global.R). the cruise,
  # measurement, active tab, depth cap and bathymetry toggle are plain inputs
  # and round-trip automatically. the cast selection lives in rv$sel_occ (not
  # an input): onBookmark writes it into the link's _values_, and it is read
  # back on restore from the query string (below) — onRestore does not fire for
  # global.R-enabled bookmarking on this Shiny build, but input restore does.
  #
  # exclude the noisy map / table / pane event inputs — restoring them would
  # re-fire the selection writers with stale, pre-load row indices (and bloat
  # the link); the cast selection is restored via sel_occ instead.
  setBookmarkExclude(c(
    "._bookmark_", "bookmark_search", "tour_seen", "dark_toggle",
    "btn_help", "btn_start_walkthrough", "btn_tour_close", "btn_reopen_welcome",
    "btn_reset_sel", "btn_select_all", "btn_settings",
    "dl_data", "dl_casts", "dl_data_all", "dl_casts_all", "param_pick",
    "btn_back_to_params", "cruise_pick",
    "btn_feedback", "btn_feedback_submit",
    "fb_working", "fb_improve", "fb_broken", "fb_email",
    "map_cruise_bbox", "map_cruise_feature_click", "map_cruise_click",
    "map_cruise_bounds", "map_cruise_center", "map_cruise_zoom",
    "tbl_casts_columns_selected", "tbl_casts_cells_selected",
    "tbl_casts_rows_selected", "tbl_casts_row_last_clicked",
    "tbl_casts_rows_current", "tbl_casts_rows_all", "tbl_casts_state",
    "tbl_casts_search", "tbl_casts_search_columns", "tbl_casts_cell_clicked",
    "tbl_values_rows_current", "tbl_values_rows_all", "tbl_values_state",
    "tbl_values_search", "tbl_values_cell_clicked"))

  # save the cast selection into the bookmark link
  onBookmark(function(state) {
    state$values$sel_occ <- rv$sel_occ
  })

  # restore the cast selection: ui.R stashes the page's initial query string
  # (before Shiny strips it) and sends it as input$bookmark_search on connect.
  # parse sel_occ out of it and stage it for the applier below.
  observeEvent(input$bookmark_search, once = TRUE, {
    q   <- parseQueryString(input$bookmark_search)
    occ <- tryCatch(jsonlite::fromJSON(q[["sel_occ"]]), error = function(e) NULL)
    if (length(occ) > 0)
      rv$pending_sel <- as.character(occ)
  })

  # apply the pending restored selection once the cruise's stations have loaded
  # (the loader resets sel_occ, so this must run after it). reacts to whichever
  # of the two arrives last. no-op in a normal session (pending_sel stays NULL).
  observe({
    occ <- rv$pending_sel
    mc  <- rv$map_casts
    if (is.null(occ) || is.null(mc)) return()
    rv$pending_sel <- NULL
    valid <- mc$ord_occ[as.character(mc$ord_occ) %in% occ]
    if (length(valid) > 0) {
      rv$sel_source <- "restore"
      rv$sel_occ    <- valid
    }
  })

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

  # what the Casts table actually shows. narrowed to just the current
  # selection when that selection came from the map — clicking a transect on
  # the map is a "show me those stations" action, so the table should follow
  # it rather than stay on the full list with the pick just highlighted
  # underneath. table-driven selection (clicking rows yourself) does NOT
  # narrow the table — that would remove the very rows you'd click next to
  # extend a multi-select. Reset (or loading a different cruise) clears
  # rv$sel_source back off "map", which brings the full list back.
  casts_tbl_shown <- reactive({
    d <- casts_tbl()
    if (identical(rv$sel_source, "map") && length(rv$sel_occ) > 0) {
      d <- d |> filter(ord_occ %in% rv$sel_occ)
    }
    d
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
    } else {
      # pad the multi-station bbox ~10% on each side so the initial view
      # backs off slightly from a tight fit — as.numeric() strips the
      # xmin/ymin/xmax/ymax names st_bbox() attaches; a NAMED vector here
      # previously serialized to a JSON object instead of a plain array,
      # which made fit_bounds() fail silently and fall back to a whole-
      # world view. plain unnamed numbers avoid that entirely.
      bb    <- as.numeric(sf::st_bbox(mc))  # xmin, ymin, xmax, ymax
      pad_x <- (bb[3] - bb[1]) * 0.10
      pad_y <- (bb[4] - bb[2]) * 0.10
      c(bb[1] - pad_x, bb[2] - pad_y, bb[3] + pad_x, bb[4] + pad_y)
    }

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
      add_circle_layer(             # "look here" glow for the last Casts
        id = "row-glow", source = mc,  # row clicked — hidden (opacity 0)
        circle_color = "#ffd43b", circle_radius = 14, circle_opacity = 0,
        circle_stroke_width = 0) |>    # until that click sets its filter
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
      if (setequal(rv$sel_occ, occ)) {
        # re-clicking the only currently-selected station — unclick it,
        # rather than restarting a new anchor on the same spot.
        rv$sel_source <- "map"
        rv$sel_occ    <- character(0)
      } else {
        rv$sel_anchor <- occ
        rv$sel_source <- "map"
        rv$sel_occ    <- occ
      }
    } else if (identical(rv$sel_anchor, occ)) {
      # re-clicking the just-anchored station — cancel the anchor instead
      # of "completing" a zero-length range on the same point.
      rv$sel_anchor <- NULL
      rv$sel_source <- "map"
      rv$sel_occ    <- character(0)
    } else {
      i  <- range(match(c(rv$sel_anchor, occ), occ_all))
      rng <- occ_all[i[1]:i[2]]
      rv$sel_anchor <- NULL                          # range complete
      if (!setequal(rng, rv$sel_occ)) {
        rv$sel_source <- "map"
        rv$sel_occ    <- rng
      }
    }
    # jump to Casts — but only from a tab where a station click would
    # otherwise have no visible result (Cruises / Parameters / Plot).
    # Readings already follows the selection just as directly as Casts
    # does, so forcing a switch away from it just to land on Casts is a
    # step backwards, not a convenience.
    if (!isTRUE(input$subtabs %in% c("Casts", "Readings")))
      nav_select("subtabs", "Casts", session = session)
  }, ignoreInit = TRUE)

  # (2) table row click -> glow that one cast on the map, from wherever the
  #     view already is (no pan/zoom). NOT a selection write — a table click
  #     used to feed straight into rv$sel_occ (the same store the map's
  #     transect picker writes), which meant clicking a row mid-anchor
  #     silently cancelled whatever map range was in progress, and a full
  #     table redraw (e.g. right after the map-filter below narrows the
  #     list) could itself report a spurious "nothing selected" click that
  #     reset the anchor before a second map click had a chance to complete
  #     the range. the map is now the only thing that writes rv$sel_occ; a
  #     table row is just a fast way to spot a station on it. reads
  #     casts_tbl_shown() — whatever's actually on screen, which is the
  #     map-filtered list once a map selection is active.
  observeEvent(input$tbl_casts_row_last_clicked, {
    occ <- casts_tbl_shown()$ord_occ[input$tbl_casts_row_last_clicked]
    req(occ)
    maplibre_proxy("map_cruise") |>
      set_filter("row-glow", list("==", list("get", "ord_occ"), occ)) |>
      set_paint_property("row-glow", "circle-opacity", 0.55)
  })

  # === selection updaters: the store -> each view (skip the writing view) ==

  # store -> map proxy. runs for every source (including "map"): clicking a
  # maplibre feature does not auto-highlight it, so the proxy must (re)apply
  # the pink styling here regardless of who wrote the store. proxy ops don't
  # fire click events, so this can't loop.
  # paint the current selection's pink styling onto the map. factored out so
  # both the selection observer and the map-load observer below can call it.
  paint_map_selection <- function() {
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
  }

  # re-style on every selection change
  observeEvent(rv$sel_occ, paint_map_selection(),
               ignoreNULL = FALSE, ignoreInit = TRUE)

  # re-apply once the map has (re)loaded. a restored or programmatic selection
  # is set before the map's first client render finishes, so the observer above
  # would style a map that isn't ready yet and the pink transect would be lost.
  # the map emits its bounds once loaded (and on each move) — use that as the
  # "ready" signal to paint the current selection in.
  observeEvent(input$map_cruise_bbox, paint_map_selection())

  # store -> plot: output$plot_transect re-renders on rv$sel_occ (below)

  # --- selection bar — alternates between click-to-select instructions
  #     (no selection / right after Reset) and [Reset · count · Download]
  # persistent link back to Parameters — Casts is where param_pick sends
  # you, so this closes the loop: Parameters -> pick -> Casts -> Change
  # parameter -> back to Parameters, without hunting for the tab by hand.
  # one combined line rather than two stacked bars (parameter + selection
  # state used to each get their own full-width row, which read as cluttered
  # for what's really one contextual header).
  observeEvent(input$btn_back_to_params, {
    nav_select("subtabs", "Parameters", session = session)
  })

  output$ui_sel_bar <- renderUI({
    mt    <- input$sel_meas_type
    pname <- meas_types$param_name[meas_types$measurement_type == mt]
    if (length(pname) == 0) pname <- mt
    has_sel <- length(rv$sel_occ) > 0 || !is.null(rv$sel_anchor)

    div(
      class = "d-flex align-items-center gap-2 flex-wrap small",
      tags$button(
        class   = "btn btn-sm ctdviz-param-chip",
        onclick = "Shiny.setInputValue('btn_back_to_params', Math.random(), {priority: 'event'})",
        bsicons::bs_icon("chevron-left", class = "me-1"), pname),
      tags$span(class = "ctdviz-divider"),
      # always available, selected or not — the map's own picker only ever
      # gets you a transect (anchor + one more click), so this is the fast
      # path to "just give me every cast in this cruise" for either viewing
      # the full Casts list or downloading/plotting all of it at once.
      actionButton(
        "btn_select_all", "Select all",
        class = "ctdviz-btn-reset btn-sm"),
      if (has_sel) {
        tagList(
          actionButton(
            "btn_reset_sel", "Reset selection",
            class = "ctdviz-btn-reset btn-sm"),
          span(
            class = "text-muted",
            textOutput("txt_sel_count", inline = TRUE)))
      } else {
        span(
          class = "text-muted",
          "Click stations on the map to select a transect.")
      },
      div(
        class = "ms-auto",
        uiOutput("ui_download", inline = TRUE)))
  })

  observeEvent(input$btn_select_all, {
    req(rv$map_casts)
    rv$sel_anchor <- NULL
    rv$sel_source <- "map"
    rv$sel_occ    <- sort(rv$map_casts$ord_occ)
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

  # === Cruises subtab =======================================================
  # recent-by-default list — same clicking-a-row-drives-the-dropdown pattern
  # as Parameters. the full cruise list (1949-present) is too long to show
  # flat the way Parameters does, so this shows the most recent
  # CRUISE_LIST_DEFAULT_N; the Cruise dropdown up top already has its own
  # built-in search for reaching further back, so this tab doesn't duplicate
  # that with a second search box.
  CRUISE_LIST_DEFAULT_N <- 20
  output$ui_cruise_list <- renderUI({
    d <- utils::head(cruise_choices, CRUISE_LIST_DEFAULT_N)

    rows <- lapply(seq_len(nrow(d)), function(i) {
      ck     <- d$cruise_key[i]
      active <- isTRUE(input$sel_cruise == ck)
      tags$div(
        class   = paste(
          "ctdviz-param-row", if (active) "ctdviz-param-row-active"),
        onclick = sprintf(
          "Shiny.setInputValue('cruise_pick', '%s', {priority: 'event'})", ck),
        tags$span(class = "ctdviz-param-name", d$label[i]))
    })
    div(class = "ctdviz-param-list", rows)
  })

  observeEvent(input$cruise_pick, {
    updateSelectInput(session, "sel_cruise", selected = input$cruise_pick)
  })

  # === Parameters subtab ====================================================
  # flat, always-visible list of every available measurement — the starter
  # pane (see ui.R). Clicking a row is the same action as using the
  # Measurement dropdown up top; this just drives that dropdown instead of
  # keeping its own separate state, so the two stay in sync no matter which
  # one someone uses. It also jumps straight to Casts (see the param_pick
  # observer below) — Casts is where the download button lives, so picking a
  # parameter takes you directly toward getting data instead of leaving you
  # on this list to find your own way to the next step.
  param_order <- order(
    param_priority_rank(meas_types$param_name), meas_types$param_name)
  output$ui_param_list <- renderUI({
    rows <- lapply(param_order, function(i) {
      mt     <- meas_types$measurement_type[i]
      icon   <- meas_types$icon[i]
      pname  <- meas_types$param_name[i]
      units  <- meas_types$units[i]
      active <- isTRUE(input$sel_meas_type == mt)
      tags$div(
        class   = paste(
          "ctdviz-param-row", if (active) "ctdviz-param-row-active"),
        onclick = sprintf(
          "Shiny.setInputValue('param_pick', '%s', {priority: 'event'})", mt),
        tags$span(class = "ctdviz-param-icon", HTML(icon)),
        tags$span(class = "ctdviz-param-name", pname),
        if (nzchar(units))
          tags$span(class = "ctdviz-param-unit", units))
    })
    div(class = "ctdviz-param-list", rows)
  })

  observeEvent(input$param_pick, {
    updateSelectizeInput(session, "sel_meas_type", selected = input$param_pick)
    nav_select("subtabs", "Casts", session = session)
  })

  # === Casts subtab ========================================================

  output$tbl_casts <- DT::renderDT(
    {
      d <- casts_tbl_shown()
      req(nrow(d) > 0)
      d |> select(-ord_occ)        # ord_occ is the join key; cast_seq is shown
    },
    # single, not multiple — a row click no longer builds a selection (see
    # the row_last_clicked handler above), just points the map at one cast,
    # so there's nothing to accumulate across rows.
    selection = "single", rownames = FALSE, filter = "top",
    colnames = c(
      "Cast" = "cast_seq", "Line" = "line", "Station" = "sta",
      "Date & time (local)" = "dtime_pt", "Max depth (m)" = "max_depth_m",
      "# depths recorded" = "n_depths"),
    class   = "cell-border stripe ctdviz-tbl",
    options = list(
      # 't' = table; 'l' = length picker, 'i' = info, 'p' = pagination —
      # all at the bottom. dropping 'f' kills the global Search (the per-
      # column filter row from filter = "top" is the search now).
      # autoWidth off — see the same option on tbl_values below for why;
      # matters even more here since this table (unlike tbl_values) is kept
      # rendering while genuinely hidden (suspendWhenHidden = FALSE, just
      # below), which is exactly the situation that throws off scrollX's
      # cached column widths.
      autoWidth      = FALSE,
      dom            = "tlip",
      pageLength     = 25, lengthMenu = c(10, 25, 50, 100),
      scrollX        = TRUE, order = list(list(0, "asc"))))
  # keep it (and click events) live even while the Measurements subtab is
  # the one on screen, and so it can redraw when the map filter changes it
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
      glue("Measurements — {mt}: ",
           "{length(unique(d_show$ord_occ))} selected cast(s)",
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
    colnames = c(
      "Cast" = "cast_seq", "Date & time (local)" = "dtime_pt",
      "Depth (m)" = "depth_m", "Value" = "value", "Quality flag" = "qual"),
    class   = "cell-border stripe ctdviz-tbl",
    options = list(
      # dom without 'f' drops the global search box — it matched every
      # column's rendered text at once (a depth of 10, a value containing
      # "10", a cast numbered 10, a timestamp with "10" in it, all mixed
      # together with no way to tell which one matched), which wasn't a
      # useful filter for this table. matches the Casts tab, which dropped
      # it for the same reason.
      # autoWidth off: with scrollX, DataTables measures + fixes each
      # column's width once at init, in a separate header element from the
      # body (that's how scrollX works) — if that first measurement happens
      # before the Readings tab is ever shown (or just doesn't match later,
      # differently-sized content), the two drift out of sync and headers
      # stop lining up with their columns. autoWidth: FALSE sizes columns
      # from actual cell content on every draw instead of a cached guess.
      autoWidth  = FALSE,
      dom        = "tlip",
      pageLength = 25, lengthMenu = c(10, 25, 50, 100), scrollX = TRUE))

  # === Download ============================================================
  # joined measurements + cast metadata. lives in both the Casts and Readings
  # tab headers — genuinely different exports now, not just different
  # styling on the same download: dl_casts (Casts tab) exports the
  # cast-level metadata table (cast_seq/line/station/date/max depth/#
  # depths — what tbl_casts actually shows), dl_data (Readings tab) exports
  # the per-depth measurement rows. Both used to point at dl_data alone, so
  # the Casts button was silently exporting reading-level data instead of
  # the cast list it sits next to — two separate output ids fixes that, not
  # just the visual distinction between them.
  #
  # each is a small dropdown rather than a single button: "selected" (the
  # rows currently picked — the original, selection-only behavior) and "all"
  # (every cast in the current cruise, regardless of what's selected). the
  # "selected" entry disables itself with nothing picked rather than hiding
  # the whole control, since "all" stays available either way.
  dl_dropdown <- function(id_sel, id_all, class, label_sel, label_all) {
    has_sel <- length(rv$sel_occ) > 0
    div(
      class = "dropdown d-inline-block",
      tags$button(
        class            = paste("btn dropdown-toggle", class),
        type             = "button",
        `data-bs-toggle` = "dropdown",
        `aria-expanded`  = "false",
        bsicons::bs_icon("download", class = "me-1"), "Download"),
      tags$ul(
        class = "dropdown-menu dropdown-menu-end",
        tags$li(
          if (has_sel) {
            downloadLink(id_sel, label_sel, class = "dropdown-item")
          } else {
            tags$span(class = "dropdown-item disabled", label_sel)
          }),
        tags$li(downloadLink(id_all, label_all, class = "dropdown-item"))))
  }
  output$ui_download <- renderUI(
    dl_dropdown("dl_casts", "dl_casts_all", "btn-primary btn-sm",
                "Selected casts", "All casts in cruise"))
  output$ui_download_readings <- renderUI(
    dl_dropdown("dl_data", "dl_data_all", "btn-primary btn-sm",
                "Selected readings", "All readings in cruise"))

  output$dl_casts <- downloadHandler(
    filename = function() {
      glue("ctd-viz_{rv$cruise_key}_casts_",
           "{format(Sys.time(), '%Y%m%d-%H%M%S')}.csv")
    },
    content = function(file) {
      d <- casts_tbl()
      if (is.null(d) || nrow(d) == 0 || length(rv$sel_occ) == 0) {
        readr::write_csv(tibble(), file)
        return()
      }
      out <- d |>
        filter(ord_occ %in% rv$sel_occ) |>
        transmute(
          cruise_key  = rv$cruise_key,
          cast_seq    = cast_seq,
          line        = line,
          sta         = sta,
          dtime_pt    = dtime_pt,
          max_depth_m = max_depth_m,
          n_depths    = n_depths) |>
        arrange(cast_seq)
      readr::write_csv(out, file)
    })

  # "all casts in cruise" — casts_tbl() is already every cast for the
  # current cruise (map_casts, unfiltered by selection), so this just skips
  # the ord_occ %in% rv$sel_occ step above rather than needing its own query.
  output$dl_casts_all <- downloadHandler(
    filename = function() {
      glue("ctd-viz_{rv$cruise_key}_casts_all_",
           "{format(Sys.time(), '%Y%m%d-%H%M%S')}.csv")
    },
    content = function(file) {
      d <- casts_tbl()
      if (is.null(d) || nrow(d) == 0) {
        readr::write_csv(tibble(), file)
        return()
      }
      out <- d |>
        transmute(
          cruise_key  = rv$cruise_key,
          cast_seq    = cast_seq,
          line        = line,
          sta         = sta,
          dtime_pt    = dtime_pt,
          max_depth_m = max_depth_m,
          n_depths    = n_depths) |>
        arrange(cast_seq)
      readr::write_csv(out, file)
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

  # "all readings in cruise" — same shape as sel_meas_data() but sourced
  # from every cast in the cruise (rv$all_casts) instead of just the
  # selected transect, so there's no meaningful dist_km (that's cumulative
  # distance *along the selected transect*, undefined without one).
  all_meas_data <- reactive({
    req(rv$cruise_key, input$sel_meas_type, rv$all_casts)
    uuids <- unique(rv$all_casts$ctd_cast_uuid)
    if (length(uuids) == 0) return(NULL)

    d <- tbl(con, "ctd_thin") |>
      filter(
        cruise_key       == !!rv$cruise_key,
        measurement_type == !!input$sel_meas_type,
        ctd_cast_uuid %in% !!uuids) |>
      select(ctd_cast_uuid, depth_m, measurement_value, measurement_qual) |>
      collect()
    if (nrow(d) == 0) return(NULL)

    occ_xy <- rv$all_casts |>
      distinct(ctd_cast_uuid, .keep_all = TRUE) |>
      select(ctd_cast_uuid, ord_occ, cast_seq, dtime_pt, lat_dec, lon_dec)
    d |> left_join(occ_xy, by = "ctd_cast_uuid")
  })

  output$dl_data_all <- downloadHandler(
    filename = function() {
      glue("ctd-viz_{rv$cruise_key}_{input$sel_meas_type}_all_",
           "{format(Sys.time(), '%Y%m%d-%H%M%S')}.csv")
    },
    content = function(file) {
      d <- all_meas_data()
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
        meas_data    = d,
        meas_label   = meas_lab,
        max_depth    = input$sl_max_depth,
        cruise_label = cruise_label_for(rv$cruise_key),
        bathy_depth  = bathy_depth)
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
      meas_data    = d,
      bathy_data   = bathy,
      meas_label   = meas_lab,
      max_depth    = input$sl_max_depth,
      cruise_label = cruise_label_for(rv$cruise_key))
    if (is.null(p))
      return(transect_placeholder(
        "Too few measurements to interpolate a transect."))
    p
  })

  # === Tour ================================================================
  # two separate tours (both defined in global.R): tour_welcome is the
  # one-time intro, auto-shown the first time a visitor lands (gated
  # client-side via localStorage) with its own "Start walkthrough" button;
  # tour_walkthrough is the 6-step guided tour, which only ever starts when
  # that button is clicked or the help icon is used — never automatically on
  # its own. both wrapped in tryCatch so a conductor/JS hiccup doesn't take
  # the session down.
  # both return TRUE/FALSE for whether the tour actually started, rather
  # than swallowing the error silently — the localStorage "seen" stamp
  # below only gets written on a real success, so a conductor/JS hiccup
  # doesn't permanently suppress the welcome popup for that browser.
  start_welcome <- function() {
    tryCatch({tour_welcome$init()$start(); TRUE},
             error = function(e) {
               message("welcome tour failed to start: ", conditionMessage(e))
               FALSE
             })
  }
  start_walkthrough <- function() {
    tryCatch({tour_walkthrough$init()$start(); TRUE},
             error = function(e) {
               message("walkthrough tour failed to start: ", conditionMessage(e))
               FALSE
             })
  }
  # closing a tour from the x button (see ui.R) round-trips through here
  # rather than being handled purely client-side — cancel() called directly
  # on the JS Tour object (Shepherd.activeTour.cancel()) and a synthetic
  # Escape keydown were both tried first and neither actually closed the
  # popup, so this drives it the same way starting a tour already works:
  # through the R6 object. tries both tours, since the x doesn't know which
  # one is currently open; whichever isn't running just no-ops or errors
  # harmlessly, caught below.
  cancel_tour <- function(tour) {
    tryCatch(tour$cancel(), error = function(e) NULL)
  }

  observeEvent(input$tour_seen, once = TRUE, ignoreNULL = TRUE, {
    if (isTRUE(input$tour_seen)) return()
    if (start_welcome()) session$sendCustomMessage("ctdviz_tour_seen", TRUE)
  })

  # fired by the "Start" button embedded in tour_welcome's step (see
  # global.R) — a plain Shiny.setInputValue() call, wired up client-side in
  # ui.R (not an onclick on the button itself; Shepherd strips those from
  # step HTML), but the effect is the same as any other actionButton.
  observeEvent(input$btn_start_walkthrough, {
    cancel_tour(tour_welcome)
    start_walkthrough()
  })

  # fired by the x close control on every step of both tours (see ui.R).
  observeEvent(input$btn_tour_close, {
    cancel_tour(tour_welcome)
    cancel_tour(tour_walkthrough)
  })

  # "CTD Casts" in the header (see ui.R) re-opens the welcome popup — the
  # logo next to it already links out to calcofi.io, so this is the way
  # back to the intro once the localStorage "seen" stamp has suppressed its
  # automatic first-visit appearance.
  observeEvent(input$btn_reopen_welcome, {
    cancel_tour(tour_walkthrough)
    start_welcome()
  })

  # re-opens the walkthrough directly (not the welcome blurb) — a returning
  # visitor clicking "?" almost always wants the how-to-use-this steps, not
  # the one-time intro.
  observeEvent(input$btn_help, {
    start_walkthrough()
  })

  # === Feedback =============================================================
  # ported from CalCOFI/db-viz-station's feedback modal, but posted
  # server-side (httr::POST) rather than a client-side no-cors fetch — Shiny
  # already has a modal primitive (showModal/modalDialog), so this reuses
  # that instead of hand-rolling a second modal-backdrop system in raw HTML/
  # JS/CSS on top of what bslib/Bootstrap already provides here.
  observeEvent(input$btn_feedback, {
    showModal(modalDialog(
      title   = "Send feedback",
      easyClose = TRUE,
      textAreaInput(
        "fb_working", "What's working well?", width = "100%",
        placeholder = "e.g. the cruise/measurement dropdowns, the map selection"),
      textAreaInput(
        "fb_improve", "What needs improvement?", width = "100%",
        placeholder = "e.g. hard to find a specific cast, download is confusing"),
      textAreaInput(
        "fb_broken", "Found something broken? (optional)", width = "100%",
        placeholder = "Describe the bug or confusing behavior"),
      textInput(
        "fb_email", "Email (optional)", width = "100%",
        placeholder = "name@example.com"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_feedback_submit", "Submit", class = "btn-primary"))))
  })

  observeEvent(input$btn_feedback_submit, {
    updateActionButton(session, "btn_feedback_submit", label = "Sending…")
    sent_ok <- tryCatch({
      resp <- httr::POST(
        url    = FEEDBACK_ENDPOINT,
        body   = setNames(
          list(input$fb_working, input$fb_improve, input$fb_broken, input$fb_email),
          unlist(FEEDBACK_ENTRIES)),
        encode = "form")
      # a real POST (unlike the static site's client-side no-cors fetch) gets
      # an actual status back, so failure can be detected instead of assumed
      # sent — Google Forms returns 200 on a successful submission.
      httr::stop_for_status(resp)
      TRUE
    }, error = function(e) {
      message("feedback submit failed: ", conditionMessage(e))
      FALSE
    })

    if (isTRUE(sent_ok)) {
      removeModal()
      showNotification("Thanks — your feedback was sent.", type = "message")
    } else {
      showNotification(
        "Something went wrong sending that — check your connection and try again.",
        type = "error")
    }
  })
}
