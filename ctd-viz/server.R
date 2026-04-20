server <- function(input, output, session) {

  # reactive values ----
  rv <- reactiveValues(
    cruise_key      = NULL,
    all_casts       = NULL,   # data.frame: all casts (D and U)
    map_casts       = NULL,   # sf: deduplicated casts for map (one per station)
    cruise_segments = NULL,   # sf: segment lines between consecutive stations
    sel_begin_idx   = NULL,   # integer: begin cast index in map_casts
    sel_end_idx     = NULL,   # integer: end cast index in map_casts
    transect_casts  = NULL,   # sf: subset of map_casts in selection
    transect_plot   = NULL)   # plotly: rendered transect plot

  # load cruise ----
  observeEvent(input$btn_load_cruise, {
    req(input$sel_cruise)

    cruise_key_sel <- input$sel_cruise

    withProgress(message = "Loading cruise...", {

      # query all casts for this cruise
      d_all <- tbl(con, "ctd_cast") |>
        filter(cruise_key == !!cruise_key_sel) |>
        select(
          ctd_cast_uuid, cruise_key, cast_dir, ord_occ,
          datetime_utc, lat_dec, lon_dec, sta_key, line, sta) |>
        collect()

      setProgress(0.4, detail = "Processing casts...")

      # deduplicate: one point per station visit (prefer D over U)
      d_map <- d_all |>
        arrange(ord_occ, cast_dir) |>
        distinct(ord_occ, .keep_all = TRUE) |>
        arrange(ord_occ, datetime_utc) |>
        mutate(
          cast_idx = row_number(),
          tooltip  = paste0(
            "<b>Station:</b> ", sta_key, "<br>",
            "<b>Time:</b> ", format(datetime_utc, "%Y-%m-%d %H:%M"), "<br>",
            "<b>Line:</b> ", line, " Sta: ", sta))

      map_casts_sf <- st_as_sf(
        d_map,
        coords = c("lon_dec", "lat_dec"),
        crs    = 4326,
        remove = FALSE)

      setProgress(0.7, detail = "Computing segments...")

      segments_sf <- compute_segments(d_map)

      # store in reactive values
      rv$cruise_key      <- cruise_key_sel
      rv$all_casts       <- d_all
      rv$map_casts       <- map_casts_sf
      rv$cruise_segments <- segments_sf
      rv$sel_begin_idx   <- NULL
      rv$sel_end_idx     <- NULL
      rv$transect_casts  <- NULL
      rv$transect_plot   <- NULL

      # update time slider to cruise extent
      time_rng <- range(d_map$datetime_utc, na.rm = TRUE)
      updateSliderInput(session, "sl_time_range",
        min   = time_rng[1],
        max   = time_rng[2],
        value = time_rng)
    })
  })

  # render map ----
  output$map_cruise <- renderMaplibre({
    req(rv$map_casts)

    casts <- rv$map_casts
    segs  <- rv$cruise_segments

    m <- maplibre(style = carto_style("voyager")) |>
      add_navigation_control() |>
      add_scale_control(position = "bottom-left") |>
      fit_bounds(casts)

    # segment lines (base + highlight)
    if (!is.null(segs) && nrow(segs) > 0) {
      m <- m |>
        add_line_layer(
          id           = "segments",
          source       = segs,
          line_color   = "#888888",
          line_width   = 1.5,
          line_opacity = 0.6) |>
        add_line_layer(
          id           = "sel-segments",
          source       = segs,
          line_color   = "#ff6600",
          line_width   = 3,
          line_opacity = 0)
    }

    # cast points (base + highlight)
    m <- m |>
      add_circle_layer(
        id                  = "casts",
        source              = casts,
        circle_color        = "#0077cc",
        circle_radius       = 5,
        circle_opacity      = 0.8,
        circle_stroke_color = "white",
        circle_stroke_width = 1,
        tooltip             = "tooltip") |>
      add_circle_layer(
        id                  = "sel-casts",
        source              = casts,
        circle_color        = "#ff4444",
        circle_radius       = 7,
        circle_opacity      = 0,
        circle_stroke_color = "#ffffff",
        circle_stroke_width = 2)

    m
  })

  # map click -> transect selection ----
  observeEvent(input$map_cruise_feature_click, {
    req(rv$map_casts)

    click <- input$map_cruise_feature_click
    req(click$properties$ctd_cast_uuid)

    clicked_uuid <- click$properties$ctd_cast_uuid
    casts        <- rv$map_casts
    clicked_idx  <- which(casts$ctd_cast_uuid == clicked_uuid)
    if (length(clicked_idx) == 0) return()
    clicked_idx <- clicked_idx[1]

    if (is.null(rv$sel_begin_idx) || !is.null(rv$sel_end_idx)) {
      # first click (or restart after completed selection)
      rv$sel_begin_idx  <- clicked_idx
      rv$sel_end_idx    <- NULL
      rv$transect_casts <- NULL
    } else {
      # second click: complete selection
      idx1 <- min(rv$sel_begin_idx, clicked_idx)
      idx2 <- max(rv$sel_begin_idx, clicked_idx)
      rv$sel_begin_idx  <- idx1
      rv$sel_end_idx    <- idx2
      rv$transect_casts <- casts[idx1:idx2, ]

      # sync time slider
      sel_times <- rv$transect_casts$datetime_utc
      updateSliderInput(session, "sl_time_range",
        value = range(sel_times, na.rm = TRUE))
    }
  })

  # time slider -> transect selection ----
  observeEvent(input$sl_time_range, {
    req(rv$map_casts)

    casts    <- rv$map_casts
    time_rng <- input$sl_time_range
    in_range <- which(
      casts$datetime_utc >= time_rng[1] &
      casts$datetime_utc <= time_rng[2])

    if (length(in_range) < 2) return()

    new_begin <- min(in_range)
    new_end   <- max(in_range)

    # only update if actually changed (avoid circular triggers)
    if (!identical(rv$sel_begin_idx, new_begin) ||
        !identical(rv$sel_end_idx, new_end)) {
      rv$sel_begin_idx  <- new_begin
      rv$sel_end_idx    <- new_end
      rv$transect_casts <- casts[new_begin:new_end, ]
    }
  }, ignoreInit = TRUE)

  # highlight selection on map ----
  observe({
    begin_idx <- rv$sel_begin_idx
    req(begin_idx)
    end_idx <- rv$sel_end_idx

    proxy <- maplibre_proxy("map_cruise")

    if (is.null(end_idx)) {
      # begin only: highlight single cast
      proxy |>
        set_paint_property("casts", "circle-opacity", 0.4) |>
        set_filter("sel-casts",
          list("==", list("get", "cast_idx"), begin_idx)) |>
        set_paint_property("sel-casts", "circle-opacity", 1.0) |>
        set_paint_property("sel-segments", "line-opacity", 0)
    } else {
      # full selection: highlight range
      proxy |>
        set_paint_property("casts", "circle-opacity", 0.3) |>
        set_paint_property("segments", "line-opacity", 0.3) |>
        set_filter("sel-casts", list("all",
          list(">=", list("get", "cast_idx"), begin_idx),
          list("<=", list("get", "cast_idx"), end_idx))) |>
        set_paint_property("sel-casts", "circle-opacity", 1.0) |>
        set_filter("sel-segments", list("all",
          list(">=", list("get", "seg_idx"), begin_idx),
          list("<=", list("get", "seg_idx"), end_idx - 1L))) |>
        set_paint_property("sel-segments", "line-opacity", 1.0)
    }
  })

  # selection info ----
  output$txt_selection_info <- renderUI({
    if (is.null(rv$sel_begin_idx)) {
      p("Click two casts on map to select transect",
        class = "text-muted small")
    } else {
      casts    <- rv$map_casts
      begin_st <- casts$sta_key[rv$sel_begin_idx]

      if (is.null(rv$sel_end_idx)) {
        HTML(glue(
          "<small><b>Begin:</b> {begin_st}<br>",
          "<em>Click another cast for end point</em></small>"))
      } else {
        end_st <- casts$sta_key[rv$sel_end_idx]
        n      <- rv$sel_end_idx - rv$sel_begin_idx + 1
        HTML(glue(
          "<small><b>Begin:</b> {begin_st}<br>",
          "<b>End:</b> {end_st}<br>",
          "<b>Casts:</b> {n}</small>"))
      }
    }
  })

  # reset selection ----
  observeEvent(input$btn_reset_selection, {
    rv$sel_begin_idx  <- NULL
    rv$sel_end_idx    <- NULL
    rv$transect_casts <- NULL

    maplibre_proxy("map_cruise") |>
      set_paint_property("casts", "circle-opacity", 0.8) |>
      set_paint_property("segments", "line-opacity", 0.6) |>
      set_paint_property("sel-casts", "circle-opacity", 0) |>
      set_paint_property("sel-segments", "line-opacity", 0)

    if (!is.null(rv$map_casts)) {
      time_rng <- range(rv$map_casts$datetime_utc, na.rm = TRUE)
      updateSliderInput(session, "sl_time_range", value = time_rng)
    }
  })

  # plot transect ----
  observeEvent(input$btn_plot, {
    req(rv$transect_casts, rv$all_casts, rv$cruise_key)

    transect <- rv$transect_casts
    n_casts  <- nrow(transect)

    withProgress(message = "Building transect...", {

      # cumulative distance between consecutive casts
      if (n_casts > 1) {
        dists <- c(0, cumsum(
          geosphere::distHaversine(
            cbind(transect$lon_dec[-n_casts], transect$lat_dec[-n_casts]),
            cbind(transect$lon_dec[-1],       transect$lat_dec[-1])) / 1000))
      } else {
        dists <- 0
      }

      transect_dist <- st_drop_geometry(transect) |>
        mutate(dist_km = dists) |>
        select(ord_occ, dist_km)

      # find UUIDs for selected cast direction
      transect_ord_occs <- transect$ord_occ
      cast_dir_sel      <- input$sel_cast_dir
      meas_type_sel     <- input$sel_meas_type

      transect_uuids <- rv$all_casts |>
        filter(
          ord_occ  %in% transect_ord_occs,
          cast_dir == cast_dir_sel) |>
        pull(ctd_cast_uuid)

      if (length(transect_uuids) == 0) {
        showNotification(
          "No casts found for selected direction", type = "warning")
        return()
      }

      setProgress(0.3, detail = "Querying measurements...")

      # query ctd_measurement with partition pruning on cruise_key
      d_meas <- tbl(con, "ctd_measurement") |>
        filter(
          cruise_key       == !!rv$cruise_key,
          measurement_type == !!meas_type_sel) |>
        inner_join(
          tbl(con, "ctd_cast") |>
            filter(ctd_cast_uuid %in% !!transect_uuids) |>
            select(ctd_cast_uuid, ord_occ),
          by = "ctd_cast_uuid") |>
        select(depth_m, measurement_value, ord_occ) |>
        collect() |>
        left_join(transect_dist, by = "ord_occ")

      if (nrow(d_meas) == 0) {
        showNotification("No measurements found", type = "warning")
        return()
      }

      setProgress(0.5, detail = "Interpolating...")

      # measurement label
      meas_info  <- meas_types |> filter(measurement_type == meas_type_sel)
      meas_label <- paste0(meas_info$description, " (", meas_info$units, ")")

      # bathymetry (optional)
      bathy_data <- NULL
      if (input$chk_bathy) {
        setProgress(0.6, detail = "Fetching bathymetry...")
        bathy_data <- get_transect_bathy(transect$lon_dec, transect$lat_dec)
      }

      setProgress(0.8, detail = "Building plot...")

      p <- build_transect_plot(
        meas_data  = d_meas,
        bathy_data = bathy_data,
        meas_label = meas_label,
        max_depth  = input$sl_max_depth,
        interp_n   = input$num_interp_n)

      if (is.null(p)) {
        showNotification(
          "Too few data points for interpolation", type = "warning")
        return()
      }

      # add title and convert to plotly
      p <- p + labs(title = glue("{meas_label} \u2014 Cruise {rv$cruise_key}"))

      rv$transect_plot <- ggplotly(p) |>
        config(displaylogo = FALSE)
    })

    # switch to transect tab
    updateTabsetPanel(session, "tabs", selected = "Transect")
  })

  # render transect ----
  output$plot_transect <- renderPlotly({
    req(rv$transect_plot)
    rv$transect_plot
  })
}
