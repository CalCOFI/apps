server <- function(input, output, session) {

  # reactive values ----
  rv <- reactiveValues(
    cruise_key = NULL)

  # table ----
  output$tbl_cruises <- renderDT({
    d_tbl <- d_summary |>
      mutate(
        Map = glue(
          '<button class="btn btn-sm btn-primary" ',
          'onclick="Shiny.setInputValue(\'sel_cruise\', \'{cruise_key}\', ',
          '{{priority: \'event\'}})">',
          '<i class="bi bi-map"></i> Map</button>')) |>
      select(cruise_key, year, month, ship_name,
             ichthyo, bottle, `ctd-cast`, dic, Map)

    datatable(
      d_tbl,
      escape    = FALSE,
      rownames  = FALSE,
      selection = "single",
      options   = list(
        order      = list(list(1, "desc"), list(2, "desc")),
        pageLength = 25,
        dom        = "frtip",
        columnDefs = list(
          list(className = "dt-center", targets = 4:7),
          list(className = "dt-center", targets = 8))))
  })

  # map button click ----
  observeEvent(input$sel_cruise, {
    rv$cruise_key <- input$sel_cruise
    updateNavsetCardPill(session, "navset", selected = "Map")
  })

  # map title ----
  output$txt_map_title <- renderText({
    if (is.null(rv$cruise_key)) {
      "Select a cruise from the Table tab"
    } else {
      cruise_info <- d_summary |> filter(cruise_key == rv$cruise_key)
      if (nrow(cruise_info) > 0) {
        glue("Cruise {rv$cruise_key} - {cruise_info$ship_name[1]}")
      } else {
        glue("Cruise {rv$cruise_key}")
      }
    }
  })

  # render map ----
  output$map_cruise <- renderMaplibre({
    req(rv$cruise_key)
    ck <- rv$cruise_key

    withProgress(message = "Loading cruise data...", {

      # query root station events per dataset for this cruise ----
      # all datasets now share the unified `sample` event dimension; root
      # (station) events have parent_sample_key IS NULL and carry grid_key,
      # lat/long and datetime directly. map each release dataset_key back to the
      # short label the app uses downstream (colors, legend, tracks).
      setProgress(0.2, detail = "Loading sample events...")
      ds_labels <- c(
        swfsc_ichthyo      = "ichthyo",
        calcofi_bottle     = "bottle",
        `calcofi_ctd-cast` = "ctd-cast",
        calcofi_dic        = "dic")

      d_pts <- tryCatch(
        tbl(con, "sample") |>
          filter(
            cruise_key == !!ck,
            is.na(parent_sample_key),
            dataset_key %in% !!names(ds_labels)) |>
          select(sample_key, dataset_key, grid_key, longitude, latitude, datetime) |>
          collect(),
        error = function(e) tibble())

      pts_list <- list()
      if (nrow(d_pts) > 0) {
        d_pts <- d_pts |>
          mutate(
            dataset      = unname(ds_labels[dataset_key]),
            lon          = longitude,
            lat          = latitude,
            datetime_utc = datetime,
            # ctd-cast mints one root event per cast direction (…NNNd / …NNNu);
            # collapse to one point per station (prefer D) so the map matches the
            # table's ctd_cast count. other datasets are already one root/station.
            station_key  = ifelse(
              dataset == "ctd-cast", sub("[ud]$", "", sample_key), sample_key)) |>
          filter(!is.na(lon), !is.na(lat)) |>
          arrange(station_key, sample_key) |>
          distinct(station_key, .keep_all = TRUE) |>
          select(dataset, sample_key, grid_key, lon, lat, datetime_utc)
        pts_list <- split(d_pts, d_pts$dataset)
      }

      setProgress(0.8, detail = "Building map...")

      # combine all points
      all_pts <- bind_rows(pts_list) |>
        filter(!is.na(lon), !is.na(lat))

      if (nrow(all_pts) == 0) {
        showNotification("No spatial data found for this cruise", type = "warning")
        return(
          maplibre(style = carto_style("dark-matter")) |>
            add_navigation_control())
      }

      all_sf <- st_as_sf(all_pts, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

      # build map ----
      m <- maplibre(style = carto_style("dark-matter")) |>
        add_navigation_control() |>
        add_scale_control(position = "bottom-left") |>
        fit_bounds(all_sf, padding = 50)

      # grid polygons (from grid table)
      grid_sf <- tryCatch(
        calcofi4r::cc_read_sf(con, "grid"),
        error = function(e) NULL)

      if (!is.null(grid_sf) && nrow(grid_sf) > 0) {
        # filter to grids that contain data points — grid_key now travels with
        # each `sample` row, so no lookup against retired event tables is needed
        grid_keys_used <- all_pts |>
          filter(!is.na(grid_key)) |>
          pull(grid_key) |>
          unique()

        grid_show <- grid_sf |> filter(grid_key %in% grid_keys_used)

        if (nrow(grid_show) > 0) {
          m <- m |>
            add_fill_layer(
              id                 = "grid",
              source             = grid_show,
              fill_color         = "#ffffff",
              fill_opacity       = 0.08,
              fill_outline_color = "#ffffff50")
        }
      }

      # cruise track lines (connect sequential casts by datetime)
      for (ds in c("bottle", "ctd-cast")) {
        ds_pts <- pts_list[[ds]]
        if (!is.null(ds_pts) && nrow(ds_pts) > 1 && "datetime_utc" %in% names(ds_pts)) {
          ds_ordered <- ds_pts |> arrange(datetime_utc)
          segs <- compute_segments(ds_ordered)
          if (!is.null(segs) && nrow(segs) > 0) {
            m <- m |>
              add_line_layer(
                id           = glue("track-{ds}"),
                source       = segs,
                line_color   = dataset_colors[ds],
                line_width   = 1.5,
                line_opacity = 0.5)
          }
        }
      }

      # add points per dataset (layered so all are visible)
      for (ds in names(pts_list)) {
        ds_sf <- all_sf |> filter(dataset == ds)
        if (nrow(ds_sf) > 0) {
          m <- m |>
            add_circle_layer(
              id                  = glue("pts-{ds}"),
              source              = ds_sf,
              circle_color        = dataset_colors[ds],
              circle_radius       = 6,
              circle_opacity      = 0.85,
              circle_stroke_color = "#ffffff",
              circle_stroke_width = 1,
              tooltip             = "sample_key")
        }
      }

      # legend
      m <- m |>
        add_legend(
          "Datasets",
          type   = "categorical",
          values = names(pts_list),
          colors = unname(dataset_colors[names(pts_list)]),
          position     = "bottom-right",
          margin_right = 10)

      m
    })
  })
}
