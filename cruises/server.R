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

      # query sites per dataset for this cruise ----
      pts_list <- list()

      # ichthyo sites
      setProgress(0.1, detail = "Ichthyo sites...")
      d_ich <- tryCatch(
        tbl(con, "site") |>
          filter(cruise_key == !!ck) |>
          select(site_key, latitude, longitude, line, station) |>
          collect(),
        error = function(e) tibble())
      if (nrow(d_ich) > 0) {
        pts_list$ichthyo <- d_ich |>
          mutate(dataset = "ichthyo", lon = longitude, lat = latitude) |>
          select(dataset, site_key, lon, lat)
      }

      # bottle casts
      setProgress(0.3, detail = "Bottle casts...")
      d_bot <- tryCatch(
        tbl(con, "casts") |>
          filter(cruise_key == !!ck) |>
          select(site_key, lon_dec, lat_dec, datetime_utc) |>
          collect(),
        error = function(e) tibble())
      if (nrow(d_bot) > 0) {
        pts_list$bottle <- d_bot |>
          mutate(dataset = "bottle", lon = lon_dec, lat = lat_dec) |>
          select(dataset, site_key, lon, lat, datetime_utc)
      }

      # ctd casts
      setProgress(0.5, detail = "CTD casts...")
      d_ctd_pts <- tryCatch(
        tbl(con, "ctd_cast") |>
          filter(cruise_key == !!ck) |>
          select(site_key, lon_dec, lat_dec, datetime_utc, cast_dir) |>
          collect() |>
          # deduplicate: one point per station (prefer D over U)
          arrange(site_key, cast_dir) |>
          distinct(site_key, .keep_all = TRUE),
        error = function(e) tibble())
      if (nrow(d_ctd_pts) > 0) {
        pts_list$`ctd-cast` <- d_ctd_pts |>
          mutate(dataset = "ctd-cast", lon = lon_dec, lat = lat_dec) |>
          select(dataset, site_key, lon, lat, datetime_utc)
      }

      # dic samples
      setProgress(0.6, detail = "DIC samples...")
      d_dic_pts <- tryCatch(
        tbl(con, "dic_sample") |>
          inner_join(
            tbl(con, "casts") |>
              filter(cruise_key == !!ck) |>
              select(cast_id, site_key, lon_dec, lat_dec, datetime_utc),
            by = "cast_id") |>
          select(site_key, lon_dec, lat_dec, datetime_utc) |>
          distinct(site_key, .keep_all = TRUE) |>
          collect(),
        error = function(e) tibble())
      if (nrow(d_dic_pts) > 0) {
        pts_list$dic <- d_dic_pts |>
          mutate(dataset = "dic", lon = lon_dec, lat = lat_dec) |>
          select(dataset, site_key, lon, lat, datetime_utc)
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
        # filter to grids that contain data points
        grid_keys_used <- all_pts |>
          inner_join(
            DBI::dbGetQuery(con, glue(
              "SELECT DISTINCT site_key, grid_key FROM site
               UNION
               SELECT DISTINCT site_key, grid_key FROM casts")) |>
              filter(!is.na(grid_key)),
            by = "site_key") |>
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
              tooltip             = "site_key")
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
