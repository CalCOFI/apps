# packages ----
librarian::shelf(
  bslib, bsicons, calcofi/calcofi4r,
  DBI, dbplyr, dplyr, duckdb,
  geosphere, ggplot2, glue, htmltools,
  lubridate, mapgl, marmap, MBA,
  plotly, purrr, sf, shiny, stringr,
  tidyr, viridis,
  quiet = T)

# database ----
# cc_get_db() handles httpfs setup and partitioned parquet (ctd_measurement, etc.)
con <- calcofi4r::cc_get_db()

# cruise choices ----
# derive date from ctd_cast.datetime_utc (cruise table is stale for newer cruises)
cruise_choices <- tbl(con, "ctd_cast") |>
  group_by(cruise_key, ship_key) |>
  summarize(
    date_min = min(datetime_utc, na.rm = TRUE),
    .groups  = "drop") |>
  inner_join(
    tbl(con, "ship") |> select(ship_key, ship_name),
    by = "ship_key") |>
  collect() |>
  mutate(
    date_ym = format(date_min, "%Y-%m")) |>
  arrange(desc(date_ym), ship_name) |>
  mutate(
    label = paste0(date_ym, " \u2014 ", ship_name, " (", cruise_key, ")"))

cruise_vec     <- setNames(cruise_choices$cruise_key, cruise_choices$label)
default_cruise <- cruise_choices$cruise_key[1]

# measurement types (CTD only, from small reference table) ----
meas_types <- tbl(con, "measurement_type") |>
  filter(`_source_table` == "ctd_raw") |>
  select(measurement_type, description, units) |>
  collect() |>
  mutate(
    label = paste0(description, " (", units, ")"))

meas_vec <- setNames(meas_types$measurement_type, meas_types$label)

# default measurement: temperature_1 if available
default_meas <- {
  temp_idx <- grep("^temperature_1$", meas_types$measurement_type)
  if (length(temp_idx) > 0) meas_types$measurement_type[temp_idx[1]]
  else meas_types$measurement_type[1]
}

# helpers ----

# compute segment lines between consecutive casts
compute_segments <- function(casts_df) {
  n <- nrow(casts_df)
  if (n < 2) return(NULL)

  casts_df |>
    mutate(
      uuid_beg     = lag(ctd_cast_uuid),
      lon_beg      = lag(lon_dec),
      lat_beg      = lag(lat_dec),
      datetime_beg = lag(datetime_utc)) |>
    filter(!is.na(lon_beg)) |>
    mutate(
      seg_idx = row_number(),
      m       = pmap(
        list(lon_beg, lat_beg, lon_dec, lat_dec),
        \(x1, y1, x2, y2) matrix(c(x1, y1, x2, y2), nrow = 2, byrow = T)),
      geom    = map(m, st_linestring)) |>
    select(-m) |>
    st_as_sf(sf_column_name = "geom", crs = 4326) |>
    mutate(
      length_km = as.numeric(st_length(geom)) / 1000)
}

# fetch bathymetry along a transect path
get_transect_bathy <- function(lons, lats, n_pts = 100) {
  lon_rng <- range(lons) + c(-0.5, 0.5)
  lat_rng <- range(lats) + c(-0.5, 0.5)

  bathy <- tryCatch(
    marmap::getNOAA.bathy(
      lon1 = lon_rng[1], lon2 = lon_rng[2],
      lat1 = lat_rng[1], lat2 = lat_rng[2],
      resolution = 1, keep = TRUE),
    error = function(e) {
      message("Bathymetry fetch failed: ", e$message)
      NULL
    })

  if (is.null(bathy)) return(NULL)

  pts_lon <- approx(seq_along(lons), lons, n = n_pts)$y
  pts_lat <- approx(seq_along(lats), lats, n = n_pts)$y
  depths  <- marmap::get.depth(bathy, x = pts_lon, y = pts_lat, locator = FALSE)

  dist_km <- c(0, cumsum(
    geosphere::distHaversine(
      cbind(pts_lon[-n_pts], pts_lat[-n_pts]),
      cbind(pts_lon[-1],     pts_lat[-1])) / 1000))

  tibble(
    dist_km       = dist_km,
    bathy_depth_m = -depths$depth)
}

# build ODV-style transect plot with MBA interpolation
build_transect_plot <- function(meas_data, bathy_data = NULL,
                                meas_label, max_depth = 500,
                                interp_n = 100) {
  xyz <- meas_data |>
    filter(!is.na(measurement_value)) |>
    select(x = dist_km, y = depth_m, z = measurement_value) |>
    as.data.frame()

  if (nrow(xyz) < 4) return(NULL)

  surf   <- MBA::mba.surf(xyz, no.X = interp_n, no.Y = interp_n,
                           extend = TRUE)$xyz.est
  d_grid <- expand.grid(dist_km = surf$x, depth_m = surf$y) |>
    mutate(value = as.vector(surf$z)) |>
    filter(depth_m >= 0, depth_m <= max_depth)

  station_pos <- meas_data |> distinct(dist_km)

  p <- ggplot() +
    geom_raster(
      data = d_grid,
      aes(dist_km, depth_m, fill = value),
      interpolate = TRUE) +
    scale_fill_viridis_c(name = meas_label) +
    geom_contour(
      data = d_grid,
      aes(dist_km, depth_m, z = value),
      color = "white", alpha = 0.4, linewidth = 0.3) +
    geom_rug(
      data = station_pos,
      aes(x = dist_km), sides = "t")

  if (!is.null(bathy_data)) {
    bathy_poly <- bind_rows(
      bathy_data |> mutate(bathy_depth_m = pmin(bathy_depth_m, max_depth)),
      tibble(
        dist_km       = c(max(bathy_data$dist_km), min(bathy_data$dist_km)),
        bathy_depth_m = c(max_depth, max_depth)))

    p <- p + geom_polygon(
      data = bathy_poly,
      aes(dist_km, bathy_depth_m),
      fill = "gray40", color = "black", linewidth = 0.5)
  }

  p +
    scale_y_reverse(limits = c(max_depth, 0), expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(x = "Distance (km)", y = "Depth (m)") +
    theme_minimal() +
    theme(panel.grid = element_blank())
}

# cleanup ----
onStop(function() {
  dbDisconnect(con, shutdown = TRUE)
})
