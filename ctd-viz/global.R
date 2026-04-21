# packages ----
librarian::shelf(
  bslib, bsicons, calcofi/calcofi4r,
  DBI, dbplyr, dplyr, duckdb,
  geosphere, ggExtra, ggplot2, glue, htmltools,
  lubridate, mapgl, MBA,
  purrr, sf, shiny, stringr, terra,
  tidyr, viridis,
  quiet = T)

# paths ----
app_dir <- dirname(normalizePath(
  sys.frame(1)$ofile %||% "global.R", mustWork = FALSE))
if (!dir.exists(app_dir)) app_dir <- getwd()

db_file <- if (dir.exists("/share/data")) {
  "/share/data/ctd-viz/ctd-viz.duckdb"
} else {
  path.expand("~/_big/calcofi.org/ctd-viz/ctd-viz.duckdb")
}
stopifnot(
  "prepped database not found; run `Rscript prep_db.R` first" =
    file.exists(db_file))

bathy_tif <- file.path(app_dir, "data/gebco_calcofi.tif")
stopifnot(
  "bathymetry raster not found; run `Rscript prep_bathy.R` first" =
    file.exists(bathy_tif))

# database ----
con <- dbConnect(
  duckdb::duckdb(
    dbdir     = db_file,
    read_only = TRUE,
    config    = list(autoload_known_extensions = "true")))
dbExecute(con, "INSTALL spatial; LOAD spatial;")

# bathymetry (loaded once at app start) ----
bathy_rast <- terra::rast(bathy_tif)

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

# extract bathymetry at each cast position (not interpolated between).
# a CalCOFI cruise zigzags across the grid; interpolating lon/lat between
# consecutive casts often crosses land (e.g. Channel Islands), producing
# nonsense "bathymetry" along the transect. using cast locations only keeps
# depth firmly in-water.
get_transect_bathy <- function(lons, lats, dists_km) {
  stopifnot(
    length(lons) == length(lats),
    length(lons) == length(dists_km))
  if (length(lons) < 2) return(NULL)

  # local gebco_calcofi.tif uses positive-down convention (ocean depth > 0,
  # land near 0). clamp land/negatives to 0 so the polygon stays at the surface
  # over dry points (CalCOFI casts shouldn't be on land, but be defensive).
  depths <- terra::extract(
    bathy_rast,
    cbind(lons, lats),
    method = "bilinear")[, 1]

  tibble(
    dist_km       = dists_km,
    bathy_depth_m = pmax(depths, 0))
}

# build ODV-style transect plot with MBA interpolation, sampling dots,
# and marginal density histograms. Returns a patchwork object.
build_transect_plot <- function(meas_data, bathy_data = NULL,
                                meas_label, max_depth = 500,
                                interp_n = 100,
                                cruise_key = NULL,
                                cast_info = NULL) {
  xyz <- meas_data |>
    filter(!is.na(measurement_value)) |>
    select(x = dist_km, y = depth_m, z = measurement_value) |>
    as.data.frame()

  if (nrow(xyz) < 4) return(NULL)

  # MBA interpolation (ODV-style smooth field)
  surf <- MBA::mba.surf(
    xyz, no.X = interp_n, no.Y = interp_n, extend = TRUE)$xyz.est
  d_grid <- expand.grid(dist_km = surf$x, depth_m = surf$y) |>
    mutate(value = as.vector(surf$z)) |>
    filter(depth_m >= 0, depth_m <= max_depth)

  x_rng <- range(xyz$x)
  y_rng <- c(0, max_depth)

  # main section plot ----
  p_main <- ggplot() +
    geom_raster(
      data        = d_grid,
      aes(dist_km, depth_m, fill = value),
      interpolate = TRUE) +
    scale_fill_viridis_c(name = meas_label) +
    geom_contour(
      data      = d_grid,
      aes(dist_km, depth_m, z = value),
      color     = "white",
      alpha     = 0.4,
      linewidth = 0.3) +
    # sampling dots: show where measurements actually occurred
    geom_point(
      data   = xyz,
      aes(x, y),
      colour = "black",
      alpha  = 0.15,
      size   = 0.25,
      stroke = 0)

  if (!is.null(bathy_data)) {
    bathy_poly <- bind_rows(
      bathy_data |> mutate(bathy_depth_m = pmin(bathy_depth_m, max_depth)),
      tibble(
        dist_km       = c(max(bathy_data$dist_km), min(bathy_data$dist_km)),
        bathy_depth_m = c(max_depth, max_depth)))

    p_main <- p_main +
      geom_polygon(
        data      = bathy_poly,
        aes(dist_km, bathy_depth_m),
        fill      = "grey70",
        colour    = "black",
        linewidth = 0.4)
  }

  title_lab <- if (!is.null(cruise_key)) {
    glue("{meas_label} \u2014 Cruise {cruise_key}")
  } else meas_label

  # build a date axis on top if cast_info provided (~6 evenly spaced labels)
  sec_x <- ggplot2::waiver()
  if (!is.null(cast_info) && nrow(cast_info) >= 2) {
    n_breaks <- min(6, nrow(cast_info))
    br_idx   <- round(seq(1, nrow(cast_info), length.out = n_breaks))
    sec_x    <- ggplot2::dup_axis(
      breaks = cast_info$dist_km[br_idx],
      labels = format(cast_info$datetime_utc[br_idx], "%m-%d"),
      name   = NULL)
  }

  p_main <- p_main +
    scale_y_reverse(limits = rev(y_rng), expand = c(0, 0)) +
    scale_x_continuous(
      limits   = x_rng,
      expand   = c(0, 0),
      sec.axis = sec_x) +
    guides(fill = guide_colourbar(
      barwidth = unit(8, "cm"), barheight = unit(0.3, "cm"))) +
    labs(x = "Distance (km)", y = "Depth (m)", title = title_lab) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid         = element_blank(),
      legend.position    = "bottom",
      legend.title       = element_text(size = 9),
      legend.text        = element_text(size = 8),
      legend.margin      = margin(0, 0, 0, 0),
      legend.box.margin  = margin(-2, 0, 0, 0))

  # marginal histograms via ggExtra::ggMarginal — purpose-built to produce a
  # narrow (~5%) strip along x and y without stealing panel width. requires
  # a point geom inside the main plot, which we already have via geom_point.
  # `size = 20` means "main plot is 20× the marginal strip" (i.e. ~5% strip).
  ggMarginal(
    p_main,
    type   = "histogram",
    margins = "both",
    size   = 20,
    xparams = list(bins = 60, fill = "grey30", colour = NA),
    yparams = list(bins = 60, fill = "grey30", colour = NA))
}

# cleanup ----
onStop(function() {
  dbDisconnect(con, shutdown = TRUE)
})
