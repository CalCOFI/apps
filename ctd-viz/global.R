# packages ----
librarian::shelf(
  bslib, bsicons, calcofi/calcofi4r,
  DBI, dbplyr, dplyr, DT, duckdb,
  geosphere, ggplot2, glue, htmltools,
  lubridate, mapgl, MBA, plotly,
  purrr, sf, shiny, shinyjqui, stringr, terra,
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
  "bathymetry raster not found; run `Rscript prep_db.R` first" =
    file.exists(bathy_tif))

# database ----
con <- dbConnect(
  duckdb::duckdb(
    dbdir     = db_file,
    read_only = TRUE,
    config    = list(autoload_known_extensions = "true")))
dbExecute(con, "INSTALL spatial; LOAD spatial;")

# ctd_thin is the headline CTD table (adaptively thinned). confirm it's present
# and probe for the optional retained_reason column (drives a stats metric).
stopifnot(
  "ctd_thin not in database; rebuild with prep_db.R against v2026.05.14+" =
    "ctd_thin" %in% dbListTables(con))
thin_flds       <- dbListFields(con, "ctd_thin")
thin_has_reason <- "retained_reason" %in% thin_flds

# bathymetry (loaded once at app start) ----
# bathy_rast: full-res positive-down depth (m), land = 0 — used to sample
# seafloor depth at cast positions for the transect plot (get_transect_bathy).
# bathy_rast_map: a coarser, land-masked copy for the map raster layer (NA
# land -> transparent; aggregated 3x, ample for the regional overview).
bathy_rast     <- terra::rast(bathy_tif)
bathy_rast_map <- terra::aggregate(
  terra::ifel(bathy_rast <= 0, NA, bathy_rast),
  fact = 3, fun = "mean", na.rm = TRUE)
# shallow -> light, deep -> dark blue
bathy_pal <- grDevices::colorRampPalette(
  c("#d6e8f5", "#3b7bb3", "#0a2f5c"))(255)

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
    label = paste0(date_ym, " — ", ship_name, " (", cruise_key, ")"))

cruise_vec     <- setNames(cruise_choices$cruise_key, cruise_choices$label)
default_cruise <- cruise_choices$cruise_key[1]

# measurement types — only those actually present in ctd_thin (the canonical
# set), joined to measurement_type for human-readable labels + units
thin_types <- dbGetQuery(
  con, "SELECT DISTINCT measurement_type FROM ctd_thin")$measurement_type

meas_types <- tbl(con, "measurement_type") |>
  select(measurement_type, description, units) |>
  collect() |>
  filter(measurement_type %in% thin_types) |>
  mutate(label = paste0(description, " (", units, ")")) |>
  arrange(label)

meas_vec <- setNames(meas_types$measurement_type, meas_types$label)

# default measurement: temperature_ave if available, else first
default_meas <- if ("temperature_ave" %in% meas_types$measurement_type) {
  "temperature_ave"
} else {
  meas_types$measurement_type[1]
}

# helpers ----

# segment lines between consecutive station occupations. carries the start/end
# ord_occ (the selection unit) so the map can light up a segment when both
# endpoints are selected.
compute_segments <- function(casts_df) {
  n <- nrow(casts_df)
  if (n < 2) return(NULL)

  casts_df |>
    arrange(ord_occ) |>
    mutate(
      occ_beg = lag(ord_occ),
      occ_end = ord_occ,
      lon_beg = lag(lon_dec),
      lat_beg = lag(lat_dec)) |>
    filter(!is.na(occ_beg)) |>
    mutate(
      seg_idx = row_number(),
      m       = pmap(
        list(lon_beg, lat_beg, lon_dec, lat_dec),
        \(x1, y1, x2, y2) matrix(c(x1, y1, x2, y2), nrow = 2, byrow = T)),
      geom    = map(m, st_linestring)) |>
    select(occ_beg, occ_end, seg_idx, geom) |>
    st_as_sf(sf_column_name = "geom", crs = 4326)
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

# assign short transect labels (A, B, C, … then 1, 2, 3 … beyond 26) to a set
# of station occupations, in cruise-track (ord_occ) order. one source of truth
# so the map markers and the plot x-axis agree.
assign_occ_labels <- function(ord_occ) {
  o <- sort(unique(ord_occ[!is.na(ord_occ)]))
  if (length(o) == 0) return(setNames(character(0), character(0)))
  labs <- if (length(o) <= 26) LETTERS[seq_along(o)]
          else as.character(seq_along(o))
  setNames(labs, o)
}

# build an ODV-style transect plot (MBA-interpolated field, labelled white
# contours, observation points, bathymetry, lettered station markers) as an
# interactive plotly. display-only — the map + table drive the selection;
# ord_occ rides along as customdata only to tag the point trace for the hover
# wiring below.
build_transect_plotly <- function(meas_data, bathy_data = NULL,
                                  meas_label, max_depth = 500,
                                  interp_n = 100,
                                  cruise_key = NULL) {
  # station labels — supplied by the caller for map/plot consistency, or
  # derived here if absent (e.g. a standalone call)
  if (!"label" %in% names(meas_data)) {
    lab_map <- assign_occ_labels(meas_data$ord_occ)
    meas_data$label <- unname(lab_map[meas_data$ord_occ])
  }
  # the colour-bar shows units only — the variable name is already in the
  # plot title, so a short legend title leaves the panel more width
  units_lab <- sub("^.*\\(([^)]*)\\)\\s*$", "\\1", meas_label)

  xyz <- meas_data |>
    filter(!is.na(measurement_value)) |>
    mutate(
      hover = paste0(
        "<b>Station ", label, "</b><br>",
        "Depth: ", round(depth_m, 1), " m<br>",
        meas_label, ": ", round(measurement_value, 3))) |>
    select(x = dist_km, y = depth_m, z = measurement_value,
           ord_occ, label, hover) |>
    as.data.frame()

  if (nrow(xyz) < 4) return(NULL)

  # MBA interpolation (ODV-style smooth field)
  surf <- MBA::mba.surf(
    xyz[, c("x", "y", "z")], no.X = interp_n, no.Y = interp_n,
    extend = TRUE)$xyz.est
  d_grid <- expand.grid(dist_km = surf$x, depth_m = surf$y) |>
    mutate(value = as.vector(surf$z)) |>
    filter(depth_m >= 0, depth_m <= max_depth)

  # contour breaks shared by the lines and their value labels. one label per
  # level — placed on its longest contour piece, nearest the panel centre —
  # extracted via a throwaway ggplot_build (no clean public API for this).
  ctr_breaks <- pretty(range(d_grid$value, na.rm = TRUE), n = 7)
  ctr_lab <- tryCatch(
    ggplot_build(
      ggplot(d_grid, aes(dist_km, depth_m, z = value)) +
        geom_contour(breaks = ctr_breaks))$data[[1]],
    error = function(e) NULL)
  if (!is.null(ctr_lab) && nrow(ctr_lab) > 0) {
    x_mid   <- mean(range(d_grid$dist_km))
    ctr_lab <- ctr_lab |>
      group_by(level, piece) |>
      mutate(.piece_n = n()) |>
      group_by(level) |>
      filter(.piece_n == max(.piece_n)) |>
      slice_min(abs(x - x_mid), n = 1, with_ties = FALSE) |>
      ungroup()
  } else ctr_lab <- NULL

  x_rng <- range(xyz$x)
  # y-axis fits the data and the seafloor, capped at the Max depth control —
  # avoids a sliver of data over a wedge of empty water
  y_max <- min(
    max_depth,
    max(xyz$y,
        if (!is.null(bathy_data)) bathy_data$bathy_depth_m else 0,
        na.rm = TRUE))
  y_rng <- c(0, y_max)

  p_main <- ggplot() +
    geom_raster(
      data        = d_grid,
      aes(dist_km, depth_m, fill = value),
      interpolate = TRUE) +
    scale_fill_viridis_c(name = units_lab) +
    geom_contour(
      data      = d_grid,
      aes(dist_km, depth_m, z = value),
      breaks    = ctr_breaks,
      color     = "white",
      alpha     = 0.4,
      linewidth = 0.3)

  # value labels on the contour lines — solid bold white reads against both
  # the faint lines and the dark interpolated field
  if (!is.null(ctr_lab)) {
    p_main <- p_main +
      geom_text(
        data     = ctr_lab,
        aes(x, y, label = level),
        color    = "white",
        size     = 2.6,
        fontface = "bold")
  }

  # observation points: every ctd_thin sample that fed the interpolation,
  # drawn last (on top) and prominently — white fill, dark ring, slightly
  # transparent so dense/overlapping samples stay legible — so observed
  # values read clearly against the interpolated field. customdata = ord_occ
  # only tags this trace so the hover wiring below can pick it out; text =
  # hover drives the tooltip. ggplot2 warns "unknown aesthetic" for both —
  # benign, ggplotly still carries them onto the trace, so muffle at source.
  p_main <- p_main +
    suppressWarnings(geom_point(
      data  = xyz,
      aes(x, y, customdata = ord_occ, text = hover),
      shape = 21, fill = "white", colour = "grey20",
      size  = 1.5, stroke = 0.3, alpha = 0.6))

  if (!is.null(bathy_data)) {
    bathy_poly <- bind_rows(
      bathy_data |> mutate(bathy_depth_m = pmin(bathy_depth_m, y_max)),
      tibble(
        dist_km       = c(max(bathy_data$dist_km), min(bathy_data$dist_km)),
        bathy_depth_m = c(y_max, y_max)))

    p_main <- p_main +
      geom_polygon(
        data      = bathy_poly,
        aes(dist_km, bathy_depth_m),
        fill      = "grey70",
        colour    = "black",
        linewidth = 0.4)
  }

  # faint vertical guide line at each station occupation; the letter labels
  # themselves go on a paper-anchored top axis after ggplotly() (below), so
  # they stay visible at any depth zoom rather than clipping inside the panel
  station_labels <- xyz |>
    distinct(ord_occ, x, label) |>
    arrange(x)
  p_main <- p_main +
    geom_vline(
      data = station_labels, aes(xintercept = x),
      color = "white", alpha = 0.4, linewidth = 0.3)

  title_lab <- if (!is.null(cruise_key)) {
    glue("{meas_label} — Cruise {cruise_key}")
  } else meas_label

  p_main <- p_main +
    scale_y_reverse(limits = rev(y_rng), expand = c(0, 0)) +
    scale_x_continuous(limits = x_rng, expand = expansion(mult = 0.04)) +
    labs(x = "Distance (km)", y = "Depth (m)", title = title_lab) +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank())

  # tooltip = "text" -> the point trace's hover comes from the `hover` column;
  # ggplot2 warns "unknown aesthetic" for customdata/text — benign, ggplotly
  # still carries them onto the trace, so mute just that warning
  p <- withCallingHandlers(
    plotly::ggplotly(p_main, tooltip = "text"),
    warning = function(w) {
      if (grepl("unknown aesthetic", conditionMessage(w), ignore.case = TRUE))
        invokeRestart("muffleWarning")
    })
  # hover only on the observation-point trace (the one carrying customdata =
  # ord_occ); the interpolated raster, contours, bathymetry + guide lines are
  # context, not values to inspect — silence them. this also drops ggplotly's
  # default x/y readout, which was inconsistent across layers and showed a
  # confusing negative depth (an artifact of scale_y_reverse).
  for (i in seq_along(p$x$data)) {
    if (is.null(p$x$data[[i]]$customdata)) {
      p$x$data[[i]]$hoverinfo <- "skip"
    } else {
      p$x$data[[i]]$hovertemplate <- "%{text}<extra></extra>"
    }
  }
  # station letters as a paper-anchored top axis: xref = "x" tracks each
  # station's distance (moves with x-zoom), yref = "paper" pins them just
  # above the panel so they stay visible at any depth zoom — unlike an
  # in-panel geom_text, which clips when zoomed into a depth range
  p |>
    plotly::add_annotations(
      x = station_labels$x, y = 1, xref = "x", yref = "paper",
      yanchor = "bottom", text = station_labels$label, showarrow = FALSE,
      font = list(size = 12, color = "#222")) |>
    plotly::layout(dragmode = "zoom", margin = list(t = 70)) |>
    plotly::config(displayModeBar = TRUE)
}

# per-occupation depth summary for a cruise (max depth + # retained depths),
# joining ctd_thin -> ctd_cast for ord_occ. measurement-independent, so it can
# be computed once at cruise load and merged into the map tooltip.
occ_depth_summary <- function(con, cruise_key) {
  dbGetQuery(con, glue("
    SELECT c.ord_occ,
           ROUND(MAX(t.depth_m))     AS max_depth_m,
           COUNT(DISTINCT t.depth_m) AS n_depths
    FROM ctd_thin t
    JOIN ctd_cast c ON t.ctd_cast_uuid = c.ctd_cast_uuid
    WHERE t.cruise_key = '{cruise_key}'
    GROUP BY c.ord_occ"))
}

# cruise-level summary statistics for the Cruise Stats subtab. returns a tidy
# (metric, value) tibble. ctd_cast and ctd_thin are stored at the depth-scan
# grain, so every per-cast metric aggregates to the station occupation
# (ord_occ) — joining ctd_thin -> ctd_cast where ord_occ is needed.
cruise_stats <- function(con, cruise_key) {
  ck <- cruise_key

  # occupation-level cast counts
  casts <- dbGetQuery(con, glue("
    SELECT
      COUNT(DISTINCT ord_occ)                                AS n_casts,
      COUNT(DISTINCT ord_occ) FILTER (WHERE cast_dir = 'D')   AS n_down,
      COUNT(DISTINCT ord_occ) FILTER (WHERE cast_dir = 'U')   AS n_up
    FROM ctd_cast WHERE cruise_key = '{ck}'"))

  # hours between consecutive station occupations (first datetime per occupation)
  time_gap <- dbGetQuery(con, glue("
    WITH occ AS (
      SELECT ord_occ, MIN(datetime_utc) AS dt
      FROM ctd_cast WHERE cruise_key = '{ck}' GROUP BY ord_occ),
    g AS (
      SELECT EPOCH(dt - LAG(dt) OVER (ORDER BY dt)) / 3600.0 AS gap_hr FROM occ)
    SELECT ROUND(MEDIAN(gap_hr), 2) AS median_hr FROM g WHERE gap_hr IS NOT NULL"))

  # depth gap between retained ctd_thin samples within an occupation profile
  # (temperature_ave as the representative variable)
  depth_gap <- dbGetQuery(con, glue("
    WITH prof AS (
      SELECT DISTINCT c.ord_occ, t.depth_m
      FROM ctd_thin t
      JOIN ctd_cast c ON t.ctd_cast_uuid = c.ctd_cast_uuid
      WHERE t.cruise_key = '{ck}' AND t.measurement_type = 'temperature_ave'),
    g AS (
      SELECT depth_m - LAG(depth_m) OVER (
               PARTITION BY ord_occ ORDER BY depth_m) AS gap_m
      FROM prof)
    SELECT ROUND(MEDIAN(gap_m), 2) AS median_m FROM g WHERE gap_m IS NOT NULL"))

  # replicate-aggregation stats from ctd_summary
  summ <- dbGetQuery(con, glue("
    SELECT ROUND(AVG(n_obs), 2)  AS avg_n_obs,
           ROUND(AVG(stddev), 4) AS avg_stddev
    FROM ctd_summary WHERE cruise_key = '{ck}'"))

  rows <- list(
    c("Casts (station occupations)", format(casts$n_casts, big.mark = ",")),
    c("Occupations with downcast / upcast",
      paste(casts$n_down, "/", casts$n_up)),
    c("Median hours between occupations", time_gap$median_hr),
    c("Median depth gap, ctd_thin (m)",   depth_gap$median_m),
    c("Avg replicates per site×depth×type (n_obs)", summ$avg_n_obs),
    c("Avg stddev between replicates",     summ$avg_stddev))

  if (thin_has_reason) {
    infl <- dbGetQuery(con, glue("
      WITH p AS (
        SELECT c.ord_occ, t.retained_reason
        FROM ctd_thin t
        JOIN ctd_cast c ON t.ctd_cast_uuid = c.ctd_cast_uuid
        WHERE t.cruise_key = '{ck}')
      SELECT ROUND(
        COUNT(*) FILTER (WHERE retained_reason = 'inflection')
        / GREATEST(COUNT(DISTINCT ord_occ), 1)::DOUBLE, 1) AS per_cast
      FROM p"))
    rows <- c(rows, list(
      c("Avg inflection samples per occupation", infl$per_cast)))
  }

  tibble(
    metric = vapply(rows, `[`, character(1), 1),
    value  = vapply(rows, `[`, character(1), 2))
}

# cleanup ----
onStop(function() {
  dbDisconnect(con, shutdown = TRUE)
})
