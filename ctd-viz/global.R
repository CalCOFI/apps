# packages ----
librarian::shelf(
  bslib, bsicons, calcofi/calcofi4r, conductor,
  DBI, dbplyr, dplyr, DT, duckdb,
  geosphere, ggplot2, glue, htmltools,
  lubridate, mapgl, MBA, plotly,
  purrr, readr, sf, shiny, shinyjqui, stringr, terra,
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
# and that the app-facing helper columns (cast_seq, dtime_pt) were baked in.
stopifnot(
  "ctd_thin not in database; rebuild with prep_db.R against v2026.05.14+" =
    "ctd_thin" %in% dbListTables(con))
cast_flds <- dbListFields(con, "ctd_cast")
if (!all(c("cast_seq", "dtime_pt") %in% cast_flds))
  stop(
    "ctd_cast missing helper columns (cast_seq, dtime_pt). ",
    "Rebuild with: Rscript prep_db.R latest TRUE")

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
    # strip the redundant YYYY-MM- prefix from cruise_key — it's already in
    # date_ym, so "2025-11-33P4" -> "33P4" in the parenthetical
    label = paste0(
      date_ym, " — ", ship_name, " (",
      sub("^\\d{4}-\\d{2}-", "", cruise_key), ")"))

cruise_vec     <- setNames(cruise_choices$cruise_key, cruise_choices$label)
default_cruise <- cruise_choices$cruise_key[1]

# which cruises carry each measurement — used by server.R to filter the
# cruise dropdown when the user picks a different variable (some cruises
# don't carry, e.g., pH for older runs). built once at startup.
meas_to_cruises <- dbGetQuery(con, "
  SELECT DISTINCT measurement_type, cruise_key
  FROM ctd_thin") |>
  tibble::as_tibble()

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

# map each station occupation to its cast_seq (numeric ord_occ, drops zero
# padding). returned as character so it can drive plot/map text fields
# directly. one source of truth so map markers and the plot x-axis agree.
assign_occ_labels <- function(ord_occ) {
  o <- sort(unique(ord_occ[!is.na(ord_occ)]))
  if (length(o) == 0) return(setNames(character(0), character(0)))
  seq_int <- suppressWarnings(as.integer(o))
  labs    <- ifelse(is.na(seq_int), o, as.character(seq_int))
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

  # MBA interpolation (ODV-style smooth field). clip the visible grid to
  # the actual data extent so the interpolated surface doesn't extrapolate
  # into the bathymetry zone when the user drags the slider deeper.
  surf <- MBA::mba.surf(
    xyz[, c("x", "y", "z")], no.X = interp_n, no.Y = interp_n,
    extend = TRUE)$xyz.est
  grid_max <- min(max_depth, max(xyz$y, na.rm = TRUE) + 10)
  d_grid   <- expand.grid(dist_km = surf$x, depth_m = surf$y) |>
    mutate(value = as.vector(surf$z)) |>
    filter(depth_m >= 0, depth_m <= grid_max)

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
  # y-axis is driven directly by the Max-depth slider (server auto-sets it
  # to data_max + small pad on each selection change). dragging the slider
  # deeper exposes the bathymetry silhouette; shallower focuses on the data.
  y_max <- max_depth
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

# single-cast profile plotly: value on x, depth on y (0 at top). called when
# the selection has exactly one occupation — the transect plot needs ≥ 2
# casts to interpolate. an optional `bathy_depth` (seafloor depth in m at the
# cast's lon/lat) is drawn as a dashed horizontal line.
build_profile_plotly <- function(meas_data, meas_label, max_depth,
                                 cruise_key = NULL, bathy_depth = NULL) {
  d <- meas_data |>
    filter(!is.na(measurement_value)) |>
    arrange(depth_m)
  if (nrow(d) < 2) return(NULL)

  units_lab <- sub("^.*\\(([^)]*)\\)\\s*$", "\\1", meas_label)
  cast_lab  <- unique(d$cast_seq)[1]
  ttl <- if (!is.null(cruise_key)) {
    glue("{meas_label} — Cast {cast_lab} on Cruise {cruise_key}")
  } else {
    glue("{meas_label} — Cast {cast_lab}")
  }

  p <- plotly::plot_ly(
    data         = d,
    x            = ~measurement_value,
    y            = ~depth_m,
    type         = "scatter",
    mode         = "lines+markers",
    line         = list(color = "#0077cc", width = 2),
    marker       = list(
      color = "#0077cc", size = 6,
      line  = list(color = "white", width = 1)),
    hovertemplate = paste0(
      "<b>Depth:</b> %{y:.1f} m<br>",
      meas_label, ": %{x:.3f}<extra></extra>"))

  # seafloor as a dashed horizontal line, if a depth was supplied + visible
  if (!is.null(bathy_depth) && !is.na(bathy_depth) &&
      bathy_depth > 0 && bathy_depth <= max_depth) {
    xr <- range(d$measurement_value, na.rm = TRUE)
    p  <- p |>
      plotly::add_segments(
        x         = xr[1], xend = xr[2],
        y         = bathy_depth, yend = bathy_depth,
        line      = list(color = "#666", dash = "dash", width = 1),
        hoverinfo = "skip", showlegend = FALSE, inherit = FALSE) |>
      plotly::add_annotations(
        x = xr[2], y = bathy_depth, xanchor = "right", yanchor = "bottom",
        text = paste0("seafloor ≈ ", round(bathy_depth), " m"),
        showarrow = FALSE,
        font = list(size = 10, color = "#666"))
  }

  p |>
    plotly::layout(
      title  = list(text = ttl, x = 0, font = list(size = 13)),
      xaxis  = list(title = units_lab, side = "top"),
      yaxis  = list(title = "Depth (m)", range = c(max_depth, 0)),
      margin = list(t = 80), showlegend = FALSE) |>
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

# tour ----
# conductor tour: auto-shown on first visit (gated client-side via
# localStorage), and re-openable any time from the help icon in the header.
# step `el` selectors target ids assigned in ui.R.
tour <- Conductor$new(exitOnEsc = TRUE, keyboardNavigation = TRUE)$
  step(
    title = "Welcome to CalCOFI CTD Casts",
    text  = "Explore <b>CTD</b> (<b>C</b>onductivity, <b>T</b>emperature,
      <b>D</b>epth) casts from CalCOFI cruises (1949–present). The instrument
      is lowered through the water column on a winch, recording temperature,
      salinity (derived from conductivity), pressure, oxygen, and other
      variables every few centimetres of descent.<br><br>
      <b>What you're looking at:</b> casts have been adaptively thinned
      (Douglas-Peucker, ~10 m grid + measurement inflections) and limited to
      <b>downcasts only</b>, so the app stays fast while preserving the shape
      of each profile.<br><br>
      <i>Press Esc to exit at any time. Re-open this tour with the
      <span style='font-size:1.1em'>?</span> in the header.</i>")$
  step(
    el    = "#sel_cruise + .selectize-control",
    title = "Pick a cruise",
    text  = "Cruises are labelled <b>YYYY-MM — ship (cruise_key)</b>, newest
      first. Changing the cruise reloads the map, the table, and the plot.")$
  step(
    el    = "#sel_meas_type + .selectize-control",
    title = "Pick a measurement",
    text  = "The selected variable drives the transect plot and the
      Measurements table. Choices are the canonical CTD variables retained in
      <code>ctd_thin</code>. The cruise dropdown auto-filters to cruises
      that actually carry the chosen variable.")$
  step(
    el    = "#pane_top",
    title = "Map: pick a transect",
    text  = "Click one station to anchor the transect, then click another —
      every station between them along the cruise track gets selected.
      Only casts that carry the chosen measurement appear on the map.
      Selected casts are numbered by <b>cast_seq</b> (station-occupation
      order).")$
  step(
    el    = "#subtabs",
    title = "Table / Plot",
    text  = "<b>Casts</b>: click rows to multi-select arbitrary stations,
      reset the selection, and download.<br>
      <b>Measurements</b>: the <code>ctd_thin</code> rows feeding the plot.<br>
      <b>Plot</b>: ODV-style interpolated transect, with bathymetry as a
      clipped silhouette underneath.")$
  step(
    el    = "#btn_settings",
    title = "Plot settings",
    text  = "Optional <b>Bathymetry</b> overlay on the map (GEBCO 2025
      seafloor) and an optional <b>Max depth</b> cap on the plot y-axis &
      Measurements table. Both default off / open so the plot fits the
      data it just drew.")

# cleanup ----
onStop(function() {
  dbDisconnect(con, shutdown = TRUE)
})
