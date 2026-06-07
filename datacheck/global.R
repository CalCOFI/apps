# datacheck — cross-dataset observation explorer.
#
# one row per sampling event across ALL released CalCOFI datasets (the `obs`
# table built by prep_db.R), keyed by cruise. pick a cruise -> see every
# dataset's stations for that cruise on a map + table + space-time plot, with a
# deep-linkable URL (?cruise=&datasets=&id=) so a specific view can be pasted
# into a provider question or an issue. this is the feature ctd-viz lacks.

# packages ----
librarian::shelf(
  bslib, bsicons, calcofi/calcofi4r, conductor,
  DBI, dplyr, DT, duckdb,
  glue, htmltools, lubridate, mapgl,
  plotly, purrr, readr, sf, shiny, shinyjqui, stringr, tibble,
  quiet = T)

# paths ----
app_dir <- dirname(normalizePath(
  sys.frame(1)$ofile %||% "global.R", mustWork = FALSE))
if (!dir.exists(app_dir)) app_dir <- getwd()

db_file <- if (dir.exists("/share/data")) {
  "/share/data/datacheck/datacheck.duckdb"
} else {
  path.expand("~/_big/calcofi.org/datacheck/datacheck.duckdb")
}
stopifnot(
  "prepped database not found; run `Rscript prep_db.R` first" =
    file.exists(db_file))

# database ----
con <- dbConnect(
  duckdb::duckdb(
    dbdir     = db_file,
    read_only = TRUE,
    config    = list(autoload_known_extensions = "true")))
dbExecute(con, "INSTALL spatial; LOAD spatial;")

# dataset palette ----
# stable colors per dataset, used by the map circles, the table chips and the
# plot. order = display order (legend / filter pills). a qualitative set chosen
# for separability on both light and dark basemaps.
dataset_pal <- c(
  "calcofi_bottle"             = "#4C78A8",  # blue
  "calcofi_ctd-cast"           = "#72B7B2",  # teal
  "calcofi_dic"                = "#F58518",  # orange
  "swfsc_ichthyo"              = "#54A24B",  # green
  "cce-lter_euphausiids"       = "#B279A2",  # purple
  "pic_zooplankton"            = "#E45756",  # red
  "swfsc_cufes"                = "#EECA3B",  # yellow
  "calcofi_phyllosoma"         = "#FF9DA6",  # pink
  "calcofi_bird_mammal_census" = "#9D755D")  # brown

# short human labels for datasets (used in the legend + tooltips)
dataset_label <- c(
  "calcofi_bottle"             = "Bottle (casts)",
  "calcofi_ctd-cast"           = "CTD casts",
  "calcofi_dic"                = "DIC samples",
  "swfsc_ichthyo"              = "Ichthyoplankton (sites)",
  "cce-lter_euphausiids"       = "Euphausiids (tows)",
  "pic_zooplankton"            = "Zooplankton (tows)",
  "swfsc_cufes"                = "CUFES (samples)",
  "calcofi_phyllosoma"         = "Phyllosoma (tows)",
  "calcofi_bird_mammal_census" = "Bird/mammal (transects)")

# cruise choices ----
# built from obs itself (so every cruise that has plottable observations
# appears), enriched with ship_name from the cruise -> ship reference tables
# where available. label: "YYYY-MM — ship · N datasets (cruise_suffix)".
cruise_summary <- dbGetQuery(con, "
  SELECT
    o.cruise_key,
    MIN(o.datetime)::DATE         AS date_min,
    MAX(o.datetime)::DATE         AS date_max,
    COUNT(*)                      AS n_obs,
    COUNT(DISTINCT o.dataset)     AS n_datasets,
    s.ship_name                   AS ship_name
  FROM obs o
  LEFT JOIN cruise c ON o.cruise_key = c.cruise_key
  LEFT JOIN ship   s ON c.ship_key   = s.ship_key
  WHERE o.cruise_key IS NOT NULL
  GROUP BY o.cruise_key, s.ship_name") |>
  as_tibble() |>
  mutate(
    date_ym = if_else(
      is.na(date_min),
      str_extract(cruise_key, "^\\d{4}-\\d{2}"),
      format(date_min, "%Y-%m")),
    ship_lab = if_else(is.na(ship_name) | ship_name == "", "—", ship_name),
    suffix   = str_remove(cruise_key, "^\\d{4}-\\d{2}-"),
    label    = glue(
      "{date_ym} — {ship_lab} · {n_datasets} ds ({suffix})")) |>
  arrange(desc(date_ym), desc(n_datasets), cruise_key)

cruise_vec     <- setNames(cruise_summary$cruise_key, cruise_summary$label)
# default: the most recent cruise with the widest dataset coverage
default_cruise <- cruise_summary |>
  arrange(desc(n_datasets), desc(date_ym)) |>
  slice(1) |>
  pull(cruise_key)

# helpers ----

# pull every observation for one cruise as an sf of points (one row per event).
# adds display columns (color, dataset label) so the map / table / plot share a
# single source of truth.
obs_for_cruise <- function(cruise_key) {
  d <- dbGetQuery(con, glue_sql("
    SELECT dataset, tbl, id, cruise_key, latitude, longitude, datetime, site_key
    FROM obs WHERE cruise_key = {cruise_key}
    ORDER BY dataset, datetime, id", .con = con)) |>
    as_tibble() |>
    mutate(
      color    = unname(dataset_pal[dataset]),
      ds_label = unname(dataset_label[dataset]),
      color    = if_else(is.na(color), "#888888", color),
      ds_label = if_else(is.na(ds_label), dataset, ds_label),
      # the deep-link key for a single observation: "<dataset>:<id>"
      obs_key  = paste0(dataset, ":", id),
      tooltip  = paste0(
        "<b>", ds_label, "</b><br>",
        "id: ", id, "<br>",
        if_else(is.na(site_key) | site_key == "", "",
                paste0("site: ", site_key, "<br>")),
        if_else(is.na(datetime), "",
                paste0("time: ", format(datetime, "%Y-%m-%d %H:%M"), "<br>")),
        "lon, lat: ", round(longitude, 4), ", ", round(latitude, 4)))
  d
}

obs_to_sf <- function(d) {
  if (nrow(d) == 0) return(NULL)
  st_as_sf(d, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
}

# tour ----
tour <- Conductor$new(exitOnEsc = TRUE, keyboardNavigation = TRUE)$
  step(
    title = "CalCOFI DataCheck",
    text  = "Cross-reference <b>every dataset</b> sampled on a common CalCOFI
      cruise. Each point is one sampling event — a bottle cast, a CTD cast, a
      net tow, an underway sample, a bird/mammal transect — colored by its
      source dataset.<br><br>
      Use it to spot coverage gaps and mismatches across datasets (the kind of
      thing worth asking a data provider about), then copy the URL to reference
      that exact view in a question.<br><br>
      <i>Press Esc to exit; re-open with the <span style='font-size:1.1em'>?</span>
      in the header.</i>")$
  step(
    el    = "#sel_cruise + .selectize-control",
    title = "Pick a cruise",
    text  = "Cruises are labelled <b>YYYY-MM — ship · N datasets
      (cruise_key)</b>, widest coverage first. Changing it reloads the map,
      table and plot — and updates the URL.")$
  step(
    el    = "#ds_pills",
    title = "Filter datasets",
    text  = "Toggle datasets on/off. The map, table and plot follow, and the
      URL records your selection so a filtered view is shareable.")$
  step(
    el    = "#pane_map",
    title = "Map",
    text  = "Stations colored by dataset. Click a point to pin it — the table
      scrolls to it and the URL gains <code>&id=dataset:id</code>, so you can
      link a colleague straight to one observation.")$
  step(
    el    = "#subtabs",
    title = "Table / Plot",
    text  = "<b>Table</b>: every observation for the cruise (searchable,
      downloadable).<br>
      <b>Plot</b>: a space-time view (date × latitude) — overlapping points
      across datasets means they co-sampled the same station.")$
  step(
    el    = "#btn_copy",
    title = "Copy link",
    text  = "Grab the current URL — cruise, dataset filter and any pinned
      observation — to paste into a provider question, GitHub issue or email.")

# cleanup ----
onStop(function() {
  dbDisconnect(con, shutdown = TRUE)
})
