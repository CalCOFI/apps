# packages ----
librarian::shelf(
  calcofi / calcofi4r,
  bslib,
  bsicons,
  DBI,
  dbplyr,
  dplyr,
  DT,
  glue,
  mapgl,
  purrr,
  sf,
  shiny,
  stringr,
  tibble,
  tidyr,
  quiet = T
)
# debugging: load calcofi4r from local source
devtools::load_all("../../calcofi4r")

# database ----
con <- calcofi4r::cc_get_db()

# dbListTables(con)

# dataset colors ----
dataset_colors <- c(
  ichthyo = "#ff6b6b",
  bottle = "#4ecdc4",
  `ctd-cast` = "#45b7d1",
  dic = "#f9ca24"
)

# cruise summary ----
# the enriched `cruise` table already carries per-dataset station counts
# (distinct root sampling events per cruise), precomputed at release time in the
# columns ichthyo / bottle / ctd_cast / dic. read it directly instead of
# rolling up the retired per-dataset event tables (site / casts / ctd_cast /
# dic_sample). rename ctd_cast -> `ctd-cast` to preserve the app's column shape.
d_summary <- tbl(con, "cruise") |>
  select(cruise_key, year, month, ship_name, ichthyo, bottle, ctd_cast, dic) |>
  collect() |>
  rename(`ctd-cast` = ctd_cast) |>
  mutate(
    year  = as.integer(year),
    month = as.integer(month),
    across(c(ichthyo, bottle, `ctd-cast`, dic), \(x) replace_na(as.integer(x), 0L))) |>
  arrange(desc(year), desc(month))

# helper: connect sequential casts by datetime ----
compute_segments <- function(casts_df) {
  n <- nrow(casts_df)
  if (n < 2) {
    return(NULL)
  }

  casts_df |>
    mutate(
      lon_beg = lag(lon),
      lat_beg = lag(lat),
      datetime_beg = lag(datetime_utc)
    ) |>
    filter(!is.na(lon_beg)) |>
    mutate(
      seg_idx = row_number(),
      m = pmap(
        list(lon_beg, lat_beg, lon, lat),
        \(x1, y1, x2, y2) matrix(c(x1, y1, x2, y2), nrow = 2, byrow = T)
      ),
      geom = map(m, st_linestring)
    ) |>
    select(-m) |>
    st_as_sf(sf_column_name = "geom", crs = 4326)
}

# cleanup ----
onStop(function() {
  dbDisconnect(con, shutdown = TRUE)
})
