# packages ----
librarian::shelf(
  calcofi/calcofi4r,
  bslib, bsicons, DBI, dbplyr, dplyr, DT, glue,
  mapgl, purrr, sf, shiny, stringr, tibble, tidyr,
  quiet = T)

# database ----
con <- calcofi4r::cc_get_db()

# dataset colors ----
dataset_colors <- c(
  ichthyo    = "#ff6b6b",
  bottle     = "#4ecdc4",
  `ctd-cast` = "#45b7d1",
  dic        = "#f9ca24")

# cruise summary ----
# build cross-dataset cruise summary with site counts per dataset

# ichthyo sites per cruise
d_ichthyo <- tbl(con, "site") |>
  filter(!is.na(cruise_key)) |>
  group_by(cruise_key) |>
  summarize(ichthyo = n_distinct(site_key), .groups = "drop") |>
  collect()

# bottle casts per cruise
d_bottle <- tbl(con, "casts") |>
  filter(!is.na(cruise_key)) |>
  group_by(cruise_key) |>
  summarize(bottle = n_distinct(site_key), .groups = "drop") |>
  collect()

# ctd casts per cruise
d_ctd <- tbl(con, "ctd_cast") |>
  filter(!is.na(cruise_key)) |>
  group_by(cruise_key) |>
  summarize(`ctd-cast` = n_distinct(site_key), .groups = "drop") |>
  collect()

# dic samples per cruise (via casts join)
d_dic <- tbl(con, "dic_sample") |>
  inner_join(
    tbl(con, "casts") |> select(cast_id, cruise_key),
    by = "cast_id") |>
  filter(!is.na(cruise_key)) |>
  group_by(cruise_key) |>
  summarize(dic = n_distinct(site_key), .groups = "drop") |>
  collect()

# cruise metadata
d_cruise <- tbl(con, "cruise") |>
  inner_join(
    tbl(con, "ship") |> select(ship_key, ship_name, ship_nodc),
    by = "ship_key") |>
  select(cruise_key, date_ym, ship_name, ship_nodc) |>
  collect() |>
  mutate(
    year  = as.integer(format(date_ym, "%Y")),
    month = as.integer(format(date_ym, "%m")))

# join all datasets into summary
d_summary <- d_cruise |>
  left_join(d_ichthyo, by = "cruise_key") |>
  left_join(d_bottle,  by = "cruise_key") |>
  left_join(d_ctd,     by = "cruise_key") |>
  left_join(d_dic,     by = "cruise_key") |>
  mutate(across(c(ichthyo, bottle, `ctd-cast`, dic), \(x) replace_na(x, 0L))) |>
  arrange(desc(year), desc(month))

# helper: connect sequential casts by datetime ----
compute_segments <- function(casts_df) {
  n <- nrow(casts_df)
  if (n < 2) return(NULL)

  casts_df |>
    mutate(
      lon_beg      = lag(lon),
      lat_beg      = lag(lat),
      datetime_beg = lag(datetime_utc)) |>
    filter(!is.na(lon_beg)) |>
    mutate(
      seg_idx = row_number(),
      m       = pmap(
        list(lon_beg, lat_beg, lon, lat),
        \(x1, y1, x2, y2) matrix(c(x1, y1, x2, y2), nrow = 2, byrow = T)),
      geom    = map(m, st_linestring)) |>
    select(-m) |>
    st_as_sf(sf_column_name = "geom", crs = 4326)
}

# cleanup ----
onStop(function() {
  dbDisconnect(con, shutdown = TRUE)
})
