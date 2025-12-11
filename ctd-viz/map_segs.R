librarian::shelf(
  dbplyr,
  dplyr,
  duckdb,
  glue,
  leaflet,
  leaflet.extras,
  mapview,
  purrr,
  sf,
  viridis
)

dir_db <- switch(
  Sys.info()["sysname"],
  Linux = "/share/public/data",
  Darwin = "~/My Drive/projects/calcofi/data/calcofi.org/ctd-cast/download"
)
db_local <- glue("{dir_db}/calcofi-ctd.duckdb")
con <- dbConnect(duckdb::duckdb(), dbdir = db_local, read_only = TRUE)

dbExecute(con, "INSTALL icu; LOAD icu;")
dbExecute(con, "INSTALL spatial; LOAD spatial;")
dbExecute(con, "SET TIMEZONE = 'UTC'") # https://github.com/duckdb/duckdb/issues/9381#issuecomment-1768727953

# dbListFields(con, "ctd")

# Step 1: Base data with PST timestamps
base <- tbl(con, "ctd") |>
  select(cruise_id, dtime_utc, ord_occ, point_geom) |>
  mutate(
    dtime_pst = sql("timezone('America/Los_Angeles', dtime_utc)::TIMESTAMPTZ"),
    date_pst = sql("(timezone('America/Los_Angeles', dtime_utc))::DATE")
  )

# Step 2: Get sequence of dates to find "previous date" for each date
dates_seq <- base |>
  distinct(cruise_id, date_pst) |>
  group_by(cruise_id) |>
  window_order(date_pst) |>
  mutate(prev_date_pst = lag(date_pst)) |>
  ungroup()

# Step 3: Get first point of each day (these become bridge points)
first_pts <- base |>
  group_by(cruise_id, date_pst) |>
  window_order(dtime_pst, ord_occ) |>
  filter(row_number() == 1L) |>
  ungroup()

# Step 4: Create bridge points - first point of each day assigned to previous day's segment
bridge_pts <- first_pts |>
  inner_join(dates_seq, by = c("cruise_id", "date_pst")) |>
  filter(!is.na(prev_date_pst)) |>
  transmute(
    cruise_id,
    segment_date = prev_date_pst,
    dtime_pst,
    ord_occ,
    point_geom
  )

# Step 5: Union original data with bridge points
all_pts <- union_all(
  base |>
    transmute(
      cruise_id,
      segment_date = date_pst,
      dtime_pst,
      ord_occ,
      point_geom
    ),
  bridge_pts
)

# Step 6: Create line segments grouped by segment_date
d_segs <- all_pts |>
  group_by(cruise_id, segment_date) |>
  summarize(
    n_rows = n(),
    beg_dtime = min(dtime_pst, na.rm = TRUE),
    end_dtime = max(dtime_pst, na.rm = TRUE),
    line_geom = sql(
      "ST_Simplify(ST_RemoveRepeatedPoints(ST_MakeLine(LIST(point_geom ORDER BY dtime_pst, ord_occ))), 0.0001)"
    ),
    .groups = "drop"
  ) |>
  filter(n_rows >= 2L) |>
  mutate(line_geom_wkt = sql("ST_AsText(line_geom)")) |>
  rename(date_pst = segment_date) |>
  arrange(cruise_id, date_pst) |>
  collect()

sf_segs <- st_as_sf(d_segs, wkt = "line_geom_wkt", remove = FALSE, crs = 4326)

object.size(sf_segs) |> format(units = "MB") # 5.5 Mb

# sort(unique(sf_segs$cruise_id))

# mapView(sf_segs,
#         zcol = "cruise_id",
#         layer.name = "Cruises")

# sf_segs |>
#   filter(cruise_id == "2507SR") |>
#   leaflet() |>
#   leaflet::addProviderTiles("Esri.OceanBasemap") |>
#   leaflet::addPolylines()

# get bbox for each cruise
d_cruise_bb <- suppressWarnings(
  sf_segs |>
    group_by(cruise_id) |>
    # summarize(bbox = st_bbox(st_union(geometry))) |>
    summarize() |>
    mutate(
      bb = map(geometry, \(x) st_bbox(x)),
      xmin = map_dbl(bb, "xmin"),
      ymin = map_dbl(bb, "ymin"),
      xmax = map_dbl(bb, "xmax"),
      ymax = map_dbl(bb, "ymax"),
    ) |>
    select(-bb) |>
    st_drop_geometry() |>
    arrange(cruise_id)
)
# d_cruise_bb |> View()

cruises_valid <- d_cruise_bb |>
  filter(
    ymin > 20,
    xmax < -100
  ) |>
  pull(cruise_id)

cruises_invalid <- setdiff(d_cruise_bb$cruise_id, cruises_valid)
d_cruise_bb |>
  filter(cruise_id %in% cruises_invalid)
   cruise_id  xmin  ymin  xmax  ymax
   <chr>     <dbl> <dbl> <dbl> <dbl>
 1 0310NH    -172.  29.8 172.   35.1
 2 0501NH    -124.   0     0    35.1
 3 0504NH    -124.   0     0    35.1
 4 0511NH    -124.  29.8 -99    35.1
 5 0701JD    -125.   0     0    36.8
 6 0707NH    -124.   0     0    35.1
 7 0711NH    -170.  29.8 175.   35.0
 8 0801JD    -180. -89.5 179.   89.9
 9 1307NH    -124.   0     0    35.1
10 1311NH    -124.   0     0    35.1
11 1402SH    -124.   0     0    34.0
12 1507OC    -125.  29.8  12.5  35.1
13 2404SH    -177. -87.9 180.   89.0

# vs 80 valid

# Create labels for popup and hover
sf_segs_filt <- sf_segs |>
  # filter(cruise_id == "0302JD") |>
  filter(cruise_id %in% cruises_valid) |>
  mutate(
    date_pst_f = as.factor(as.character(date_pst)),
    popup_label = glue(
      "<b>Cruise:</b> {cruise_id}<br>",
      "<b>Date:</b> {as.character(date_pst)}<br>",
      "<b>Start:</b> {format(beg_dtime, '%Y-%m-%d %H:%M')}<br>",
      "<b>End:</b> {format(end_dtime, '%Y-%m-%d %H:%M')}<br>",
      "<b>Points:</b> {n_rows}"
    ),
    hover_label = glue("{cruise_id} | {as.character(date_pst)}")
  )

# Create color palette based on date_pst as factor
# n_dates <- length(unique(sf_segs_filt$date_pst_f))
# pal <- colorFactor(
#   palette = viridis(n_dates),
#   domain = sf_segs_filt$date_pst_f
# )

# Create color palette based on cruise_id as factor
n_clr <- length(unique(sf_segs_filt$cruise_id))
pal <- colorFactor(
  palette = viridis(n_clr),
  domain = sf_segs_filt$cruise_id
)

# Create interactive map with hover, popup, and color by date
sf_segs_filt |>
  leaflet() |>
  addProviderTiles("Esri.OceanBasemap") |>
  addPolylines(
    color = ~ pal(cruise_id),
    weight = 2,
    opacity = 0.8,
    label = ~hover_label,
    popup = ~popup_label,
    highlightOptions = highlightOptions(
      color = "yellow",
      weight = 4,
      opacity = 1,
      bringToFront = TRUE
    )
  ) |>
  addLegend(
    "bottomright",
    pal = pal,
    values = sf_segs_filt$cruise_id,
    title = "Cruise ID",
    opacity = 1
  )
