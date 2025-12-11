librarian::shelf(
  dbplyr, 
  dplyr, 
  duckdb,
  glue,
  leaflet,
  leaflet.extras,
  mapview,
  sf,
  viridis)

dir_db <- switch(
  Sys.info()["sysname"],
  Linux = "/share/public/data",
  Darwin = "~/My Drive/projects/calcofi/data/calcofi.org/ctd-cast/download"
)
db_local <- glue("{dir_db}/calcofi-ctd.duckdb")
con <- dbConnect(duckdb::duckdb(), dbdir = db_local, read_only = TRUE)

dbExecute(con, "INSTALL icu; LOAD icu;")
dbExecute(con, "INSTALL spatial; LOAD spatial;")

# dbListFields(con, "ctd")

# Extract daily cruise line segments with simplified geometries
d_segs <- tbl(con, "ctd") |>
  select(cruise_id, dtime_utc, ord_occ, event_num, cast_id, point_geom) |>
  mutate(
    dtime_pst = sql("timezone('America/Los_Angeles', dtime_utc)"),
    date_pst  = sql("date_trunc('day', dtime_pst)")
  ) |>
  group_by(cruise_id, date_pst) |>
  window_order(dtime_pst, ord_occ) |>
  # Get temporal summaries
  summarise(
    beg_dtime = min(dtime_pst, na.rm = TRUE),
    end_dtime = max(dtime_pst, na.rm = TRUE),
    n_rows = n(),
    line_geom = sql("ST_Simplify(ST_RemoveRepeatedPoints(ST_MakeLine(LIST(point_geom))), 0.0001)"),
    .groups = "drop"
  ) |> 
  filter(n_rows >= 2) |>
  mutate(
    line_geom_wkt = sql("ST_AsText(line_geom)")
  ) |> 
  arrange(cruise_id, date_pst) |>
  collect()
sf_segs <- st_as_sf(d_segs, wkt = "line_geom_wkt", remove = F, crs = 4326)

object.size(sf_segs) |> format(units = "MB") # before: 345.8 Mb; after: 2.6 Mb

# mapView(sf_segs, 
#         zcol = "cruise_id",
#         layer.name = "Cruises")
sf_segs |> 
  filter(cruise_id == "0302JD") |>
  leaflet() |> 
  leaflet::addProviderTiles("Esri.OceanBasemap") |>
  leaflet::addPolylines()
  


  plot()
  pull(line_geom_wkt)
  mutate(

  )
  mapView()

  # Now query back with spatial operations in DuckDB
  {
    # Get the collected data
    daily_summary <- .
    
    # Create spatial linestrings with simplification
    con |>
      tbl("ctd") |>
      mutate(date_pst = as.Date(dtime_utc - INTERVAL '8 hours')) |>
      inner_join(
        copy_to(con, daily_summary, "daily_summary", temporary = TRUE),
        by = c("cruise_id", "date_pst")
      ) |>
      group_by(cruise_id, date_pst, beg_dtime, mid_dtime, end_dtime, n_points) |>
      summarise(
        # Create linestring from points ordered by time
        geom = ST_AsText(
          ST_Simplify(
            ST_MakeLine(
              ST_Point(lon_dec, lat_dec) 
              ORDER BY dtime_utc
            ),
            0.01  # Tolerance in degrees (~1km)
          )
        ),
        .groups = "drop"
      ) |>
      collect() |>
      # Convert to sf object
      st_as_sf(wkt = "geom", crs = 4326)
  }

# Preview with mapview
mapView(cruise_lines, 
        zcol = "cruise_id",
        layer.name = "Cruise Lines")