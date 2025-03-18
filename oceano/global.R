# TODO:
# - show: points, raster, polygons
# - play: animate window of time on bottom of map

# devtools::load_all("/share/github/oceanmetrics/leaftiles")

librarian::shelf(
  calcofi/calcofi4r,
  dbplyr, dplyr, DT, dygraphs, ggplot2, glue, here, htmltools, htmlwidgets, 
  leaflet, leaflet.extras, 
  oceanmetrics/leaftiles,
  # logger, loggit, 
  lubridate, plotly, png, readr, rnaturalearth, rnaturalearthhires, 
  shiny, shinydashboard, shinyjs, 
  stringr, webshot2)
# remotes::install_github("calcofi/calcofi4r", force=T)   # install remote
# devtools::install_local(here::here("../calcofi4r"), force=T)  # install local
# devtools::load_all(here("../calcofi4r"))                # debug

# loggit::set_logfile("/share/github/apps_dev/oceano/logs/loggit.log")
# loggit::read_logs()
# logger::log_appender(appender_file("/share/github/apps_dev/oceano/logs/logger.log"))
# logger::log_appender()

enableBookmarking(store = "url")

source(here("libs/db.R"))         # con: database connection 
source(here("libs/functions.R"))

dir_cache <- switch(
  Sys.info()["sysname"],
  Linux  = "/share/data/cache_idw",
  Darwin = here("cache"))
# TODO: add check in get_map_data() for existence of files and if not, 
#  recreate (since caches differ on server vs laptop); consider + cache_system column to track which cache
url_cache <- "https://file.calcofi.io/cache_idw"

# ranges for date and depth
rng_dates <- dbGetQuery(
  con, "SELECT MIN(date) min, MAX(date) max FROM ctd_casts")
rng_depths <- dbGetQuery(
  con, "SELECT MAX(depth_m) max FROM ctd_bottles")

# data table of variables
d_vars  <- tbl(con, "field_labels") |>
  # dbExecute(con, "UPDATE field_labels SET active = FALSE WHERE plot_title = 'Dissolved Inorganic Carbon'")
  filter(active) |>   
  arrange(plot_title) |> 
  collect()

aoi_cat_init <- "CalCOFI Zones"
aoi_keys_init <- c(
  "cc_nearshore-extended",
  "cc_offshore-extended",
  "cc_nearshore-standard",
  "cc_offshore-standard")
d_places <- cc_places |> 
  st_drop_geometry() |> 
  filter(category == aoi_cat_init)
aoi_rows_init <- which(d_places$key %in% aoi_keys_init)
  
# workaround Warning:
#   sf layer has inconsistent datum (+proj=longlat +ellps=WGS84 +no_defs).
#   Need '+proj=longlat +datum=WGS84'
cc_places <- st_transform(cc_places, 4326)


addCalcofiStations <- function(map){
# addCalcofiStations <- function(map, filter){
  
  map |> 
    addVectorTiles(
      server  = "https://tile.calcofi.io",
      layer   = "public.stations",
      # filter  = filter,
      layerId = "stationid",
      style = list(
        fillColor   = "purple",
        color       = "purple",
        fill        = TRUE,
        fillOpacity = 0.1,
        opacity     = 0.5,
        weight      = 0.2,
        radius      = 1),
      styleHighlight = list(
        fillColor   = "yellow",
        color       = "yellow",
        fill        = TRUE,
        fillOpacity = 0.1,
        opacity     = 0.5,
        weight      = 0.2,
        radius      = 1))
}