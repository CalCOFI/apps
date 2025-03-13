librarian::shelf(
  digest, dplyr, jsonlite, labeling, rnaturalearth, rnaturalearthhires, rpostgis, sf, terra, tidyr)

get_aoi_sql <- function(
    aoi_keys = NULL,
    aoi_ewkt = NULL){
  
  # aoi_keys = c("cc_nearshore-standard", "cc_nearshore-extended", "cc_offshore-standard","cc_offshore-extended")
  # aoi_keys = NULL
  # drawn <- mapedit::drawFeatures()
  # ( aoi_ewkt <- st_as_text(drawn$geometry, EWKT=T) )
  # aoi_ewkt = ""SRID=4326;POLYGON ((-67.06819 44.99416, -67.1857 44.94707, -67.21651 44.88058, -67.15834 44.78871, -67.04385 44.81789, -66.91015 44.86279, -67.06819 44.99416))"
  
  stopifnot(sum(c(is.null(aoi_ewkt), is.null(aoi_keys))) == 1)
  
  if (!is.null(aoi_keys)){
    # check all aoi_keys in places
    stopifnot(length(setdiff(aoi_keys, pull(tbl(con, "places"), key))) == 0)
    
    # get sql
    aoi_sql <- glue(
      "SELECT ST_Union(geom) AS geom
       FROM places 
       WHERE
         key IN ('{paste(aoi_keys, collapse = '\\',\\'')}')")
  }
  if (!is.null(aoi_ewkt)){
    if(length(aoi_ewkt) > 1){
      geoms   <- glue("ST_GeomFromEWKT('{aoi_ewkt}')")
      aoi_sql <- glue(
        "SELECT ST_COLLECT(
             {paste(geoms, collapse = ',\n')}
           ) AS geom")
    } else {
      aoi_sql <- glue("SELECT ST_GeomFromEWKT('{aoi_ewkt}') AS geom")
    }
  }      
  # test get_aoi_sql():
  # aoi_sql <- get_aoi_sql(aoi_keys = aoi_keys)
  # aoi <- st_read(con, query = aoi_sql)
  # mapview::mapView(aoi)
  
  aoi_sql
}
# aoi_sql <- get_aoi_sql(aoi_keys = aoi_keys)
# aoi_sql <- get_aoi_sql(aoi_ewkt = aoi_ewkt, aoi_keys = NULL)
# aoi <- st_read(con, query = aoi_sql)
# mapview::mapView(drawn)

get_aoi <- function(
    aoi_keys = NULL,
    aoi_ewkt = NULL){
  aoi_sql <- get_aoi_sql(aoi_keys = aoi_keys, aoi_ewkt = aoi_ewkt)
  
  aoi <- st_read(con, query = aoi_sql, quiet = T) |> 
    st_as_sf(aoi) |> 
    st_set_geometry("geom")
  
  if (!is.null(aoi_ewkt)){
    
    land <- rnaturalearth::ne_countries(
      scale = 10, returnclass = "sf") |> 
      filter(
        adm0_a3 %in% c("USA", "MEX")) |> 
      st_union()  # mapView(land)
    
    aoi <- st_difference(aoi, land) |> 
      st_make_valid()  # mapView(aoi)
  }
  
  aoi
}

get_hash <- function(..., algo = "crc32", add_attr_args = T){
  args <- setNames(
    list(...), 
    lapply(substitute(list(...))[-1], deparse))
  
  # sort values to prevent duplication
  if ("aoi_keys" %in% names(args) && length(args$aoi_keys) > 1)
    args$aoi_keys = sort(args$aoi_keys)
  
  hash <- digest(args, algo = algo)
  # message(glue("hash: {hash}"))
  
  if (add_attr_args)
    attr(hash, "args") <- args

  hash
}

get_variable <- function(variable){
  v <- tbl(con, "field_labels") %>% 
    filter(table_field == !!variable) %>% 
    collect() %>% 
    tidyr::separate(table_field, into=c("tbl", "fld"), sep="\\.", remove=F)
  stopifnot(nrow(v) == 1)
  v
}

update_idw_stats <- function(hash, view = NULL, download = NULL){
  stopifnot( sum(c(is.null(view), is.null(download))) == 1)
  
  if (!is.null(view)){
    stopifnot( view %in% c("ply", "rast") )
    q <- glue(
      "UPDATE idw_stats SET
           dtime_last_viewed = '{Sys.time()}',
           n_views_{view}    = n_views_{view} + 1
         WHERE hash = '{hash}'")
  } else {
    stopifnot( download %in% c("ply", "rast") )
    q <- glue(
      "UPDATE idw_stats SET
           dtime_last_downloaded  = '{Sys.time()}',
           n_downloads_{download} = n_downloads_{download} + 1
         WHERE hash = '{hash}'")
  }
  
  res <- dbExecute(con, q)
}

get_points <- function(
    variable, value, 
    aoi_ewkt = NULL, # for custom drawn AOI
    aoi_keys = c(
      "cc_nearshore-standard",
      "cc_nearshore-extended",
      "cc_offshore-standard",
      "cc_offshore-extended"),  # lookup in db.places (also calcofi4r::cc_places)
    date_beg, date_end, date_qrtr = 1,
    depth_m_min, depth_m_max){
  
  # variable
  v <- get_variable(variable)
  
  # area of interest
  aoi_sql <- get_aoi_sql(aoi_ewkt = aoi_ewkt, aoi_keys = aoi_keys)
  # aoi <- st_read(con, query = aoi_sql) # mapview::mapView(aoi)
  
  q_from <- case_when(
    v$tbl == "ctd_bottles"    ~ "ctd_casts JOIN ctd_bottles USING (cast_count)",
    v$tbl == "ctd_dic"        ~ "ctd_casts JOIN ctd_bottles USING (cast_count) JOIN ctd_dic USING (btl_cnt)")
  # TODO: variable, value; use MATERIALIZED VIEW to combine all vars into single table in advance
  # TODO: incorporate larvae_counts a la [plumber.R](https://github.com/CalCOFI/api/blob/d40c48b08316899c2e84481b4d07feb389fea957/plumber.R#L160-L163)
  # v$tbl == "larvae_counts"  ~ "larvae_counts 
  #     JOIN tows USING (cruise, ship, orderocc, towtype, townum, netloc)
  #     JOIN stations USING (cruise, ship, orderocc)
  #     LEFT JOIN species_groups USING (spccode)")
  
  q_tbl_geom <- case_when(
    v$tbl %in% c("ctd_bottles", "ctd_dic") ~ "ctd_casts.geom")
  # TODO: incorporate larvae_counts a la [plumber.R](https://github.com/CalCOFI/api/blob/d40c48b08316899c2e84481b4d07feb389fea957/plumber.R#L160-L163)
  # v$tbl == "larvae_counts"               ~ "stations.geom")
  
  pts_sql <- glue("
    WITH
    aoi AS ({aoi_sql})
    SELECT
      {value}({v$fld}) AS {value},
      COUNT({v$fld}) AS n_obs,
      {q_tbl_geom} AS geom
    FROM {q_from}
      JOIN aoi ON ST_Covers(aoi.geom, {q_tbl_geom})
    WHERE 
      ({v$tbl}.depth_m BETWEEN {depth_m_min} AND {depth_m_max}) AND
      (date BETWEEN '{date_beg}' AND '{date_end}') AND
      DATE_PART('quarter', date) IN ({paste(date_qrtr, collapse=',')})
    GROUP BY {q_tbl_geom} ")
  
  st_read(con, query = pts_sql, quiet = T) # mapview::mapView(pts)
}


get_depth_profile_data <- function(
    variable,
    aoi_ewkt = NULL, # for custom drawn AOI
    aoi_keys = c(
      "cc_nearshore-standard",
      "cc_nearshore-extended",
      "cc_offshore-standard",
      "cc_offshore-extended"),  # lookup in db.places (also calcofi4r::cc_places)
    date_beg, date_end, date_qrtr = 1,
    depth_m_min, depth_m_max,
    n_bins = 10){
  
  # variable = "ctd_bottles.t_degc"
  # aoi_ewkt = NULL
  # aoi_keys = c(
  #   "cc_nearshore-standard",
  #   "cc_nearshore-extended",
  #   "cc_offshore-standard",
  #   "cc_offshore-extended")
  # date_beg = "2015-01-25"
  # date_end = "2020-01-26"
  # date_qrtr = 1
  # depth_m_min = 0
  # depth_m_max = 515
  # n_bins = 10
  
  # variable
  v <- get_variable(variable)
  
  # area of interest
  aoi_sql <- get_aoi_sql(aoi_ewkt = aoi_ewkt, aoi_keys = aoi_keys)

  q_from <- case_when(
    v$tbl == "ctd_bottles"    ~ "ctd_casts JOIN ctd_bottles USING (cast_count)",
    v$tbl == "ctd_dic"        ~ "ctd_casts JOIN ctd_bottles USING (cast_count) JOIN ctd_dic USING (btl_cnt)")

  q_tbl_geom <- case_when(
    v$tbl %in% c("ctd_bottles", "ctd_dic") ~ "ctd_casts.geom")

  d_sql <- glue("
    WITH
    aoi AS ({aoi_sql})
    SELECT
      width_bucket(depth_m::int4, {depth_m_min}, {depth_m_max}, {n_bins}) AS depth_bin,
      min(depth_m::int4) AS depth_min,
      avg(depth_m::int4) AS depth_avg,
      max(depth_m::int4) AS depth_max,
      int4range(min(depth_m::int4), max(depth_m::int4), '[]')             AS depth_bin_label,
      AVG({v$fld})    AS v_avg,
      STDDEV({v$fld}) AS v_stddev,
      COUNT({v$fld})  AS v_nobs
    FROM {q_from}
      JOIN aoi ON ST_Covers(aoi.geom, {q_tbl_geom})
    WHERE 
      {v$tbl}.{v$fld} IS NOT NULL AND 
      ({v$tbl}.depth_m BETWEEN {depth_m_min} AND {depth_m_max}) AND
      (date BETWEEN '{date_beg}' AND '{date_end}') AND
      DATE_PART('quarter', date) IN ({paste(date_qrtr, collapse=',')})
    GROUP BY depth_bin")
  # message(d_sql)
  
  d <- dbGetQuery(con, d_sql)
  d
}

plot_depth_profile <- function(d, v, interactive = T){
  librarian::shelf(
    ggplot2, plotly)
  
  p <- d |> 
    mutate(
      txt = glue(
        "{v$plot_label}:
        average: {round(v_avg, 3)}
        +/- stddev: {round(v_stddev, 3)}
        for Depth (m): {depth_bin_label}
        # observations: {format(as.numeric(v_nobs), big.mark=',')}")) |> 
    ggplot() +
    geom_rect(
      aes(
        xmin = v_avg - v_stddev, 
        xmax = v_avg + v_stddev, 
        ymin = depth_min, 
        ymax = depth_max), 
      fill = "gray", color = "gray") +
    geom_line(
      aes(x=v_avg, y=depth_avg),
      color = "red") +
    suppressWarnings(
      geom_point(
        aes(x=v_avg, y=depth_avg, text=txt),
        color = "red")) +
    scale_y_reverse() + 
    theme_minimal() +
    labs(
      x = v$plot_label, y = "Depth (m)")
  
  if (interactive)
    p <- plotly::ggplotly(p, tooltip = "text")
  
  p
}

get_map_data <- function(
    variable, value, 
    aoi_ewkt = NULL, # for custom drawn AOI
    aoi_keys = c(
      "cc_nearshore-standard",
      "cc_nearshore-extended",
      "cc_offshore-standard",
      "cc_offshore-extended"),
    date_beg, date_end, date_qrtr = 1,
    depth_m_min, depth_m_max,
    n_bins = 7, 
    return_type = c(
      "polygons",         "raster",     "points",
      "polygons_geojson", "raster_tif", "points_csv"),
    dir_cache = here::here("cache")){
  
  return_type = return_type[1]
  
  # message("get_map_data() begin ~ {Sys.time()}")
  
  # message(glue("  is.null(aoi_ewkt): {is.null(aoi_ewkt)}"))
  # message(glue("  aoi_keys: {paste(aoi_keys, collapse = ', ')}"))
  
  # variable    = "ctd_bottles.t_degc" # input$sel_var
  # value       = "avg"                # input$sel_val
  # aoi_ewkt     = NULL
  # # aoi_keys    = c("cc_nearshore-standard","cc_nearshore-extended","cc_offshore-standard","cc_offshore-extended")
  # aoi_keys    = c("boem-wpa_NI10-03", "boem-wpa_NK10-01")
  # date_beg    = as.Date(today() - dyears(5))  # input$sel_date_range[1]
  # date_end    = rng_dates$max             # input$sel_date_range[2]
  # date_qrtr   = c(1, 2)
  # depth_m_min = 0                    # input$sel_depth_range[1]
  # depth_m_max = 515                  # input$sel_depth_range[2]
  # n_bins      = 7
  # return_type = "polygons"
  # dir_cache   = here::here("cache")
  
  # * check inputs -----
  stopifnot(is.null(aoi_ewkt) | is.null(aoi_keys))
  
  stopifnot(
    return_type %in% c(
      "polygons", "raster", "points",
      "polygons_geojson", "raster_tif"))
  
  # * define hash ----
  hash <- get_hash(
    variable, value, 
    aoi_ewkt, aoi_keys,
    date_beg, date_end, date_qrtr,
    depth_m_min, depth_m_max,
    n_bins)
  idw_geo <- glue("{dir_cache}/idw_{hash}.geojson")
  idw_tif <- glue("{dir_cache}/idw_{hash}.tif")
  
  # * check if hash exists in idw table ----
  has_idw <- tbl(con, "idw_stats") |> 
    filter(hash == !!hash) |> 
    collect() |> 
    nrow() == 1
  
  # log_info(
  #   list(get_map_data = list(
  #       variable    = variable,
  #       value       = value,
  #       aoi_ewkt    = aoi_ewkt,
  #       aoi_keys    = aoi_keys)))
  
  # loggit(
  #   "INFO",
  #   "get_map_data()",
  #   data_json = jsonlite::toJSON(list(
  #     variable    = variable,
  #     value       = value,
  #     aoi_ewkt    = aoi_ewkt,
  #     aoi_keys    = aoi_keys,
  #     date_beg    = date_beg,
  #     date_end    = date_end,
  #     date_qrtr   = date_qrtr,
  #     depth_m_min = depth_m_min,
  #     depth_m_max = depth_m_max,
  #     n_bins      = n_bins,
  #     return_type = return_type,
  #     dir_cache   = dir_cache,
  #     clientData  = clientData)) |> 
  #     as.character())
  
  # * if exists, update stats and return requested type ----
  if (has_idw & return_type != "points"){

    # update idw_stats
    res <- switch(
      return_type,
      polygons         = update_idw_stats(hash, view     = "ply"),
      polygons_geojson = update_idw_stats(hash, download = "ply"),
      raster           = update_idw_stats(hash, view     = "rast"),
      raster_tif       = update_idw_stats(hash, download = "rast"))
    
    # return output
    # message("get_map_data() exists, end ~ {Sys.time()}")
    o <- switch(
      return_type,
      polygons         = st_read(idw_geo, quiet = T),
      polygons_geojson = idw_geo,
      raster           = rast(idw_tif),
      raster_tif       = idw_tif)
    attr(o, "hash") <- hash
    return(o)
  }
  
  # * otherwise has_idw = F, continue to get points ----
  pts <- get_points(
    variable    = variable, 
    value       = value, 
    aoi_ewkt    = aoi_ewkt,
    aoi_keys    = aoi_keys,
    date_beg    = date_beg, 
    date_end    = date_end, 
    date_qrtr   = date_qrtr,
    depth_m_min = depth_m_min, 
    depth_m_max = depth_m_max)
  
  if (nrow(pts) == 0)
    return(NULL)
  
  if (return_type == "points")
    return(pts)
  
  # * if wanting more than points, continue to create idw products ----
  
  # interpolate points to raster using IDW
  aoi <- get_aoi(aoi_ewkt = aoi_ewkt, aoi_keys = aoi_keys)
  r_idw <- pts_to_rast_idw(pts, value, aoi, out_tif = idw_tif)
  # mapview::mapView(r_idw) # mapview::mapView(aoi)
  
  # generate contour polygons from raster
  plys <- rast_to_contours(r_idw, aoi, n_brks = n_bins + 1)
  # mapview::mapView(plys, zcol="z_avg")
  st_write(plys, idw_geo, quiet = T)
  
  # add row into idw_stats
  # TODO: use idw_stats to run cron job to cleanup of files (idw_geo, idw_tif) not viewed or downloaded
  row_idw_stats <- tibble(
    hash      = hash,
    ply_geo   = idw_geo,
    rast_tif  = idw_tif,
    args      = attr(hash, "args") |> 
      jsonlite::toJSON(auto_unbox = T),
    dtime_created         = Sys.time(),
    dtime_last_viewed     = dtime_created,
    dtime_last_downloaded = NA,
    n_views_ply           = 1L,
    n_views_rast          = 0L,
    n_downloads_ply       = 0L,
    n_downloads_rast      = 0L)
  # initialize table idw:
  #   dbWriteTable(
  #     con, "idw_stats", row_idw_stats, overwrite=T,
  #     field.types = c(
  #       dtime_last_downloaded = "timestamptz",
  #       args                  = "json"))
  #   create_index(con, "idw_stats", "hash", is_unique = T)
  dbWriteTable(con, "idw_stats", row_idw_stats, append=T)

  # message("get_map_data() created, end ~ {Sys.time()}")
  
  # * return_type -----
  o <- switch(
    return_type,
    polygons         = st_read(idw_geo, quiet = T),
    polygons_geojson = idw_geo,
    raster           = rast(idw_tif),
    raster_tif       = idw_tif)
  attr(o, "hash") <- hash
  o

}

get_timeseries_data <- function(
    variable = "ctdcast_bottle.t_deg_c",
    aoi_wkt = NULL,
    depth_m_min = NULL, depth_m_max = NULL,
    date_beg = NULL, date_end = NULL,
    time_step = "year",
    stats = c("p10", "mean", "p90")){
  
  # DEBUG
  # variable = "ctd_bottles.t_degc"
  # aoi_sf <- st_read(con, "aoi_fed_sanctuaries") %>%
  #   filter(nms == "CINMS")
  # aoi_wkt <- aoi_sf %>%
  #   pull(geom) %>%
  #   st_as_text()
  # st_bbox(aoi_sf) %>% st_as_sfc() %>% st_as_text()
  # aoi_wkt <- "POLYGON ((-120.6421 33.36241, -118.9071 33.36241, -118.9071 34.20707, -120.6421 34.20707, -120.6421 33.36241))"
  # date_beg = NULL; date_end = NULL
  # time_step = "year"
  # stats = "p10, mean, p90"
  # depth_m_min = NULL; depth_m_max = NULL
  
  # TODO: 
  
  # check input arguments ----
  
  # variable
  v <- tbl(con, "field_labels") %>% 
    filter(table_field == !!variable) %>% 
    collect() %>% 
    separate(table_field, into=c("tbl", "fld"), sep="\\.", remove=F)
  stopifnot(nrow(v) == 1)
  
  # TODO: is_valid_date(), is_valid_aoi(): wkt, aoi_id
  # if (!is.null(aoi_wkt))
  #   aoi_sf <- st_as_sf(tibble(geom = aoi_wkt), wkt = "geom") %>% 
  #     st_set_crs(4326)  # mapview::mapview(aoi_sf)
  
  # TODO: document DATE_PART() options
  # https://www.postgresql.org/docs/13/functions-datetime.html#FUNCTIONS-DATETIME-EXTRACT
  # is_valid_aoi(aoi)
  # is_valid_date(date_beg)
  # is_valid_date(date_end)
  
  stopifnot(time_step %in% c("decade","year","year_quarter","year_month","year_week","date","quarter","month","week","julianday","hour"))
  # TODO: Describe non- vs climatalogical vars: "quarter","month","week","julianday"
  q_time_step <- switch(
    time_step,
    decade       = "DATE_PART('decade' , date) * 10)",
    year         = "DATE_PART('year'   , date)",
    year_quarter = "DATE_PART('year'   , date) + (DATE_PART('quarter', date) * 0.1 )",
    year_month   = "DATE_PART('year'   , date) + (DATE_PART('month', date)   * 0.01)",
    year_week    = "DATE_PART('year'   , date) + (DATE_PART('week', date)    * 0.01)",
    date         = "date",
    quarter      = "DATE_PART('quarter', date)",
    month        = "DATE_PART('month'  , date)",
    week         = "DATE_PART('week'   , date)",
    julianday    = "DATE_PART('doy'    , date)",
    hour         = "DATE_PART('hour'   , datetime)")
  if (is.null(q_time_step))
    q_time_step = "datetime"
  
  q_from <- case_when(
    v$tbl == 'ctd_bottles'     ~ "ctd_casts JOIN ctd_bottles USING (cast_count)",
    v$tbl == 'ctd_bottles_dic' ~ "ctd_casts JOIN ctd_bottles USING (cast_count) JOIN ctd_dic USING (btl_cnt)")
  
  q_where_aoi = ifelse(
    !is.null(aoi_wkt),
    glue("ST_Intersects(ST_GeomFromText('{aoi_wkt}', 4326), ctd_casts.geom)"),
    "TRUE")
  
  q_where_date = glue("date >= '{date_beg}' AND date <= '{date_end}'")
  
  q_where_depth = glue("depth_m >= {depth_m_min} AND depth_m <= {depth_m_max}")
  
  # TODO: get median, percentile ----
  # https://leafo.net/guides/postgresql-calculating-percentile.html
  # https://www.postgresql.org/docs/9.4/functions-aggregate.html
  
  # TODO: incorporate different stats besides mean & stddev, which are temporarily hard-coded
  q <- glue(
    "SELECT {q_time_step} AS {time_step}, AVG({v$tbl}.{v$fld}) AS {v$fld}_avg, STDDEV({v$tbl}.{v$fld}) AS {v$fld}_sd, COUNT(*) AS n_obs
    FROM {q_from}
    WHERE {q_where_aoi} AND {q_where_date} AND {q_where_depth}
    GROUP BY {q_time_step} 
    ORDER BY {time_step}")
  # message(q)
  d <- dbGetQuery(con, q)
  
  # TODO: add attributes like Cristina's original function  
  # attr(d_aoi_summ, "labels")    <- eval(parse(text = glue("var_lookup$`{var}`")))
  # attr(d_aoi_summ, "time_step") <- time_step
  # attr(d_aoi_summ, "date_msg")  <- glue("This dataset was summarized by {time_step}.")
  # attr(d_aoi_summ, "aoi") <- ifelse(
  #   empty_data_for_var,
  #   glue("No data were found for {var} in this area of interest. Summaries were conducted across all existing data points."),
  #   glue("Data for {var} in selected area of interest")
  # )
  
  d
}

map_base <- function(
    base_opacity       = 0.5, 
    zoomControl        = T,
    attributionControl = F){
  
  leaflet(options = leafletOptions(
    zoomControl        = zoomControl,
    attributionControl = attributionControl)) |> 
    # add base: blue bathymetry and light brown/green topography
    addProviderTiles(
      "Esri.OceanBasemap",
      options = providerTileOptions(
        variant = "Ocean/World_Ocean_Base",
        opacity = base_opacity),
      group = "OceanBase") |>
    # add reference: placename labels and borders
    addProviderTiles(
      "Esri.OceanBasemap",
      options = providerTileOptions(
        variant = "Ocean/World_Ocean_Reference",
        opacity = base_opacity),
      group = "OceanLabels")
}

add_contours <- function(m, p, title, add_lyrs_ctrl = T){
  
  qbins <- sort(unique(c(p$z_min, p$z_max)))
  qpal <- colorBin("Spectral", bins = qbins, reverse = T)
  
  m <- m |> 
    clearGroup("contours") |> 
    clearControls() |> 
    addPolygons(
      data = p, fillOpacity = 0.5, weight = 2, opacity = 0.7,
      color = ~qpal(z_avg), group = "contours") |> 
    addLegend(
      pal = qpal, values = p$z_avg, opacity = 1, 
      title = title, group = "contours")
  if (add_lyrs_ctrl)
    m <- m |> 
    addLayersControl(
      baseGroups = c("OceanBase", "OceanLabels"),
      overlayGroups = c("aoi", "contours"),
      options = layersControlOptions(collapsed = T))
  m
}

# DEBUG functions for scripts:explore_pg_contour2.qmd ----
interp_rast <- function(algorithm){
  r <- do.call(
    get_contour,
    c(contour_args, list(
      return_type   = "raster",
      idw_algorithm = algorithm)))
  map_rp(r, p)
}

map_rp <- function(r,p){
  rng_r <- global(r, "range", na.rm=T) |> as.numeric()
  rng_p <- range(p$z)
  rng <- range(c(rng_r, rng_p))
  
  pal <- colorNumeric("Spectral", rng, na.color = NA)
  
  map_base(base_opacity = 0.2) |> 
    addRasterImage(
      r, colors = pal, opacity=0.7, group = "raster") |>
    addLegend(
      pal = pal, values = rng) |> 
    addCircleMarkers(
      data   = p,
      radius = 5, fillOpacity = 0.2,
      stroke = T, weight = 1, opacity = 0.8,
      color  = ~pal(z), group = "points") |> 
    addLayersControl(
      # baseGroups    = c("OceanBase", "OceanLabels"),
      overlayGroups = c("raster", "points"),
      options       = layersControlOptions(collapsed = T),
      position      = "topleft") |> 
    hideGroup("points")
}

map_rpk <- function(r, p, k){
  rng_r <- global(r, "range", na.rm=T) |> as.numeric()
  rng_p <- range(p$z)
  rng_k <- range(c(k$val_min, k$val_max))
  rng <- range(c(rng_r, rng_p, rng_k))
  
  pal <- colorNumeric("Spectral", rng, na.color = NA)
  
  qbins <- sort(unique(c(k$val_min, k$val_max)))
  message(glue("k$val_avg: {paste(sort(unique(c(k$val_min, k$val_max))), collapse=', ')}"))
  message(glue("    qbins: {paste(qbins, collapse=', ')}", .trim = F))
  qpal <- colorBin("Spectral", bins = qbins, reverse = T)
  
  map_base(base_opacity = 0.2) |> 
    addRasterImage(
      r, colors = pal, opacity=0.7, group = "raster") |>
    addLegend(
      pal = pal, values = rng, title="raster") |> 
    addCircleMarkers(
      data   = p,
      radius = 5, fillOpacity = 0.2,
      stroke = T, weight = 1, opacity = 0.8,
      color  = ~pal(z), group = "points") |> 
    addPolygons(
      data = p, fillOpacity = 0.5, weight = 2, opacity = 0.7,
      color = ~qpal(val_avg), group = "contours") |> 
    addLegend(
      pal = qpal, values = p$val_avg, opacity = 1, 
      title = "contours", group = "contours") |> 
    addLayersControl(
      # baseGroups    = c("OceanBase", "OceanLabels"),
      overlayGroups = c("raster", "points","contours"),
      options       = layersControlOptions(collapsed = T),
      position      = "topleft") |> 
    hideGroup(c("points","raster"))
}


# p <- get_contour(
#   variable = "ctd_bottles.t_degc",
#   value = "avg",
#   depth_m_min = 0,
#   depth_m_max = 5351,
#   date_beg = "1949-02-28",
#   date_end = "2020-01-26")
# mapview::mapView(p, zcol = "val_avg")

options(digits=12) # Part of the problem is that your default digits = 7,
#   so you won't see any digits beyond that.
#  If you put too many (e.g, 22), you'll get floating point imprecision
#
#  Note the behavior for rounding off a 5 under ?round:
#  "IEC 60559 standard is expected to be used, ‘go to the even digit’."
#
# digits needed to distinguish between two nonequal elements
diff.dec.places <- function(x, y){
  delta <- abs(x - y)
  dec.place <- -ceiling(log10(abs(delta))) # your idea here was correct
  # print(paste0("The elements (",x," & ",y,") differ in the ",
  #              10^-dec.place,"'s place."))
  if(round(x, dec.place)==round(y, dec.place)){
    #print("But we add another sig. figure so they do not round to the same number.")
    dec.place <- dec.place + 1
  }
  #print(paste("The elements will round to",round(x, dec.place),'&',round(y, dec.place)))
  dec.place
}
#
discr.round <- function(x){
  #- Find minimum-magnitude difference and which elements possess it:
  #-   Create upper triangle of difference matrix of elements of vector with itself
  d <- abs(outer(x,x,"-"))
  d[lower.tri(d, diag=T)] <- NA
  #-   Return the smallest-magnitude difference and indices of elements forming it
  m <- min(d, na.rm=T)
  if(m != 0){
    #- Round to number of dec places required to distinguish the closest elements
    e <- x[which(d==m, arr.ind=T)]
    round(x, diff.dec.places(e[1],e[2]))
  }
  else{
    #print("Closest elements are equal.")
    x
  }
}
# message(glue("z, discr.round(z$value): {paste(discr.round(z$value), collapse=', ')}"))
# z$value <- discr.round(z$value)