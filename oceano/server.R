shinyServer(function(input, output, session) {
  
  rxvals <- reactiveValues(
    hash            = NULL,
    map_init        = F,
    tbl_places_init = F,
    aoi_cat  = aoi_cat_init, 
    aoi_keys = aoi_keys_init,
    aoi_ewkt = NULL,
    aoi_rows = aoi_rows_init,
    mapedit_counter = 1)
  
  # bookmarking ----

  # Save extra values in state$values when we bookmark
  onBookmark(function(state) {
    state$values$hash            <- rxvals$hash
    state$values$map_init        <- rxvals$map_init
    state$values$tbl_places_init <- rxvals$tbl_places_init
    state$values$aoi_cat         <- rxvals$aoi_cat
    state$values$aoi_keys        <- rxvals$aoi_keys
    state$values$aoi_ewkt        <- rxvals$aoi_ewkt
    state$values$aoi_rows        <- rxvals$aoi_rows
    state$values$mapedit_counter <- rxvals$mapedit_counter
  })
  
  # Read values from state$values when we restore
  onRestore(function(state) {
    rxvals$hash            <- state$values$hash
    rxvals$map_init        <- state$values$map_init
    rxvals$tbl_places_init <- state$values$tbl_places_init
    rxvals$aoi_cat         <- state$values$aoi_cat
    rxvals$aoi_keys        <- state$values$aoi_keys
    rxvals$aoi_ewkt        <- state$values$aoi_ewkt
    rxvals$aoi_rows        <- state$values$aoi_rows
    rxvals$mapedit_counter <- state$values$mapedit_counter
  })
  
  setBookmarkExclude(c(
    "_values_",
    "btn_aoi",
    "btn_update",
    "map_bounds",
    "map_groups",
    "map_places_bounds",
    "map_places_center",
    "map_places_zoom",
    "map_shape_mouseout",
    "map_shape_mouseover",
    "map_side_bounds",
    "map_side_center",
    "map_side_groups",
    "map_side_zoom",
    "mapedit_bounds",
    "mapedit_click",
    "mapedit_draw_start",
    "mapedit_draw_stop",
    "mapedit_draw_all_features",
    "mapedit_draw_new_feature",
    "mapedit_groups",
    "mapedit_shape_mouseout",
    "mapedit_shape_mouseover",
    "mapedit_zoom",
    "mapedit_center",
    "sidebarCollapsed",
    "sidebarItemExpanded",
    "tbl_places_cell_clicked",
    "tbl_places_cells_selected",
    "tbl_places_columns_selected",
    "tbl_places_init",
    "tbl_places_rows_all",
    "tbl_places_rows_current",
    "tbl_places_rows_selected",
    "tbl_places_search",
    "tbl_places_state"))
  
  # * update url with each input change ----
  
  # ui.R: commented out `bookmarkButton()` since updating here
  
  observe({
    # trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  # map ----
  output$map <- renderLeaflet({

    plys <- isolate(get_map_data(
      variable      = input$sel_var,
      value         = input$sel_val,
      aoi_keys      = rxvals$aoi_keys,
      aoi_ewkt      = rxvals$aoi_ewkt, 
      depth_m_min   = input$sel_depth_range[1],
      depth_m_max   = input$sel_depth_range[2],
      date_qrtr     = input$sel_qtr,
      date_beg      = input$sel_date_range[1],
      date_end      = input$sel_date_range[2],
      return_type   = "polygons",
      dir_cache     = dir_cache))
    
    rxvals$hash <- attr(plys, "hash")
    
    title = d_vars |> 
      filter(table_field == isolate(input$sel_var)) |> 
      pull(plot_label) |> 
      paste(glue("<br>{isolate(input$sel_val)}"))
    
    rxvals$map_init <- T
    
    map_base() |>
      addCalcofiStations() |> 
      add_contours(plys, title)
  })
  
  # map_side ----
  output$map_side <- renderLeaflet({
    map_base(zoomControl = F) |> 
      setView(-122.4, 33.8, 4)
  })
  
  # observeEvent btn_update ----
  observeEvent(input$btn_update, {
    # message(glue("observeEvent btn_update beg ~ {Sys.time()}"))

    plys <- get_map_data(
      variable      = input$sel_var,
      value         = input$sel_val,
      aoi_keys      = rxvals$aoi_keys,
      aoi_ewkt      = rxvals$aoi_ewkt, 
      depth_m_min   = input$sel_depth_range[1],
      depth_m_max   = input$sel_depth_range[2],
      date_qrtr     = input$sel_qtr,
      date_beg      = input$sel_date_range[1],
      date_end      = input$sel_date_range[2],
      return_type   = "polygons",
      dir_cache     = dir_cache)
    
    rxvals$hash <- attr(plys, "hash")
    
    # loggit(
    #   "INFO",
    #   "session$clientData$url_hostname",
    #   clientData = session$clientData$url_hostname)
    
    if (is.null(plys)){
      showNotification(
        "Sorry, no observations were found. Please try expanding your query in time and/or space.",
        duration = NULL, closeButton = T, type = "error")
    } else {
      title = d_vars |> 
        filter(table_field == input$sel_var) |> 
        pull(plot_label) |> 
        paste(glue("<br>{input$sel_val}"))
      
      leafletProxy("map") |>
        add_contours(plys, title)
    }
    
    # message(glue("observeEvent btn_update end ~ {Sys.time()}"))
  })
  
  # observe aoi ----
  observe({
    req(rxvals$map_init)
    
    # message(glue("observe aoi - rxvals$aoi_keys: {paste(rxvals$aoi_keys, collapse =',')}"))
    # message(glue("observe aoi ~ {Sys.time()}"))
    #browser()
    
    aoi <- get_aoi(
      aoi_keys = rxvals$aoi_keys,
      aoi_ewkt = rxvals$aoi_ewkt)
    
    b <- st_bbox(aoi) |> as.numeric()
    
    leafletProxy("map") |>
      clearGroup("aoi") |> 
      addPolygons(
        data = aoi, group = "aoi",
        fill = F,
        color = "black") |> 
      fitBounds(b[1], b[2], b[3], b[4])
    
    leafletProxy("map_side") |>
      clearGroup("aoi") |> 
      addPolygons(
        data = aoi, group = "aoi",
        fill = T,
        color = "black") # |> 
      # fitBounds(b[1], b[2], b[3], b[4])
    
    leafletProxy("mapedit") |>
      clearGroup("aoi_edit") |>
      addPolygons(
        data = aoi, group = "aoi_edit")
  })
  
  get_tbl_places <- reactive({
    cc_places |> 
      st_drop_geometry() |> 
      filter(category == input$sel_place_category)
  })
  
  # tbl_places ----
  output$tbl_places <- renderDataTable({
    rxvals$tbl_places_init = T
    get_tbl_places() |> 
      select(name)
  },
  options   = list(pageLength = 6)  #,
  # server    = FALSE,
  # selection = list(mode = 'multiple') 
  )
  
  # observeEvent tbl_places_rows_selected ----
  observeEvent(input$tbl_places_rows_selected,{
    # message("observeEvent(input$tbl_places_rows_selected")
    # message(glue("observeEvent tbl_places_rows_selected, beg - rxvals$aoi_keys: {paste(rxvals$aoi_keys, collapse =',')}"))
    
    # browser()
    rxvals$aoi_keys <- get_tbl_places() |> 
      slice(input$tbl_places_rows_selected) |> 
      pull(key)
    
    rxvals$aoi_rows <- input$tbl_places_rows_selected
    
    # message(glue("observeEvent tbl_places_rows_selected, end - rxvals$aoi_keys: {paste(rxvals$aoi_keys, collapse =',')}"))
    # message(glue("observeEvent tbl_places_rows_selected, end - rxvals$aoi_keys: {paste(rxvals$aoi_keys, collapse =',')}"))
  })
  
  # map_places ----
  output$map_places <- renderLeaflet({
    req(input$sel_place_category)
    req(rxvals$tbl_places_init)

    # rxvals <- list(aoi_keys =  c(
    #   "cc_nearshore-extended",
    #   "cc_offshore-extended",
    #   "cc_nearshore-standard",
    #   "cc_offshore-standard"))
    
    #browser()
    # message(glue("map_places, beg - rxvals$aoi_keys: {paste(rxvals$aoi_keys, collapse =',')}"))
    
    plys <- cc_places |> 
      filter(
        category == input$sel_place_category)
    plys_aoi <- cc_places |> 
      filter(
        key %in% rxvals$aoi_keys)
    
    dataTableProxy("tbl_places") |> 
      selectRows(rxvals$aoi_rows)
    
    map_base()  |> 
      addCalcofiStations() |> 
      addPolygons(
        data = plys, layerId = ~key) |> 
      addPolygons(
        data = plys_aoi, layerId = ~key,
        group = "plys_aoi", color = "yellow")
  })
  
  # dl_map_csv: download points link ----
  output$dl_map_csv <- downloadHandler(
    filename = function() {
      glue("calcofi_map-points_{input$sel_var}.csv")
    },
    content = function(file) {
      pts <- isolate(get_map_data(
        variable      = input$sel_var,
        value         = input$sel_val,
        aoi_keys      = rxvals$aoi_keys,
        aoi_ewkt      = rxvals$aoi_ewkt, 
        depth_m_min   = input$sel_depth_range[1],
        depth_m_max   = input$sel_depth_range[2],
        date_qrtr     = input$sel_qtr,
        date_beg      = input$sel_date_range[1],
        date_end      = input$sel_date_range[2],
        return_type   = "points",
        dir_cache     = dir_cache))
    
      pts |> 
        mutate(
          longitude = st_coordinates(st_geometry(pts))[,"X"],
          latitude = st_coordinates(st_geometry(pts))[,"Y"]) |> 
        st_drop_geometry() |> 
        write_csv(file)
    }
  )
  
  # dl_map_png: download image link ----
  output$dl_map_png <- downloadHandler(
    filename = function() {
      glue("calcofi_map-image_{input$sel_var}.png")
    },
    content = function(file) {
      
      plys <- isolate(get_map_data(
        variable      = input$sel_var,
        value         = input$sel_val,
        aoi_keys      = rxvals$aoi_keys,
        aoi_ewkt      = rxvals$aoi_ewkt, 
        depth_m_min   = input$sel_depth_range[1],
        depth_m_max   = input$sel_depth_range[2],
        date_qrtr     = input$sel_qtr,
        date_beg      = input$sel_date_range[1],
        date_end      = input$sel_date_range[2],
        return_type   = "polygons",
        dir_cache     = dir_cache))
      
      rxvals$hash <- attr(plys, "hash")
      
      title = d_vars |> 
        filter(table_field == isolate(input$sel_var)) |> 
        pull(plot_label) |> 
        paste(glue("<br>{isolate(input$sel_val)}"))
      
      ctr <- input$map_center
      m <- map_base(
        zoomControl        = F,
        attributionControl = F) |>
        add_contours(plys, title, add_lyrs_ctrl = F) |> 
        setView(ctr$lng, ctr$lat, input$map_zoom)
      m
      
      htm <- tempfile(fileext = ".html")
      m |> saveWidget(file = htm)
      htm |> webshot(file = file, delay = 2)
    },
    contentType	= "image/png"
  )
  
  # dl_map_other: download links ----
  output$dl_map_other <- renderUI({
    req(rxvals$hash)
    
    url_rast <- glue("{url_cache}/idw_{rxvals$hash}.tif")
    url_plys <- glue("{url_cache}/idw_{rxvals$hash}.geojson")
    
    tagList(
      a("raster.tif", href = url_rast, target="_blank"),
      ", ",
      a("polygons.geojson", href = url_plys, target="_blank"))
  })
  
  # observeEvent map_places_shape_click ----
  observeEvent(input$map_places_shape_click,{
    # message("observeEvent(input$map_places_shape_click")
    # message(glue("observeEvent map_places_shape_click, beg - rxvals$aoi_keys: {paste(rxvals$aoi_keys, collapse =',')}"))
    
    key_click <- input$map_places_shape_click$id
    if (key_click %in% rxvals$aoi_keys){
      rxvals$aoi_keys <- setdiff(rxvals$aoi_keys, key_click)
    } else {
      rxvals$aoi_keys <- c(rxvals$aoi_keys, key_click)
    }
    
    rxvals$aoi_rows <- which(isolate(get_tbl_places()$key) %in% rxvals$aoi_keys)
    
    dataTableProxy("tbl_places") |> 
      selectRows(rxvals$aoi_rows)
    
    # message(glue("observeEvent map_places_shape_click, end - rxvals$aoi_keys: {paste(rxvals$aoi_keys, collapse =',')}"))
    # message(glue("observeEvent map_places_shape_click, end - rxvals$aoi_rows: {paste(rxvals$aoi_rows, collapse =',')}"))
  })
  
  # observeEvent sel_place_category ----
  observeEvent(input$sel_place_category,{
    # message("observeEvent(input$sel_place_category")
    # message(glue("observeEvent sel_place_category, beg - rxvals$aoi_keys: {paste(rxvals$aoi_keys, collapse =',')}"))
    
    leafletProxy("map_places") |> 
      clearGroup("plys_aoi")
    dataTableProxy("tbl_places") |> 
      selectRows(NULL)
    
    # browser()
    if (input$sel_place_category == rxvals$aoi_cat){
      rxvals$aoi_rows <- which(isolate(get_tbl_places()$key) %in% rxvals$aoi_keys)
    } else {
      rxvals$aoi_rows = NULL
    }
  })
  
  # observeEvent btn_aoi -> modal ----
  observeEvent(input$btn_aoi, {
    
    # kick mapedit to regenerate
    rxvals$mapedit_counter = rxvals$mapedit_counter + 1
    
    # message(glue("observeEvent btn_aoi, beg - rxvals$aoi_keys: {paste(rxvals$aoi_keys, collapse =',')}"))
    
    showModal(modalDialog(
      title = "Modify Area",
      tabsetPanel(
        tabPanel(
          "Existing",
            "Select an existing Area of interest.",
            selectInput(
              "sel_place_category",
              "Category",
              cc_places$category |> unique() |> sort(),
              rxvals$aoi_cat),
          fluidRow(
            column(
              6,
              leafletOutput("map_places") ),
            column(
              6,
              dataTableOutput("tbl_places") ) ) ),
        tabPanel(
          "Custom",
          "Draw your own Area of interest using the tools on left of the map.",
          leafletOutput("mapedit"))
      ),
      footer    = modalButton("Close"),
      easyClose = T,
      size      = "l"))
    
  })
  
  # mapedit ----
  output$mapedit <- renderLeaflet({
    library(leaflet.extras)
    
    # message(glue("mapedit {rxvals$mapedit_counter} renderLeaflet() ~ {Sys.time()}"))
    
    aoi <- get_aoi(
      aoi_keys = rxvals$aoi_keys,
      aoi_ewkt = rxvals$aoi_ewkt)
  
    m <- map_base() |>
      addCalcofiStations() |>
      addPolygons(data = aoi, group = "aoi_edit") |> 
      addDrawToolbar(
        targetGroup = "aoi_edit",
        editOptions = editToolbarOptions(
          edit   = T,
          remove = T),
        circleOptions       = F,
        circleMarkerOptions = F,
        markerOptions       = F,
        polylineOptions     = F,
        singleFeature       = T)
  
    m
  })
  
  # observe mapedit_draw ----
  observe({
    # use the draw_stop event to detect when users finished drawing
    #   https://github.com/bhaskarvk/leaflet.extras/blob/master/inst/examples/shiny/draw-events/app.R
    
    req(input$mapedit_draw_all_features)
    
    # message(glue("observe mapedit_draw ~ {Sys.time()}"))
    # message(glue("observe mapedit_draw - rxvals$aoi_keys: {paste(rxvals$aoi_keys, collapse =',')}"))
    
    # get drawn feature
    feature <- input$mapedit_draw_all_features$features[[1]]
    aoi_ewkt <- feature$geometry |> 
      geojsonio::as.json() |> 
      st_read(quiet = T) |> 
      st_geometry() |> 
      st_as_text(EWKT=T)
    
    # update aoi_ewkt to be clipped by land
    aoi <- isolate(get_aoi(
      aoi_keys = NULL,
      aoi_ewkt = aoi_ewkt))
    rxvals$aoi_ewkt <- aoi |> 
      st_geometry(aoi) |> 
      st_as_text(EWKT=T)
    rxvals$aoi_keys <- NULL
    rxvals$aoi_rows <- NULL
    
    # message(glue("observe mapedit_draw, almost end - input$tbl_places_rows_selected: {paste(input$tbl_places_rows_selected, collapse = ',')}"))
    # browser()
    dataTableProxy("tbl_places") |>
      selectRows(NULL)
    leafletProxy("map_places") |>
      clearGroup("plys_aoi")
    
    # message(glue("observe mapedit_draw, end - input$tbl_places_rows_selected: {paste(input$tbl_places_rows_selected, collapse = ',')}"))
    
  })
  
  # get_timeseries_data() ----
  get_ts_data <- reactive({
    
    # handle special inputs
    input_stats <- c("mean", "sd")
    # TODO: translate input$sel_stats
    
    # get aoi_wkt
    if (!is.null(rxvals$aoi_ewkt)){
      aoi_wkt <- rxvals$aoi_ewkt |> 
        str_replace("SRID=.*;", "")
    } else {
      aoi <- get_aoi(
        aoi_keys = rxvals$aoi_keys,
        aoi_ewkt = NULL)
      aoi_wkt <- st_as_text(st_geometry(aoi))
    }
    
    get_timeseries_data(
      variable    = input$sel_var,
      aoi_wkt     = aoi_wkt,
      depth_m_min = input$sel_depth_range[1], 
      depth_m_max = input$sel_depth_range[2],
      date_beg    = input$sel_date_range[1], 
      date_end    = input$sel_date_range[2], 
      time_step   = input$sel_time_step,
      stats       = input_stats)
    
  })
  
  # plot_ts ----
  output$plot_ts <- renderDygraph({
    
    d <- get_ts_data()
    
    v <- d_vars %>% 
      filter(table_field == input$sel_var)
    
    names(d) <- c("time", "avg", "sd", "n")
    d$lwr <- d$avg - d$sd
    d$upr <- d$avg + d$sd
    
    p <- d %>% 
      select(time, avg, lwr, upr) %>% 
      dygraph(main = v$plot_title) %>%
      dySeries(
        c("lwr", "avg", "upr"), 
        label = v$plot_label, color = v$plot_color) # %>%
    # dyOptions(drawGrid = input$showgrid)
    
    if (input$sel_time_step == "year") 
      p <- p |> 
        dyAxis(
          name="x",
          axisLabelFormatter = "function(x){ return parseInt(x) }")
  
    p
  })
  
  # * dl_ts_csv: download timeseries csv ----
  output$dl_ts_csv <- downloadHandler(
    filename = function() {
      glue("calcofi_time-series_{input$sel_var}.csv")
    },
    content = function(file) {
      write_csv(get_ts_data(), file)
    }
  )
  
  # dl_ts_png: download time series image link ----
  output$dl_ts_png <- downloadHandler(
    filename = function() {
      glue("calcofi_timeseries-image_{input$sel_var}.png")
    },
    content = function(file) {
      
      d <- isolate(get_ts_data())
      
      v <- d_vars %>% 
        filter(table_field == isolate(input$sel_var))
      
      names(d) <- c("time", "avg", "sd", "n")
      d$lwr <- d$avg - d$sd
      d$upr <- d$avg + d$sd
      
      p <- d %>% 
        select(time, avg, lwr, upr) %>% 
        dygraph(main = v$plot_title) %>%
        dySeries(
          c("lwr", "avg", "upr"), 
          label = v$plot_label, color = v$plot_color)
      # TODO: get xy extent of dygraph
      
      htm <- tempfile(fileext = ".html")
      p |> saveWidget(file = htm)
      htm |> webshot(file = file, delay = 2)
    },
    contentType	= "image/png"
  )
  
  
  # get_depth_data() ----
  get_depth_data <- reactive({
    
    get_depth_profile_data(
      variable      = input$sel_var,
      aoi_keys      = rxvals$aoi_keys,
      aoi_ewkt      = rxvals$aoi_ewkt, 
      depth_m_min   = input$sel_depth_range[1],
      depth_m_max   = input$sel_depth_range[2],
      date_qrtr     = input$sel_qtr,
      date_beg      = input$sel_date_range[1],
      date_end      = input$sel_date_range[2])
  })
  
  # * dl_depth_csv: download depth csv ----
  output$dl_depth_csv <- downloadHandler(
    filename = function() {
      glue("calcofi_depth-profile_{input$sel_var}.csv")
    },
    content = function(file) {
      write_csv(get_depth_data(), file)
    }
  )
  
  # plot_depth ----
  output$plot_depth <- renderPlotly({

    d <- get_depth_data()
    
    v <- d_vars %>% 
      filter(table_field == input$sel_var)
    
    plot_depth_profile(d, v, interactive = T)
  })
  
  # dl_depth_png: download depth profile image link ----
  output$dl_depth_png <- downloadHandler(
    filename = function() {
      glue("calcofi_depth-image_{input$sel_var}.png")
    },
    content = function(file) {
      
      d <- isolate(get_depth_data())
      
      v <- d_vars %>% 
        filter(table_field == input$sel_var)
      
      p <- plot_depth_profile(d, v, interactive = T)
      # TODO: get xy extent of plotly
      
      htm <- tempfile(fileext = ".html")
      p |> saveWidget(file = htm)
      htm |> webshot(file = file, delay = 2)
    },
    contentType	= "image/png"
  )
  
  # observe tab ----
  observeEvent(input$tabs, {
    
    # message(glue("tab:", input$tabs))
    switch(
      input$tabs,
      map   = {enable( "sel_val");  enable("sel_qtr")},
      time  = {disable("sel_val"); disable("sel_qtr")},
      depth = {disable("sel_val");  enable("sel_qtr")})

  })
  
})
