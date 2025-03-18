ui <- function(request) {
  dashboardPage(
  title = "Oceano App | CalCOFI.io",
  dashboardHeader(
    title = tagList(
      a(
        img(src = "./logo_calcofi.svg", height="50px"),
        href = "https://calcofi.io"),
      "Oceano App") ),
  
  dashboardSidebar(
    
    # bookmarkButton(),
    
    selectInput(
      "sel_var",
      "Variable",
      setNames(
        d_vars$table_field,
        d_vars$plot_label),
      "ctd_bottles.t_degc"),
    
    sliderInput(
      "sel_depth_range",
      "Depth(m) range",
      min = 0, 
      max = rng_depths$max,
      step = 10,
      value = c(0, 515)),
    
    sel_qtr <- selectInput(
      "sel_qtr",
      "Season",
      c(Winter = 1, 
        Spring = 2,
        Summer = 3,
        Fall   = 4),
      selected = 1:2,
      multiple = T),
    
    dateRangeInput(
      "sel_date_range",
      "Date range",
      min   = rng_dates$min,
      max   = rng_dates$max,
      start = rng_dates$min,  # as.Date(rng_dates$max - dyears(5)),
      end   = rng_dates$max,
      startview = "year"),
    
    hr(), 
    
    actionButton(
      "btn_update", strong("Update"), icon = icon("rotate"), width = "200px"),
    
    hr(), 
    
    div(
      class="shiny-input-container",
      leafletOutput("map_side", height = "150px")),
    actionButton(
      "btn_aoi", "Area", icon = icon("edit"), width = "200px")
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(tags$link(rel="stylesheet", type="text/css", href="styles.css")),
    tabsetPanel(
      id = "tabs",
      tabPanel(
        title = "Map", value = "map",
        selectInput(
          "sel_val",
          "Value",
          c("avg", "min", "max", "stddev")),
        # TODO: try PERCENTILE_CONT(0.5) for median and other, eg p10 or p90
        #   * [How to Calculate Median in PostgreSQL - Ubiq BI](https://ubiq.co/database-blog/calculate-median-postgresql/)
        #   * [PostgreSQL: Documentation: 15: 9.21. Aggregate Functions](https://www.postgresql.org/docs/15/functions-aggregate.html#FUNCTIONS-ORDEREDSET-TABLE)
        leafletOutput("map", width="100%", height="70vh"),
        "Download: ",
        downloadLink("dl_map_png", "image.png"), ", ",
        downloadLink("dl_map_csv", "points.csv"), ", ",
        div(style="display: inline-block; vertical-align:top;",
          uiOutput("dl_map_other") )
      ),
      
      tabPanel(
        title = "Time series", value = "time",
        selectInput(
          "sel_time_step",
          "Temporal resolution",
          c("decade",
            "year",
            "year_quarter","year_month","year_week",
            "date",
            # climatic
            "quarter","month","week","julianday","hour"),
          "year"),
        dygraphOutput("plot_ts", width="100%", height="70vh"),
        "Download: ",
        downloadLink("dl_ts_png", "image.png"), ", ",
        downloadLink("dl_ts_csv", "table.csv") ),

      tabPanel(
        title = "Depth profile", value = "depth",
        numericInput(
          "num_depth_bins",
          "Depth bins",
          10),
        plotly::plotlyOutput("plot_depth", width="100%", height="70vh"),
        "Download: ",
        downloadLink("dl_depth_png", "image.png"), ", ",
        downloadLink("dl_depth_csv", "table.csv") )
    ))
)
}