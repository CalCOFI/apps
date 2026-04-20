ui <- page_sidebar(
  window_title = "CalCOFI CTD Visualization",
  title = tagList(
    span(
      a(
        img(src = "logo_calcofi.svg", height = "50px", .noWS = "after"),
        href = "https://calcofi.io"),
      "CTD Viz")),

  # sidebar ----
  sidebar = sidebar(
    width = 300,

    selectInput(
      "sel_cruise", "Cruise",
      choices  = cruise_vec,
      selected = default_cruise),
    actionButton(
      "btn_load_cruise", "Load Cruise",
      width = "100%", class = "btn-primary mb-2"),
    hr(),

    # transect selection info
    htmlOutput("txt_selection_info"),
    actionButton(
      "btn_reset_selection", "Reset Selection",
      width = "100%", class = "btn-secondary mb-2"),
    hr(),

    # transect plot controls (visible when Transect tab active)
    conditionalPanel(
      "input.tabs === 'Transect'",
      selectInput(
        "sel_meas_type", "Measurement",
        choices  = meas_vec,
        selected = default_meas),
      selectInput(
        "sel_cast_dir", "Cast Direction",
        choices  = c("Downcast (D)" = "D", "Upcast (U)" = "U"),
        selected = "D"),
      sliderInput(
        "sl_max_depth", "Max Depth (m)",
        min = 0, max = 5000, value = 500, step = 50),
      numericInput(
        "num_interp_n", "Grid Resolution",
        value = 100, min = 50, max = 500, step = 50),
      checkboxInput(
        "chk_bathy", "Show bathymetry", value = TRUE),
      actionButton(
        "btn_plot", "Plot Transect",
        width = "100%", class = "btn-primary"))),

  # main content ----
  navset_card_underline(
    id     = "tabs",
    height = "100%",

    nav_panel(
      "Cruise Map",
      div(
        style = "display: flex; flex-direction: column; height: 100%;",
        div(
          style = "flex: 1; min-height: 400px;",
          maplibreOutput("map_cruise", height = "100%")),
        div(
          style = "padding: 8px 12px;",
          sliderInput(
            "sl_time_range", "Time Range",
            min        = as.POSIXct("2020-01-01", tz = "UTC"),
            max        = as.POSIXct("2020-12-31", tz = "UTC"),
            value      = c(
              as.POSIXct("2020-01-01", tz = "UTC"),
              as.POSIXct("2020-12-31", tz = "UTC")),
            width      = "100%",
            timeFormat = "%Y-%m-%d %H:%M",
            timezone   = "+0000")))),

    nav_panel(
      "Transect",
      plotlyOutput("plot_transect", height = "100%")))
)
