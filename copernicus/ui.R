page_sidebar(
  theme   = bs_theme(
    bootswatch = "darkly",
    navbar_bg  = "#375a7f") |>
    bs_add_rules(
      list(
        "#sld_depth-label.control-label { margin-bottom: 1.5rem !important }",
        ".irs-grid-text, .irs-single, .irs-min, .irs-max  { font-size: 1rem !important }",
        ".selectize-input { font-size: 1rem !important }",
        ".control-label { font-size: 1.3rem !important }")),
  
  title   = "Copernicus + CalCOFI",
  
  sidebar = sidebar(
    selectInput(
      "sel_var", "Variable",
      choices  = vars,
      selected = var_default) ),

  tags$head(includeHTML("google-analytics.html")),

    leafletOutput("map"),
  
    # chooseSliderSkin("Square"), # "Shiny", "Flat", "Big", "Modern", "Sharp", "Round", "Square"
    absolutePanel(
      id     = "pnl_time",
      bottom = "5%", left = "26%", right = "4%",
      width  = "70%",
      
      sliderInput(
        "sld_time", "Time",
        min = min(lyr0$dates), max = max(lyr0$dates),
        value = lyr0$dates[1], # days(1),
        animate = animationOptions(
          interval = 2000),
        width = "100%") ),
  
    absolutePanel(
      top = "15%", bottom = "20%", left = "19%", right = "71%",
      height = "65%", width = "10%",
      noUiSliderInput(
        "sld_depth", "Depth (m)",
        min         = as.double(names(lyr0$depths)[1]),
        max         = as.double(names(lyr0$depths)[length(lyr0$depths)]),
        value       = as.double(names(lyr0$depths)[1]),
        step        = 1.0,
        connect     = c(F, F),
        direction   = "ltr", 
        orientation = "vertical", 
        height      = "45vh") )  
)
