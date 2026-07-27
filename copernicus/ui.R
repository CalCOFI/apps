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

  # GA4, from the one snippet shared by every CalCOFI app (calcofi4r is already
  # attached here, so this costs nothing and cannot go stale). log_url = "" keeps
  # the usage-log Sheet leg off — drop it to opt in.
  tags$head(calcofi4r::cc_ga_head("copernicus", log_url = "")),

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
