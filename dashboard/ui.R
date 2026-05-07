dashboardPage(
  
  dashboardHeader(
    title = "CalCOFI Dashboard"),
  
  # sidebarMenu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Overview", tabName = "tab_over"),
      menuItem(
        "Climate & Ocean Drivers", tabName = "tab_co",
        menuSubItem("Ocean Temperature",    tabName = "tab_co_temp"),
        menuSubItem("Regional Upwelling",   tabName = "tab_co_upwell"),
        menuSubItem("Hypoxia",              tabName = "tab_co_hypoxia"),
        menuSubItem("Ocean Acidification",  tabName = "tab_co_oa"),
        menuSubItem("Harmful algal blooms", tabName = "tab_co_hab"),
        menuSubItem("Picoplankton",         tabName = "tab_co_pico"),
        menuSubItem("Zooplankton",          tabName = "tab_co_zoo"),
        menuSubItem("Aquaculture",          tabName = "tab_co_aqua")),
      menuItem(
        "Ecological integrity", tabName = "tab_ei",
        menuSubItem("Forage Species", tabName = "tab_ei_forage"), 
        menuSubItem("Krill",          tabName = "tab_ei_krill"), 
        menuSubItem("Fish",           tabName = "tab_ei_fish"), 
        menuSubItem("Seabirds",       tabName = "tab_ei_seabird"), 
        menuSubItem("Groundfish",     tabName = "tab_ei_groundfish"), 
        menuSubItem("Marine mammals", tabName = "tab_ei_marmam"), 
        menuSubItem("Biodiversity",   tabName = "tab_ei_bio")))), 
  
  dashboardBody(
    tags$head(includeHTML("google-analytics.html")),
    tabItems(
      
      # tab_over ----
      tabItem(
        "tab_over",
        fluidRow(
          box(
            title = "Survey", width = 6, height = "92vh",
            selectInput("sel_yr", "Year", yrs_lns),
            #* map_over ----
            leafletOutput("map_over", height = "77vh")),
          box(
            title = "Indicators", width = 6, height = "92vh",
            #* vbox_* ----
            fluidRow(  
              valueBoxOutput("vbox_temp", width = 6),
              valueBoxOutput("vbox_krill", width = 6)),
            fluidRow(  
              valueBoxOutput("vbox_oxygen", width = 6),
              valueBoxOutput("vbox_forage", width = 6))))),
      
      # tab_co_temp ----
      tabItem(
        "tab_co_temp",
        h2("Ocean Temperature"),
        fluidRow(
          box(
            title = "Map of Surface", width = 6, height = "85vh",
            selectInput(
              "sel_yrmo", "Date", 
              setNames(
                sst_dates,
                format(sst_dates, "%Y-%m"))),
            #* map_sst ----
            leafletOutput("map_sst", height = "70vh")),
          box(
            title = "Plot over Time", width = 6, height = "85vh",
            #* plot_temp ----
            dygraphOutput("plot_temp", height = "77vh")))),

      # tab_co_upwell ----
      tabItem(
        "tab_co_upwell",
        h2("Regional Upwelling"),
        "TBD"),
        
      # tab_co_hypoxia ----
      tabItem(
        "tab_co_hypoxia",
        h2("Hypoxia"),
        "TBD"),
        
      # tab_co_oa ----
      tabItem(
        "tab_co_oa",
        h2("Ocean Acidification"),
        "TBD"),
        
      # tab_co_hab ----
      tabItem(
        "tab_co_hab",
        h2("Harmful algal blooms"),
        "TBD"),
        
      # tab_co_pico ----
      tabItem(
        "tab_co_pico",
        h2("Picoplankton"),
        "TBD"),
        
      # tab_co_zoo ----
      tabItem(
        "tab_co_zoo",
        h2("Zooplankton"),
        "TBD"),
        
      # tab_co_aqua ----
      tabItem(
        "tab_co_aqua",
        h2("Aquaculture"),
        "TBD"),
        
      # tab_ei_forage ----
      tabItem(
        "tab_ei_forage",
        h2("Forage Species"),
        "TBD"),
        
      # tab_ei_krill ----
      tabItem(
        "tab_ei_krill",
        h2("Krill"),
        "TBD"),
        
      # tab_ei_fish ----
      tabItem(
        "tab_ei_fish",
        h2("Fish"),
        "TBD"),
        
      # tab_ei_seabird ----
      tabItem(
        "tab_ei_seabird",
        h2("Seabirds"),
        "TBD"),
        
      # tab_ei_groundfish ----
      tabItem(
        "tab_ei_groundfish",
        h2("Groundfish"),
        "TBD"),
        
      # tab_ei_marmam ----
      tabItem(
        "tab_ei_marmam",
        h2("Marine mammals"),
        "TBD"),
        
      # tab_ei_bio ----
      tabItem(
        "tab_ei_bio",
        h2("Biodiversity"),
        "TBD")
              
      )))

