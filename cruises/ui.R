ui <- page_fillable(
  title = "CalCOFI Cruises Explorer",
  theme = bs_theme(bootswatch = "flatly"),

  navset_card_pill(
    id        = "navset",
    placement = "above",

    # table tab ----
    nav_panel(
      title = "Table",
      icon  = bs_icon("table"),
      card(
        full_screen = TRUE,
        card_header("Cruise Summary by Dataset"),
        DTOutput("tbl_cruises"))
    ),

    # map tab ----
    nav_panel(
      title = "Map",
      icon  = bs_icon("map"),
      card(
        full_screen = TRUE,
        card_header(textOutput("txt_map_title")),
        maplibreOutput("map_cruise", height = "100%"))
    )
  )
)
