# ctd-viz UI — a collapsible sidebar of controls, plus a main area with two
# vertically-stacked, resizable panes:
#   top    : birds-eye cruise map
#   bottom : Table / Plot / Cruise Stats subtabs
# selection is linked tri-directionally across map, table, and plot (server.R).

ui <- page_sidebar(
  window_title = "CalCOFI CTD Visualization",
  title = tagList(
    a(img(src = "logo_calcofi.svg", height = "34px", .noWS = "after"),
      href = "https://calcofi.io"),
    span("CTD Viz", class = "h6 m-0 ms-2 text-muted")),
  fillable = TRUE,
  padding  = 6,

  # collapsible sidebar — every control lives here ----
  sidebar = sidebar(
    width = 290,
    title = "Controls",
    selectInput(
      "sel_cruise", "Cruise",
      choices = cruise_vec, selected = default_cruise),
    checkboxInput(
      "chk_bathy", "Seafloor bathymetry", value = TRUE),
    hr(),
    selectInput(
      "sel_meas_type", "Measurement",
      choices = meas_vec, selected = default_meas),
    sliderInput(
      "sl_max_depth", "Max depth (m)",
      min = 0, max = 5000, value = 500, step = 50),
    div(
      class = "small text-muted",
      "Filters the Values table and the transect plot."),
    hr(),
    uiOutput("ui_reset_sel"),
    div(
      class = "small text-muted mt-2",
      textOutput("txt_sel_count", inline = TRUE)),
    div(
      class = "small text-muted mt-2",
      "Map: click a station to start a transect, then click another to ",
      "select every station between them along the cruise track. Use the ",
      "Table for arbitrary multi-selection.")),

  # top pane — resizable cruise map ----
  jqui_resizable(
    div(
      id    = "pane_top",
      style = "flex: 0 0 auto; height: 44vh; min-height: 220px;",
      maplibreOutput("map_cruise", height = "100%")),
    options = list(handles = "s")),

  # bottom pane — Table / Plot / Cruise Stats ----
  navset_card_underline(
    id     = "subtabs",
    height = "100%",

    nav_panel(
      "Table",
      navset_underline(
        nav_panel(
          "Casts",
          div(
            class = "small text-muted mt-2 mb-2",
            "Click rows to select; the map and plot follow ",
            "(selecting on the map updates the table in turn)."),
          DTOutput("tbl_casts")),
        nav_panel(
          "Values",
          div(
            class = "small text-muted mt-2 mb-2",
            textOutput("txt_values_caption", inline = TRUE)),
          DTOutput("tbl_values")))),

    nav_panel(
      "Plot",
      plotlyOutput("plot_transect", height = "100%")),

    nav_panel(
      "Cruise Stats",
      layout_columns(
        fill = FALSE,
        col_widths = c(4, 4, 4),
        value_box(
          "Casts in cruise", textOutput("vb_casts"),
          showcase = bsicons::bs_icon("water"), theme = "primary"),
        value_box(
          "Casts selected", textOutput("vb_selected"),
          showcase = bsicons::bs_icon("hand-index"), theme = "secondary"),
        value_box(
          "Median depth gap (m)", textOutput("vb_gap"),
          showcase = bsicons::bs_icon("rulers"), theme = "secondary")),
      DTOutput("tbl_stats")))
)
