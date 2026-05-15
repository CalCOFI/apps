# ctd-viz UI — no sidebar. A horizontal top bar carries the primary controls
# (Cruise + Measurement); lesser controls (Bathymetry, Max depth) live in a
# gear popover on the right of the tab strip. The main area is a resizable
# cruise map on top and Casts / Measurements / Plot tabs below. Selection is
# linked tri-directionally across map, table, and plot (server.R).
# A conductor tour walks first-time visitors through the controls and can be
# re-opened from the ? icon next to the title.

ui <- page_fillable(
  title           = "CalCOFI CTD Visualization",
  fillable_mobile = FALSE,
  padding         = 6,

  # conductor JS deps — required for tour$init()$start() to do anything
  useConductor(),

  # one-time client-side hooks: detect first visit, fire the tour, and let
  # the server stamp localStorage when it's been shown. tiny CSS overrides
  # tighten the top header (no form-group margin on the dropdowns) ----
  tags$head(
    tags$style(HTML("
      .ctdviz-header .shiny-input-container { margin-bottom: 0; }
      /* swap the logo variant based on the page's bslib theme — the
         original SVG has WHITE 'CalCOFI.io' text, hidden on light bg. */
      [data-bs-theme='light'] .ctdviz-logo-dark  { display: none; }
      [data-bs-theme='dark']  .ctdviz-logo-light { display: none; }
    ")),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        var seen = false;
        try { seen = localStorage.getItem('ctdviz_tour_seen') === 'true'; } catch(e) {}
        Shiny.setInputValue('tour_seen', seen, {priority: 'event'});
      });
      Shiny.addCustomMessageHandler('ctdviz_tour_seen', function(v) {
        try { localStorage.setItem('ctdviz_tour_seen', 'true'); } catch(e) {}
      });
    "))),

  # top header — logo + title + help, then Cruise + Measurement filling the
  # remaining width (each takes an equal flex share, wraps on narrow screens)
  div(
    class = "ctdviz-header d-flex align-items-end gap-3 px-2 pt-1 pb-1 flex-wrap",
    div(
      class = "d-flex align-items-center gap-2 pb-1 text-nowrap",
      a(href = "https://calcofi.io",
        img(src = "logo_calcofi.svg", height = "30px",
            class = "ctdviz-logo-dark"),
        img(src = "logo_calcofi_light.svg", height = "30px",
            class = "ctdviz-logo-light")),
      span("CTD Casts", class = "fs-5 fw-semibold"),
      actionButton(
        "btn_help",
        label = bsicons::bs_icon("question-circle"),
        class = "btn-link text-body-secondary p-0 border-0 ms-1",
        title = "Show the tour"),
      # sun / moon toggle — bslib swaps data-bs-theme on the page; the
      # server observer mirrors it onto the maplibre basemap.
      input_dark_mode(id = "dark_toggle", mode = "dark")),
    div(
      class = "flex-grow-1",
      style = "min-width: 240px;",
      selectInput(
        "sel_cruise", "Cruise",
        choices  = cruise_vec, selected = default_cruise,
        width    = "100%")),
    div(
      class = "flex-grow-1",
      style = "min-width: 200px;",
      selectInput(
        "sel_meas_type", "Measurement",
        choices  = meas_vec, selected = default_meas,
        width    = "100%"))),

  # top pane — resizable cruise map ----
  jqui_resizable(
    div(
      id    = "pane_top",
      style = "flex: 0 0 auto; height: 44vh; min-height: 220px;",
      maplibreOutput("map_cruise", height = "100%")),
    options = list(handles = "s")),

  # bottom pane — Casts / Measurements / Plot ----
  navset_card_underline(
    id          = "subtabs",
    height      = "100%",
    full_screen = TRUE,   # bslib maximize button (top-right of the card)

    nav_panel(
      "Casts",
      # one-line header that alternates between click-to-select instructions
      # (initially / after Reset) and a Reset · count · Download bar (after
      # a selection or anchor is started). server-side renderUI swap.
      div(class = "mt-2 mb-2", uiOutput("ui_sel_bar")),
      DTOutput("tbl_casts")),

    nav_panel(
      "Measurements",
      div(
        class = "small text-muted mt-2 mb-2",
        textOutput("txt_values_caption", inline = TRUE)),
      DTOutput("tbl_values")),

    nav_panel(
      "Plot",
      plotlyOutput("plot_transect", height = "100%")),

    # advanced settings — tucked into a gear popover on the right of the
    # nav bar. Bathymetry (map overlay) + Max depth (plot y-cap) are both
    # lesser-priority controls that don't need to be on screen always.
    nav_spacer(),
    nav_item(
      popover(
        trigger = tags$button(
          id    = "btn_settings",
          class = "btn btn-sm btn-link text-body-secondary",
          title = "Plot settings",
          bsicons::bs_icon("gear")),
        title = "Plot settings",
        checkboxInput(
          "chk_bathy", "Bathymetry", value = FALSE),
        sliderInput(
          "sl_max_depth", "Max depth (m)",
          min = 50, max = 5000, value = 5000, step = 50, width = "240px"),
        div(
          class = "small text-muted",
          "Bathymetry shows GEBCO 2025 seafloor depth on the map. ",
          "Max depth caps the y-axis of the Plot and filters the ",
          "Measurements table. Leave at the maximum to fit the data."))))
)
