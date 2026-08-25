# ctd-viz UI — no sidebar. A horizontal top bar carries the primary controls
# (Cruise + Measurement); lesser controls (Bathymetry, Max depth) live in a
# gear popover on the right of the tab strip. The main area is a resizable
# cruise map on top and Casts / Measurements / Plot tabs below. Selection is
# linked tri-directionally across map, table, and plot (server.R).
# A conductor tour walks first-time visitors through the controls and can be
# re-opened from the ? icon next to the title.

# wrapped in function(request) so Shiny can restore state from a bookmarked URL
# and so the brand header can start in the theme the request asks for
ui <- function(request) page_fillable(
  fillable_mobile = FALSE,
  padding         = 6,

  # conductor JS deps — required for tour$init()$start() to do anything
  useConductor(),

  # one-time client-side hooks: detect first visit, fire the tour, and let
  # the server stamp localStorage when it's been shown. tiny CSS overrides
  # tighten the top header (no form-group margin on the dropdowns) ----
  tags$head(
    # the calcofi.io brand contract (title, favicon, theme.css/js) plus GA4,
    # from the one snippet shared by every CalCOFI app.
    # log_url = "" keeps the usage-log Sheet leg off — drop it to opt in.
    calcofi4r::cc_brand_head(
      "CalCOFI CTD Explorer", ga_app = "ctd-viz", log_url = ""),
    tags$style(HTML("
      .cc-header .shiny-input-container { margin-bottom: 0; }
      /* conductor tour popup — default Shepherd width (~400 px) makes the
         intro paragraph tall enough to push the Next button off-screen.
         widen, cap height + scroll the body so buttons stay reachable. */
      .shepherd-element  { max-width: 560px !important; }
      .shepherd-content  { max-height: 85vh;
                           display: flex; flex-direction: column; }
      .shepherd-text     { overflow-y: auto; }
    ")),
    tags$script(HTML("
      // capture the bookmark query string at page load, before Shiny strips
      // it from the URL, so the server can restore the cast selection from it
      var CTDVIZ_INITIAL_SEARCH = window.location.search;
      $(document).on('shiny:connected', function() {
        Shiny.setInputValue('bookmark_search', CTDVIZ_INITIAL_SEARCH, {priority: 'event'});
        var seen = false;
        try { seen = localStorage.getItem('ctdviz_tour_seen') === 'true'; } catch(e) {}
        Shiny.setInputValue('tour_seen', seen, {priority: 'event'});
      });
      Shiny.addCustomMessageHandler('ctdviz_tour_seen', function(v) {
        try { localStorage.setItem('ctdviz_tour_seen', 'true'); } catch(e) {}
      });
    "))),

  # top header — the brand bar (logo -> calcofi.io, title, dark/light switch
  # whose id `dark_toggle` the server mirrors onto the maplibre basemap), with
  # help + Share, then Cruise + Measurement filling the remaining width
  calcofi4r::cc_brand_header(
    "CTD Explorer",
    actionButton(
      "btn_help",
      label = bsicons::bs_icon("question-circle"),
      class = "btn-link text-body-secondary p-0 border-0",
      title = "Show the tour"),
    # copy a shareable link that reopens this cruise + measurement + cast
    # selection (url bookmarking; enabled in global.R)
    bookmarkButton(
      label = "Share",
      icon  = bsicons::bs_icon("share"),
      title = "Copy a link that reopens this cruise, measurement and cast selection",
      class = "btn-sm btn-link text-body-secondary p-0 border-0"),
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
        width    = "100%")),
    mode = calcofi4r::cc_theme(request)),

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
