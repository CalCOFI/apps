# datacheck UI — a top bar (Cruise + dataset filter + copy-link), a resizable
# map of every dataset's stations for the cruise (colored by dataset), and
# Table / Plot tabs below. The dataset filter pills double as the color legend.
# State (cruise, dataset filter, pinned observation) round-trips through the URL
# query string so any view is shareable. A conductor tour walks first-timers
# through it.

# per-dataset swatch CSS for the filter pills — generated from dataset_pal so
# the legend colors never drift from the map. checkboxGroupInput(inline) emits
# <label class="checkbox-inline"> children in choices order; tag each with a
# colored dot via nth-child.
swatch_css <- paste(
  glue::glue(
    "#ds_pills .shiny-options-group > .checkbox-inline:nth-child({seq_along(dataset_pal)})::before {{ \\
     background: {unname(dataset_pal)}; }}"),
  collapse = "\n")

ui <- page_fillable(
  title           = "CalCOFI DataCheck",
  fillable_mobile = FALSE,
  padding         = 6,

  useConductor(),

  tags$head(
    tags$style(HTML(glue::glue("
      .dc-header .shiny-input-container {{ margin-bottom: 0; }}
      [data-bs-theme='light'] .dc-logo-dark  {{ display: none; }}
      [data-bs-theme='dark']  .dc-logo-light {{ display: none; }}
      /* dataset filter pills: inline checkboxes with a colored dot swatch */
      #ds_pills .shiny-options-group {{ display: flex; flex-wrap: wrap; gap: .25rem .75rem; }}
      #ds_pills .checkbox-inline {{ margin: 0; padding-left: 0; font-size: .85rem; }}
      #ds_pills .checkbox-inline::before {{
        content: ''; display: inline-block; width: .7rem; height: .7rem;
        border-radius: 50%; margin-right: .3rem; vertical-align: middle; }}
      {swatch_css}
      .shepherd-element {{ max-width: 560px !important; }}
      .shepherd-content {{ max-height: 85vh; display: flex; flex-direction: column; }}
      .shepherd-text    {{ overflow-y: auto; }}
    "))),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        var seen = false;
        try { seen = localStorage.getItem('datacheck_tour_seen') === 'true'; } catch(e) {}
        Shiny.setInputValue('tour_seen', seen, {priority: 'event'});
      });
      Shiny.addCustomMessageHandler('datacheck_tour_seen', function(v) {
        try { localStorage.setItem('datacheck_tour_seen', 'true'); } catch(e) {}
      });
      // copy the current address-bar URL (kept in sync via updateQueryString)
      function dcCopyLink(btn) {
        navigator.clipboard.writeText(window.location.href).then(function() {
          var old = btn.innerHTML;
          btn.innerHTML = '✓ Copied';
          setTimeout(function(){ btn.innerHTML = old; }, 1400);
        });
      }
    "))),

  # top header — logo + title + help/theme, then Cruise + dataset filter + copy
  div(
    class = "dc-header d-flex align-items-end gap-3 px-2 pt-1 pb-1 flex-wrap",
    div(
      class = "d-flex align-items-center gap-2 pb-1 text-nowrap",
      a(href = "https://calcofi.io",
        img(src = "logo_calcofi.svg", height = "30px", class = "dc-logo-dark"),
        img(src = "logo_calcofi_light.svg", height = "30px", class = "dc-logo-light")),
      span("DataCheck", class = "fs-5 fw-semibold"),
      actionButton(
        "btn_help", label = bsicons::bs_icon("question-circle"),
        class = "btn-link text-body-secondary p-0 border-0 ms-1",
        title = "Show the tour"),
      input_dark_mode(id = "dark_toggle", mode = "light")),
    div(
      class = "flex-grow-1",
      style = "min-width: 280px;",
      selectInput(
        "sel_cruise", "Cruise",
        choices = cruise_vec, selected = default_cruise, width = "100%")),
    div(
      style = "min-width: 260px;",
      tags$label("Datasets", class = "form-label mb-1 d-block"),
      div(id = "ds_pills", uiOutput("ds_pills_ui"))),
    div(
      class = "pb-1",
      tags$button(
        id      = "btn_copy",
        class   = "btn btn-sm btn-outline-primary",
        onclick = "dcCopyLink(this)",
        title   = "Copy a shareable link to this exact view",
        bsicons::bs_icon("link-45deg"), " Copy link"))),

  # top pane — resizable map of the cruise's stations ----
  jqui_resizable(
    div(
      id    = "pane_map",
      style = "flex: 0 0 auto; height: 46vh; min-height: 240px;",
      maplibreOutput("map_obs", height = "100%")),
    options = list(handles = "s")),

  # bottom pane — Table / Plot ----
  navset_card_underline(
    id          = "subtabs",
    height      = "100%",
    full_screen = TRUE,

    nav_panel(
      "Table",
      div(class = "small text-muted mt-2 mb-2",
          textOutput("txt_table_caption", inline = TRUE),
          span(class = "ms-2", uiOutput("ui_download", inline = TRUE))),
      DTOutput("tbl_obs")),

    nav_panel(
      "Plot",
      div(class = "small text-muted mt-2 mb-1",
          "Space-time coverage: each point is one observation (date × ",
          "latitude), colored by dataset. Points stacked at the same station ",
          "mean datasets co-sampled it."),
      plotlyOutput("plot_spacetime", height = "100%"))))
