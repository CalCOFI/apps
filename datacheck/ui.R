# datacheck UI — a top bar (Cruise + dataset filter + copy-link), a resizable
# map of every dataset's stations for the cruise (colored by dataset), and
# Table / Plot tabs below. The dataset filter pills double as the color legend.
# State (cruise, dataset filter, pinned observation) round-trips through the URL
# query string so any view is shareable. A conductor tour walks first-timers
# through it.

# per-dataset swatch color for the filter pills — driven by dataset_pal (which
# comes from the GCS erd_legend sidecar, same source as calcofi.io/schema).
# keyed by the checkbox input's value (provider_dataset) via :has(), so colors
# stay correct regardless of which datasets a given cruise happens to include.
swatch_css <- paste(
  glue::glue(
    "#ds_pills .checkbox-inline:has(input[value=\"{names(dataset_pal)}\"])::before {{ \\
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
      /* theme vars mirroring calcofi.io/schema (dark default + light) */
      :root, [data-bs-theme='dark'] {{
        --dc-accent: #4dabf7; --dc-border: #3a3f44; --dc-panel2: #2c3035; --dc-fg: #e6e9ed; }}
      [data-bs-theme='light'] {{
        --dc-accent: #2780e3; --dc-border: #dee2e6; --dc-panel2: #ffffff; --dc-fg: #212529; }}
      /* dataset filter pills — same look as schema .filter-chip + .ds-swatch */
      #ds_pills .shiny-options-group {{ display: flex; flex-wrap: wrap; gap: .35rem; }}
      #ds_pills .checkbox-inline {{
        display: inline-flex; align-items: center; gap: .3rem; margin: 0;
        background: var(--dc-panel2); border: 1px solid var(--dc-border); color: var(--dc-fg);
        border-radius: 999px; padding: .1rem .6rem;
        font-family: ui-monospace, 'SF Mono', Menlo, Consolas, monospace; font-size: .74rem;
        cursor: pointer; user-select: none; }}
      #ds_pills .checkbox-inline input {{ position: absolute; opacity: 0; width: 0; height: 0; }}
      #ds_pills .checkbox-inline::before {{
        content: ''; display: inline-block; width: .7rem; height: .7rem;
        border-radius: 2px; border: 1px solid var(--dc-border); }}
      #ds_pills .checkbox-inline:hover {{ border-color: var(--dc-accent); }}
      #ds_pills .checkbox-inline:has(input:checked) {{
        background: var(--dc-accent); border-color: var(--dc-accent); color: #fff; }}
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
      input_dark_mode(id = "dark_toggle", mode = "dark")),
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
