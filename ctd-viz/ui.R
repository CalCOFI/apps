# ctd-viz UI — no sidebar. A horizontal top bar carries the primary controls
# (Cruise + Measurement); lesser controls (Bathymetry, Max depth) live in a
# gear popover on the right of the tab strip. The main area is a resizable
# cruise map on top and Casts / Measurements / Plot tabs below. Selection is
# linked tri-directionally across map, table, and plot (server.R).
# a conductor welcome popup greets first-time visitors, with its own button
# into a separate 6-step walkthrough of the controls; the walkthrough alone
# (skipping the welcome blurb) can be re-opened any time from the ? icon
# next to the title.

# wrapped in function(request) so Shiny can restore state from a bookmarked URL
ui <- function(request) page_fillable(
  title           = "CalCOFI CTD Visualization",
  fillable_mobile = FALSE,
  padding         = 6,

  # conductor JS deps — required for either tour's $init()$start() to do
  # anything
  useConductor(),

  # one-time client-side hooks: detect first visit, fire the tour, and let
  # the server stamp localStorage when it's been shown. tiny CSS overrides
  # tighten the top header (no form-group margin on the dropdowns) ----
  tags$head(
    # GA4, from the one snippet shared by every CalCOFI app (calcofi4r is
    # already attached here, so this costs nothing and cannot go stale).
    # log_url = "" keeps the usage-log Sheet leg off — drop it to opt in.
    calcofi4r::cc_ga_head("ctd-viz", log_url = ""),
    tags$style(HTML("
      .ctdviz-header .shiny-input-container { margin-bottom: 0; }
      /* labels ('Cruise', 'Measurement') and the dropdown text both render
         at the browser default (16px) — 2px smaller reads less heavy next
         to the rest of the app without touching the 'CTD Casts' title. */
      .ctdviz-header label,
      .ctdviz-header .selectize-input,
      .ctdviz-header .selectize-dropdown { font-size: 14px; }
      /* swap the logo variant based on the page's bslib theme — the
         original SVG has WHITE 'CalCOFI.io' text, hidden on light bg. */
      [data-bs-theme='light'] .ctdviz-logo-dark  { display: none; }
      [data-bs-theme='dark']  .ctdviz-logo-light { display: none; }
      /* conductor tour popup — default Shepherd width (~400 px) made the
         intro paragraph tall enough to push the Next button off-screen, so
         this was widened once — but 640px read as unnecessarily wide for
         the short 2-4 line steps, so it's settled here narrower than that
         first pass while still short of the original cramped default. cap
         height + scroll the body so buttons stay reachable regardless. */
      .shepherd-element  { max-width: 480px !important; }
      .shepherd-content  { max-height: 85vh;
                           display: flex; flex-direction: column; }
      /* bumped up from a smaller in-between size — read as too small next
         to the app's own UI chrome. */
      .shepherd-text     { overflow-y: auto; font-size: 19px; }
      /* the Welcome step has no `el` (attachTo target), so Shepherd centers
         it and tags it with this class automatically — no custom JS/R
         needed. it's the intro/main step, with more text than any other,
         so it gets its own wider column and a size larger than the rest. */
      .shepherd-element.shepherd-centered { max-width: 600px !important; }
      .shepherd-element.shepherd-centered .shepherd-text { font-size: 21px; }
      /* Shepherd's default title renders very large relative to the rest of
         the popup — cut down closer to a normal heading size. */
      .shepherd-title    { font-size: 20px; }
      /* the Welcome step replaces Conductor's default footer buttons with
         its own 'Start' button embedded directly in the step text (see
         global.R) — hide the real footer just for this step so
         they don't both show. scoped to .shepherd-centered for the same
         reason as the font-size override just above: it's the only step
         with no `el`, so it's the only one Shepherd tags with this class. */
      .shepherd-element.shepherd-centered .shepherd-footer { display: none; }
      /* steps placed left/right of their target (Pick a parameter, Plot
         settings) don't always fit beside a target that's near the
         viewport edge — floating-ui's collision handling then shifts/flips
         them away from where they were asked to go, which is what once
         made a tooltip's arrow read as pointing at the wrong thing.
         narrower gives them more room to actually render on the requested
         side. Shepherd sets this attribute itself to reflect the placement
         it actually computed. */
      .shepherd-element[data-popper-placement^='left'],
      .shepherd-element[data-popper-placement^='right'] {
        max-width: 380px !important;
      }
      /* every walkthrough step (except Welcome) pairs a highlightClass
         outline with the popup, and by default Shepherd leaves them
         touching — the outline reads as crowded right up against the
         popup edge. floating-ui computes each popup's `top`/`left` as if
         its margin were zero, so a CSS margin added on the side facing the
         target pushes the rendered box further away without disturbing
         that calculation — margin-top for a popup placed below/above its
         target, margin-left for one placed left/right (the opposite-edge
         margins don't do anything here, since these boxes size to their
         content rather than stretching to fill a set width). direction of
         the margin (+/-) depends on which side of the target the popup is
         actually on. */
      .shepherd-element[data-popper-placement^='bottom'] { margin-top: 10px !important; }
      .shepherd-element[data-popper-placement^='top']    { margin-top: -10px !important; }
      .shepherd-element[data-popper-placement^='right']  { margin-left: 10px !important; }
      .shepherd-element[data-popper-placement^='left']   { margin-left: -10px !important; }
      /* 'x' close control, prepended to every step's text (see global.R) —
         just a marker class, no onclick: Shepherd sanitizes step HTML and
         strips inline event handler attributes, so the actual click
         handling lives in ui.R's own <script> instead (delegation on
         document, outside anything Shepherd touches). positioned absolute
         — .shepherd-element is already `position: fixed` (set by
         floating-ui), which is enough to anchor it to the popup's corner
         regardless of where it sits in the text. */
      .ctdviz-tour-close {
        position: absolute;
        top: 14px;
        right: 14px;
        width: 28px;
        height: 28px;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 20px;
        line-height: 1;
        color: #495057;
        cursor: pointer;
        border-radius: 50%;
        z-index: 10;
      }
      .ctdviz-tour-close:hover { background-color: rgba(0, 0, 0, 0.08); }
      /* Shepherd renders its tooltip in the light DOM at the very end of
         <body>, so page-level Bootstrap CSS vars (e.g. --bs-code-color, set
         light-on-dark under [data-bs-theme='dark']) cascade straight into
         it — making <code> text in tour copy invisible against Shepherd's
         fixed white tooltip background. force a readable dark color,
         independent of whatever theme the rest of the page is in (same
         pattern as the .maplibregl-popup-content fix below). */
      .shepherd-text code {
        color: #1a1a1a !important;
        background-color: rgba(0, 0, 0, 0.06);
        padding: .1em .3em;
        border-radius: .25rem;
      }
      /* tour step for #subtabs: Shepherd's default arrow points at wherever
         the tooltip happens to attach, which reads as 'pointing at just the
         Readings tab' even though the step describes all five tabs. this
         class (applied via step()'s highlightClass) outlines the *whole*
         tab bar instead, so the highlight matches what the text covers. */
      .ctdviz-tour-highlight {
        outline: 3px solid var(--bs-primary) !important;
        outline-offset: 3px;
        border-radius: 8px;
        /* soft glow on top of the outline itself — now that the popup is
           pushed off the target with its own margin (see the
           data-popper-placement rules below), there's room to make this
           more visually prominent without it reading as cluttered against
           the popup. */
        box-shadow: 0 0 14px 2px rgba(var(--bs-primary-rgb), 0.55);
      }
      /* 'N of 6' progress line appended to each walkthrough step's text
         (not shown on the separate Welcome step).
         Shepherd's tooltip background is fixed white regardless of the
         app's theme (see .shepherd-text code above), so this is a
         hardcoded gray rather than a Bootstrap CSS var. */
      .ctdviz-tour-step {
        margin-top: 10px;
        font-size: 0.75em;
        color: #6c757d;
      }
      /* Casts / Measurements tables read cramped at DT's default row height
         and font size — bump both, and give the header row a touch more
         weight so it's readable at a glance rather than requiring a lean-in. */
      .ctdviz-tbl { font-size: 0.85rem; }
      .ctdviz-tbl thead th { font-size: 0.85rem; padding-top: 10px; padding-bottom: 10px; }
      .ctdviz-tbl tbody td { padding-top: 9px; padding-bottom: 9px; }
      /* the table used to have no visible boundary of its own — with a
         single row (no alternating stripe to contrast against) it barely
         read as a table at all against the page's dark background. a
         container border/background gives it a boundary regardless of row
         count, and the striping + text color are bumped to actually stand
         out rather than relying on Bootstrap's default (very subtle in
         dark mode) values. */
      .ctdviz-tbl.dataTable {
        border: 1px solid var(--bs-border-color);
        border-radius: 8px;
        overflow: hidden;
      }
      .ctdviz-tbl.dataTable tbody tr.odd  { background-color: var(--bs-tertiary-bg); }
      .ctdviz-tbl.dataTable tbody tr.even { background-color: var(--bs-body-bg); }
      .ctdviz-tbl tbody td { color: var(--bs-body-color); font-weight: 500; }
      /* DT's own selected-row highlight was getting silently overridden by
         the odd/even striping rules just above (same specificity, defined
         later in this stylesheet) — clicking a row in Casts (which centers
         the map on that cast — see server.R) looked like nothing happened,
         since the row itself never changed color. */
      .ctdviz-tbl.dataTable tbody tr.selected td {
        background-color: var(--bs-primary) !important;
        color: #fff !important;
      }
      /* Reset selection button — no border now (too many outlined buttons
         next to each other — the chip, this, Download CSV — was reading as
         clutter). plain text-style button, with a background fill on hover
         as the only affordance. */
      .ctdviz-btn-reset {
        border: none;
        color: var(--bs-body-color);
        font-weight: 500;
        background: transparent;
      }
      .ctdviz-btn-reset:hover {
        background: var(--bs-tertiary-bg);
        color: var(--bs-primary);
      }
      /* current-parameter chip (Casts tab header) — reads as 'here's what
         you're viewing, click to change it' as one element, filter-tag
         style, rather than a name next to a separate 'back' link/button. */
      .ctdviz-param-chip {
        background: var(--bs-tertiary-bg);
        border: 1px solid var(--bs-border-color);
        border-radius: 999px;
        font-weight: 600;
        padding: 3px 12px;
      }
      .ctdviz-param-chip:hover {
        background: var(--bs-secondary-bg);
        border-color: var(--bs-primary);
      }
      .ctdviz-divider {
        width: 1px; height: 20px;
        background: var(--bs-border-color);
        flex: 0 0 auto;
      }
      /* Map + data side by side, both full height, instead of stacked
         top/bottom — see the comment above the .ctdviz-body div for why. */
      .ctdviz-body { display: flex; flex-direction: row; gap: 0; min-height: 0; }
      .ctdviz-map-col  { min-width: 260px; }
      .ctdviz-data-col { min-width: 320px; }
      /* drag-to-resize handle — a real flex child between the two columns
         (not an overlay), so it's always exactly where it visually sits.
         thin and unobtrusive at rest; lights up blue on hover/while
         dragging so it's still easy to find without looking bulky. */
      .ctdviz-resize-handle {
        flex: 0 0 4px; width: 4px; height: 100%;
        background: transparent;
        cursor: col-resize;
        margin: 0 2px;
        transition: background-color 0.1s ease;
      }
      .ctdviz-resize-handle:hover,
      .ctdviz-resize-handle.is-dragging {
        background: var(--bs-primary);
      }
      /* narrow / phone screens: side-by-side becomes unusably thin, so stack
         back to map-on-top instead (same as the app's original layout) */
      @media (max-width: 860px) {
        .ctdviz-body     { flex-direction: column; }
        .ctdviz-map-col  { flex: 0 0 38vh !important; width: 100% !important; }
        .ctdviz-data-col { flex: 1 1 auto !important; min-height: 320px; }
        .ctdviz-resize-handle { display: none; }
      }
      /* the station hover popup is always a white maplibre box regardless of
         page theme — but its text was inheriting Bootstrap's dark-mode body
         color (light grey), which is nearly invisible on that white
         background. force a readable dark color on the popup specifically,
         independent of whatever theme the rest of the page is in. */
      .maplibregl-popup-content, .maplibregl-popup-content b {
        color: #1a1a1a !important;
      }
      /* plotly's modebar icons default to a very low opacity until hover —
         nearly invisible against a white plot background. targeting the
         actual SVG path fill directly (not just container opacity) since
         Plotly sets a light grey fill via inline style that opacity alone
         wasn't fully overcoming. also nudged down slightly so the (now
         vertical, right-edge) icon column doesn't sit flush against a long
         title's top-right corner. */
      .modebar-btn { opacity: 0.9 !important; }
      .modebar-btn:hover { opacity: 1 !important; }
      .modebar-btn path { fill: rgba(0, 0, 0, 0.65) !important; }
      .modebar-btn:hover path { fill: rgba(0, 0, 0, 0.9) !important; }
      .modebar-btn svg { width: 20px !important; height: 20px !important; }
      .modebar-container { top: 35px !important; }
      /* data-panel expand toggle (button wired in the tags$script below):
         takes the Parameters/Casts/Readings/Plot panel to a full-viewport
         overlay so it's not limited to its half of the screen. click the
         same button again (it stays visible, since it's part of the panel
         that goes fullscreen) to collapse back. */
      .ctdviz-data-col.is-expanded {
        position: fixed !important;
        inset: 0;
        z-index: 1050;
        width: 100vw !important;
        height: 100vh !important;
        border-radius: 0;
      }
      /* the expand button holds both icons at all times; only one shows,
         based on which side of is-expanded the ancestor is currently on. */
      .ctdviz-icon-collapse { display: none; }
      .ctdviz-data-col.is-expanded .ctdviz-icon-expand   { display: none; }
      .ctdviz-data-col.is-expanded .ctdviz-icon-collapse { display: inline; }
      /* confirmed via DevTools: .dataTables_length / .dataTables_info /
         .dataTables_paginate are plain sibling <div>s directly inside
         .dataTables_wrapper — there is no Bootstrap .row wrapping them in
         this DT build. Bootstrap's own default CSS already floats info
         left / pagination right correctly on its own — an earlier attempt
         to force all three into inline-block broke that (inline-block has
         no way to push pagination to the right on its own; that needs
         float or flex+justify, which is exactly what floating already
         did). Reverted — only the gap above this whole block is tightened. */
      table.dataTable { margin-bottom: 2px !important; }
      .dataTables_wrapper { padding-top: 0 !important; margin-top: 0 !important; }
      /* 'Show X entries' — targeting the label + select directly (not just
         the wrapping .dataTables_length div), same lesson as the pagination
         buttons below: Bootstrap sets font-size directly on form elements,
         which beats an inherited size from any ancestor. */
      .dataTables_wrapper .dataTables_length label,
      .dataTables_wrapper .dataTables_length select {
        font-size: 13px !important;
      }
      /* the 'Showing X to Y of Z entries' text — back to a normal readable
         size (an earlier pass over-shrank it). */
      .dataTables_wrapper .dataTables_info { font-size: 13px !important; }
      /* the actual Previous/1/2/Next buttons — targeting .page-link
         directly, not the wrapping .dataTables_paginate div. Bootstrap
         sets an explicit font-size on .page-link itself, which is a DIRECT
         rule on the element and beats an INHERITED font-size from any
         ancestor no matter how specific or !important that ancestor rule
         is. this is why the earlier attempt never visibly changed anything. */
      .dataTables_wrapper .pagination {
        margin: 0 !important; gap: 0 !important;
      }
      .dataTables_wrapper .page-item { margin: 0 !important; }
      .dataTables_wrapper .page-link,
      .dataTables_wrapper .paginate_button {
        font-size: 12px !important;
        padding: 3px 9px !important;
        margin-left: -1px !important;
      }
      /* Parameters tab — flat clickable list of every available variable,
         styled as a plain divided list (thin row dividers, hover highlight,
         a left accent bar for the active row) rather than stacked bordered
         boxes, which read as dropdown options rather than a browsable panel. */
      /* hide scrollbar chrome throughout the data column — still scrolls if
         content ever needs it, just no visible track/thumb. Applied broadly
         rather than to one specific element because the actual overflow
         container here is bslib's own tab-content wrapper, not something
         with a class this file controls directly. */
      .ctdviz-data-col, .ctdviz-data-col * {
        scrollbar-width: none;      /* Firefox */
        -ms-overflow-style: none;   /* old Edge */
      }
      .ctdviz-data-col::-webkit-scrollbar,
      .ctdviz-data-col *::-webkit-scrollbar { display: none; }  /* Chrome/Safari */
      /* Parameters tab — flat clickable list of every available variable,
         styled as a plain divided list (thin row dividers, hover highlight,
         a left accent bar for the active row) rather than stacked bordered
         boxes, which read as dropdown options rather than a browsable panel. */
      .ctdviz-param-list {
        display: flex; flex-direction: column;
        border-top: 1px solid var(--bs-border-color);
      }
      .ctdviz-param-row {
        display: flex; align-items: center; gap: 10px;
        padding: 7px 10px; cursor: pointer;
        border-bottom: 1px solid var(--bs-border-color);
        border-left: 3px solid transparent;
      }
      .ctdviz-param-row:hover { background: var(--bs-tertiary-bg); }
      .ctdviz-param-icon {
        flex: 0 0 auto; width: 16px; height: 16px;
        color: var(--bs-secondary-color); display: flex;
      }
      .ctdviz-param-row-active .ctdviz-param-icon { color: var(--bs-primary); }
      .ctdviz-param-name { flex: 1 1 auto; font-size: 0.9rem; }
      .ctdviz-param-unit {
        flex: 0 0 auto; font-size: 0.78rem; color: var(--bs-secondary-color);
      }
      .ctdviz-param-row-active {
        background: color-mix(in srgb, var(--bs-primary) 14%, transparent);
        border-left-color: var(--bs-primary);
      }
      .ctdviz-param-row-active .ctdviz-param-name {
        font-weight: 600; color: var(--bs-primary);
      }
    ")),
    tags$script(HTML("
      document.addEventListener('click', function(e) {
        var btn = e.target.closest('#btn_expand_data');
        if (!btn) return;
        var col = document.querySelector('.ctdviz-data-col');
        if (!col) return;
        var expanded = col.classList.toggle('is-expanded');
        btn.title = expanded ? 'Collapse' : 'Expand to full screen';
      });
    ")),
    tags$script(HTML("
      // drag-to-resize for the map/data split — plain mouse tracking rather
      // than jQuery UI's resizable widget, which fought this flex layout
      // (its inline width updates had no visible effect against flex-basis).
      // sets the map column's pixel width directly on mousemove; the data
      // column is a bslib fill item, so it absorbs whatever's left with no
      // extra work here.
      (function() {
        var handle, mapCol, startX, startWidth;
        document.addEventListener('mousedown', function(e) {
          handle = e.target.closest('#ctdviz_resize_handle');
          if (!handle) return;
          mapCol = document.getElementById('pane_map');
          if (!mapCol) return;
          startX     = e.clientX;
          startWidth = mapCol.getBoundingClientRect().width;
          handle.classList.add('is-dragging');
          document.body.style.userSelect = 'none';
          e.preventDefault();
        });
        document.addEventListener('mousemove', function(e) {
          if (!handle) return;
          var container = mapCol.parentElement;
          var containerWidth = container.getBoundingClientRect().width;
          // cap both sides at a firm percentage of the container, not just a
          // fixed pixel floor — a raw px min/max let one pane balloon out on
          // a wide monitor. min 30% / max 65% keeps both panes usable.
          var minWidth = containerWidth * 0.30;
          var maxWidth = containerWidth * 0.65;
          var newWidth = startWidth + (e.clientX - startX);
          newWidth = Math.max(minWidth, Math.min(maxWidth, newWidth));
          mapCol.style.width = newWidth + 'px';
          // the map widget (maplibre) redraws its canvas on its own resize
          // detection, which can be debounced — firing a plain window
          // resize event on every drag step is a harmless nudge that makes
          // sure it keeps redrawing continuously instead of only catching
          // up once you let go of the handle.
          window.dispatchEvent(new Event('resize'));
        });
        document.addEventListener('mouseup', function() {
          if (!handle) return;
          handle.classList.remove('is-dragging');
          document.body.style.userSelect = '';
          handle = null;
        });
      })();
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
      // tour close (x) / start buttons — embedded as plain marker classes in
      // the step text (global.R), not onclick attributes: Shepherd
      // sanitizes step HTML and strips inline event handlers, so an
      // onclick placed there is silently dropped and never fires. handled
      // here instead, via delegation on the document (this script isn't
      // part of what Shepherd sanitizes), the same pattern already used
      // for the resize handle above.
      //
      // closing/cancelling itself is done server-side (server.R calls
      // tour_welcome$cancel() / tour_walkthrough$cancel()) rather than from
      // here — `Shepherd.activeTour.cancel()` and a synthetic Escape
      // keydown were both tried client-side first and neither actually
      // closed the popup, so this round-trips through Shiny instead, the
      // same mechanism that's already confirmed working for starting both
      // tours.
      document.addEventListener('click', function(e) {
        if (e.target.closest('.ctdviz-tour-close')) {
          Shiny.setInputValue('btn_tour_close', Math.random(), {priority: 'event'});
        }
        if (e.target.closest('.ctdviz-tour-start')) {
          Shiny.setInputValue('btn_start_walkthrough', Math.random(), {priority: 'event'});
        }
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
      # the logo above already links out to calcofi.io, so this reopens the
      # welcome popup instead — a plain span made clickable, styled to still
      # read as the app title rather than an obvious link/button.
      actionButton(
        "btn_reopen_welcome",
        label = "CTD Casts",
        class = "btn-link text-body fs-5 fw-semibold text-decoration-none p-0 border-0",
        title = "Show the welcome intro"),
      actionButton(
        "btn_help",
        label = bsicons::bs_icon("question-circle"),
        class = "btn-link text-body-secondary p-0 border-0 ms-1",
        title = "Show the tour"),
      # sun / moon toggle — bslib swaps data-bs-theme on the page; the
      # server observer mirrors it onto the maplibre basemap.
      input_dark_mode(id = "dark_toggle", mode = "dark"),
      # copy a shareable link that reopens this cruise + measurement + cast
      # selection (url bookmarking; enabled in global.R)
      bookmarkButton(
        label = "Share",
        icon  = bsicons::bs_icon("share"),
        title = "Copy a link that reopens this cruise, measurement and cast selection",
        class = "btn-sm btn-link text-body-secondary p-0 border-0 ms-1"),
      # opens a showModal() feedback form (server.R) that posts to Google
      # Forms server-side — same header-icon-button family as Help/Share
      actionButton(
        "btn_feedback",
        label = tagList(bsicons::bs_icon("chat-square-text"), " Feedback"),
        class = "btn-sm btn-link text-body-secondary p-0 border-0 ms-1",
        title = "Send feedback")),
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
      selectizeInput(
        "sel_meas_type", "Measurement",
        choices  = meas_vec, selected = default_meas,
        width    = "100%",
        options  = list(placeholder = "Search or pick a measurement…")))),

  # main body — map (left), a drag handle, then the data panel (right), as
  # three flex children (not jQuery UI's resizable — that fought the flex
  # layout and the drag never actually resized anything). Plain mouse
  # tracking instead: click_map_resize_handle below sets the map column's
  # pixel width directly as the mouse moves, and the data column (a bslib
  # fill item) absorbs whatever's left automatically.
  div(
    class = "ctdviz-body html-fill-item html-fill-container",

    div(
      id    = "pane_map",
      class = "ctdviz-map-col",
      style = "flex: 0 0 auto; width: 55%; height: 100%;",
      maplibreOutput("map_cruise", height = "100%")),

    div(id = "ctdviz_resize_handle", class = "ctdviz-resize-handle",
        title = "Drag to resize"),

    # data column — Parameters / Casts / Readings / Plot. our own
    # #btn_expand_data (top-right of the nav bar) handles full-screen now,
    # not bslib's native full_screen — that one sits bottom-right of the
    # card and was overlapping the DT pagination controls there.
    navset_card_underline(
      id          = "subtabs",
      selected    = "Parameters",  # land here first, not an empty Casts table
      height      = "100%",
      full_screen = FALSE,

      # first tab in the row — Cruise, then Parameter, is the natural order
      # someone actually works in, even though nothing here enforces it (any
      # tab is reachable any time). unlike Parameters (~14 items, fits as a
      # flat list), cruises span 1949-present — too many to just scroll, so
      # this defaults to the most recent handful with a search box to reach
      # further back. was a hover tooltip on the Cruise dropdown before;
      # dropped that — it doesn't work on touch at all, which matters for a
      # field-use app, and something you set once per session still needs to
      # be easy to find the first time.
      nav_panel(
        "Cruises",
        div(
          id = "ctdviz_cruises_pane",
          div(
            class = "small text-muted mb-2",
            "Pick a cruise here — each one has its own stations and ",
            "map track."),
          uiOutput("ui_cruise_list"))),

      # starter pane — every available parameter, flat and clickable, so
      # what's in this cruise is visible before doing anything else. Picking
      # one here is the same action as the Measurement dropdown up top (it
      # updates that dropdown directly); it doesn't gate the other tabs —
      # they're reachable any time — it's just what's shown by default.
      nav_panel(
        "Parameters",
        div(
          id = "ctdviz_params_pane",
          div(
            class = "small text-muted mb-3",
            "Pick a parameter here, then click stations on the map to ",
            "choose which casts to view."),
          uiOutput("ui_param_list"))),

      nav_panel(
        "Casts",
        # one-line header that alternates between click-to-select instructions
        # (initially / after Reset) and a Reset · count · Download bar (after
        # a selection or anchor is started). server-side renderUI swap.
        div(class = "mt-2 mb-2", uiOutput("ui_sel_bar")),
        DTOutput("tbl_casts")),

      nav_panel(
        "Readings",
        div(
          class = "d-flex align-items-center gap-2 flex-wrap small mt-2 mb-2",
          span(class = "text-muted", textOutput("txt_values_caption", inline = TRUE)),
          div(class = "ms-auto", uiOutput("ui_download_readings", inline = TRUE))),
        DTOutput("tbl_values")),

      nav_panel(
        "Plot",
        plotlyOutput("plot_transect", height = "100%")),

      # advanced settings — tucked into a gear popover on the right of the
      # nav bar. Bathymetry (map overlay) + Max depth (plot y-cap) are both
      # lesser-priority controls that don't need to be on screen always.
      nav_spacer(),
      # takes this panel full-screen — see the .ctdviz-data-col.is-expanded
      # CSS + click handler in tags$head above. Plain click-to-toggle rather
      # than bslib's built-in card fullscreen icon, which sits low-opacity
      # in a corner and is easy to miss — this stays visible and labelled.
      # both icons are always in the DOM; CSS shows/hides them based on the
      # ancestor .ctdviz-data-col's is-expanded class, so the icon swaps to
      # an X (clear "back out" affordance) without any JS icon-swapping.
      nav_item(
        tags$button(
          id    = "btn_expand_data",
          class = "btn btn-sm btn-link text-body-secondary",
          title = "Expand to full screen",
          tags$span(class = "ctdviz-icon-expand",
                     bsicons::bs_icon("arrows-angle-expand")),
          tags$span(class = "ctdviz-icon-collapse",
                     bsicons::bs_icon("x-lg")))),
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
            "Measurements table. Leave at the maximum to fit the data.")))) |>
      # navset_card_underline() doesn't take a class= argument itself (its
      # ... is reserved for nav_panel()/nav_item()), so the layout class is
      # added onto the returned tag afterward instead.
      htmltools::tagAppendAttributes(class = "ctdviz-data-col")
  )
)
