# server.R — ctd-qaqc

function(input, output, session) {

  rv <- reactiveValues(
    results = NULL, summary = NULL, review_bump = 0,
    # profile state: the flagged depth a finding carried, the rule it came from,
    # and the scan the reviewer has selected (written by BOTH the plot click and
    # the table selection, read by both — one piece of state, so they cannot
    # chase each other round a loop)
    flag_depth = NA_real_, flag_key = NULL, flag_rule = NULL, sel_scan = NULL,
    # a cast/type requested by a finding, applied once its choices exist
    want_cast = NULL, want_type = NULL,
    # upload results, kept apart from the release run so the two are
    # never confused in the UI
    up_summary = NULL, up_results = NULL)

  # -- run rules in the background ---------------------------------------------
  # ExtendedTask so a multi-second scan never freezes the session. The worker CANNOT
  # inherit `con` — a DuckDB handle is not portable across processes — so it opens
  # its own read-only handle and closes it. Everything it needs (db path, rules dir)
  # is passed by value.
  run_task <- ExtendedTask$new(function(db_path, rules_dir, keys, app_dir, cruise_key) {
    future::future(
      {
        suppressMessages({library(DBI); library(duckdb); library(dplyr)})
        source(file.path(app_dir, "R/rules.R"))
        cw <- dbConnect(duckdb::duckdb(dbdir = db_path, read_only = TRUE))
        on.exit(try(dbDisconnect(cw, shutdown = TRUE), silent = TRUE), add = TRUE)
        dbExecute(cw, "SET threads TO 2")

        rl <- qc_read_rules(rules_dir, active_only = TRUE) |>
          filter(rule_key %in% keys)
        pt <- qc_present_types(cw)
        sv  <- if (nzchar(cruise_key)) list(cruise_key = cruise_key) else list()
        res <- qc_run_all(cw, rl, limit = 500L, present_types = pt, scope_values = sv)
        list(results = res, summary = qc_summarize(res, rl))
      },
      seed = TRUE,
      globals = list(db_path = db_path, rules_dir = rules_dir, keys = keys,
                     app_dir = app_dir, cruise_key = cruise_key))
  }) |> bind_task_button("run")

  observeEvent(input$run, {
    req(length(input$sel_rules) > 0)
    run_task$invoke(db_file, rules_dir, input$sel_rules, app_dir,
                    input$cruise %||% "")
  })

  observeEvent(run_task$result(), {
    out <- run_task$result()
    rv$results <- out$results
    rv$summary <- out$summary
    showNotification(
      glue("{nrow(out$summary)} rule(s) run — ",
           "{sum(out$summary$status %in% c('FAIL','flag'))} with findings, ",
           "{sum(out$summary$status == 'skip')} skipped"),
      type = "message")
  })

  # -- summary ------------------------------------------------------------------
  output$tbl_summary <- renderDT({
    s <- rv$summary
    validate(need(!is.null(s), "Select rules and press Run."))
    s |>
      select(rule_key, status, n, severity, rule_type, any_of("scope"),
             description, elapsed_s, note) |>
      datatable(
        rownames  = FALSE, selection = "single",
        options   = list(pageLength = 15, scrollX = TRUE, dom = "tip")) |>
      formatStyle(
        "status",
        color = styleEqual(
          c("pass", "flag", "FAIL", "skip", "ERROR"),
          c("#1a7f37", "#9a6700", "#d03b3b", "#6c757d", "#d03b3b")),
        fontWeight = styleEqual(
          c("pass", "flag", "FAIL", "skip", "ERROR"),
          c("normal", "bold", "bold", "normal", "bold")))
  })

  # the rule whose findings are shown: whichever summary row is selected
  sel_rule <- reactive({
    s <- rv$summary; req(s)
    i <- input$tbl_summary_rows_selected
    if (is.null(i) || !length(i)) return(NULL)
    s$rule_key[i]
  })

  sel_findings <- reactive({
    k <- sel_rule(); req(k)
    r <- keep(rv$results, \(x) identical(x$rule_key, k))
    if (!length(r)) return(NULL)
    r[[1]]$findings
  })

  output$findings_header <- renderUI({
    k <- sel_rule()
    if (is.null(k)) return("Findings")
    s <- filter(rv$summary, rule_key == k)
    tagList(
      tags$strong(k), " — ",
      tags$span(class = "text-muted",
                glue("{s$n} finding(s); showing up to 500. {s$description}")))
  })

  output$tbl_findings <- renderDT({
    f <- sel_findings()
    validate(need(!is.null(f) && nrow(f) > 0,
                  "Select a rule in the Summary tab that has findings."))
    # scrollY, not just pagination: `detail` is a full sentence, so a dozen rows
    # can be taller than the viewport and push the verdict controls and the
    # "See it in the profile" button off-screen with no way to scroll to them.
    # Bounding the table's own height keeps the actions reachable.
    datatable(f, rownames = FALSE, selection = "single",
              # scroller = TRUE is deliberately NOT set: that option needs the
              # Scroller extension, which is not loaded, and DataTables then fails
              # to redraw on a data change — the header updated to the new rule
              # while the table kept showing the previous rule's findings
              options = list(pageLength = 25, scrollX = TRUE, scrollY = "320px",
                             dom = "tip"))
  })

  # -- profile ------------------------------------------------------------------
  # The Profile tab is addressable on its own (cruise -> cast -> measurement), so a
  # flagged cast is a shareable URL and not something reachable only by re-running
  # a rule. Selecting a finding drives the same three inputs.

  cruise_casts <- reactive({
    req(input$prof_cruise)
    qc_cruise_casts(input$prof_cruise)
  })

  # cruise -> cast choices.
  #
  # `rv$want_cast` is a reactive DEPENDENCY, not a convenience: a finding on the
  # cruise already selected would not change `input$prof_cruise`, so an observer
  # keyed only on the cruise would never fire and the requested cast would be
  # silently ignored. Depending on the request itself makes it fire either way,
  # which is also why this needs no `later()` timing hack to sequence the two
  # updates.
  observe({
    d    <- cruise_casts()
    want <- rv$want_cast
    req(nrow(d) > 0)
    lab <- paste0(coalesce(d$grid_key, "?"), "  ", qc_dir_label(d$sample_key))
    sel <- if (!is.null(want) && want %in% d$sample_key) want
           else if (isTRUE(isolate(input$prof_cast) %in% d$sample_key))
             isolate(input$prof_cast)
           else d$sample_key[1]
    updateSelectInput(session, "prof_cast",
                      choices = setNames(d$sample_key, lab), selected = sel)
    if (!is.null(want) && want %in% d$sample_key) rv$want_cast <- NULL
  })

  # cast -> the types IT recorded (never the whole registry: offering a type this
  # cast never measured produces an empty plot with no explanation)
  observe({
    ck   <- input$prof_cast
    want <- rv$want_type
    req(nzchar(ck %||% ""))
    ty <- order_profile_types(qc_cast_types(ck))
    req(length(ty) > 0)
    sel <- if (!is.null(want) && want %in% ty) want
           else if (isTRUE(isolate(input$prof_type) %in% ty))
             isolate(input$prof_type)
           else ty[1]
    updateSelectInput(session, "prof_type", choices = ty, selected = sel)
    if (!is.null(want) && want %in% ty) rv$want_type <- NULL
  })

  # the finding currently selected in the Findings tab, if any
  sel_finding <- reactive({
    f <- sel_findings(); i <- input$tbl_findings_rows_selected
    if (is.null(f) || !length(i) || !nrow(f)) return(NULL)
    as.list(f[i, , drop = FALSE])
  })

  # finding -> profile. Everything is optional except subject_key: a rule that
  # targets `sample` has no depth or measurement type, and must still be plottable.
  observeEvent(input$go_profile, {
    fd <- sel_finding()
    if (is.null(fd) || !nzchar(fd$subject_key %||% "")) {
      showNotification("Select a finding row first.", type = "warning")
      return()
    }
    # not every rule returns cruise_key (the sample-level ones do not), so fall
    # back to the lookup rather than leaving the map and the partition unscoped
    ck <- fd$cruise_key
    if (is.null(ck) || is.na(ck[1]) || !nzchar(as.character(ck[1])))
      ck <- dbGetQuery(con, "SELECT cruise_key FROM sample WHERE sample_key = ?",
                       params = list(fd$subject_key))$cruise_key

    rv$want_cast  <- fd$subject_key
    rv$want_type  <- if (nzchar(fd$measurement_type %||% ""))
      fd$measurement_type else NULL
    rv$flag_depth <- suppressWarnings(as.numeric(fd$depth_min_m %||% NA))
    rv$flag_key   <- fd$subject_key
    rv$flag_rule  <- sel_rule()

    if (length(ck) && nzchar(as.character(ck[1])))
      updateSelectInput(session, "prof_cruise", selected = ck[1])
    nav_select("tabs", "Profile")
  })

  # a flagged depth belongs to the finding that produced it — clearing it when the
  # reviewer navigates elsewhere stops a red ring persisting onto an unrelated cast
  observeEvent(list(input$prof_cast, input$prof_type), {
    fd <- isolate(sel_finding())
    if (is.null(fd) || !identical(fd$subject_key %||% "", input$prof_cast) ||
        (nzchar(fd$measurement_type %||% "") &&
         !identical(fd$measurement_type, input$prof_type))) {
      rv$flag_depth <- NA_real_
      rv$flag_key   <- NULL
      rv$flag_rule  <- NULL
    }
  }, ignoreInit = TRUE)

  profile <- reactive({
    req(input$prof_cast, input$prof_type)
    calcofi4db::qc_cast_profile(
      con, input$prof_cast, measurement_types = input$prof_type,
      cruise_key = input$prof_cruise) |>
      as_tibble() |>
      arrange(cast_dir, depth_m) |>
      mutate(.row = row_number())
  })

  output$profile_header <- renderUI({
    d <- profile()
    if (!nrow(d)) return("Profile")
    ndir  <- n_distinct(d$cast_dir)
    # a preliminary cast is not a defective one: the source warns that oxygen,
    # nitrate and chlorophyll may move significantly after post-cruise
    # calibration, so a reviewer must see this before calling a value wrong
    stage <- qc_cast_stage(input$prof_cast)
    tagList(
      tags$strong(input$prof_type), " — ",
      tags$span(class = "text-muted", glue(
        "{nrow(d)} scans, {ndir} direction(s), ",
        "{round(min(d$depth_m, na.rm = TRUE))}–",
        "{round(max(d$depth_m, na.rm = TRUE))} m")),
      if (isTRUE(stage == "preliminary")) tags$span(
        class = "badge text-bg-warning ms-2",
        title = paste("Preliminary data, for non-publication use: oxygen,",
                      "nitrate and chlorophyll may change significantly after",
                      "post-cruise calibration."),
        "preliminary"),
      if (isTRUE(stage == "final")) tags$span(
        class = "badge text-bg-secondary ms-2",
        title = "Post-cruise calibrations applied (FinalQC).", "final"),
      if (!is.na(rv$flag_depth)) tags$span(
        class = "badge text-bg-danger ms-2",
        glue("flagged at {round(rv$flag_depth, 1)} m by {rv$flag_rule}")))
  })

  # the scan a click or a row selection points at — ONE piece of state, written by
  # both directions and read by both, which is what keeps them from chasing each
  # other round a loop
  observeEvent(event_data("plotly_click", source = "profile"), {
    e <- event_data("plotly_click", source = "profile")
    k <- suppressWarnings(as.integer(e$customdata))
    req(length(k) == 1, !is.na(k))
    if (!identical(rv$sel_scan, k)) rv$sel_scan <- k
  })

  observeEvent(input$tbl_profile_rows_selected, {
    i <- input$tbl_profile_rows_selected
    if (length(i) && !identical(rv$sel_scan, as.integer(i)))
      rv$sel_scan <- as.integer(i)
  }, ignoreNULL = FALSE)

  # a new cast or variable invalidates the selected scan — row 12 of one profile
  # has nothing to do with row 12 of another
  observeEvent(list(input$prof_cast, input$prof_type), { rv$sel_scan <- NULL })

  output$plot_profile <- renderPlotly({
    d <- profile()
    validate(need(nrow(d) > 0, "No scans for this cast and measurement."))
    if (!isTRUE(input$prof_show_up)) d <- filter(d, cast_dir != "up")

    p <- plot_ly(source = "profile")

    for (dir in intersect(c("down", "up"), unique(d$cast_dir))) {
      dd <- filter(d, cast_dir == dir)
      p <- add_trace(
        p, data = dd, x = ~measurement_value, y = ~depth_m,
        customdata = ~.row, type = "scatter", mode = "lines+markers",
        name = dir,
        line   = list(width = 1.5, color = DIR_COLOR[[dir]]),
        marker = list(size = 4, color = DIR_COLOR[[dir]]),
        hovertemplate = paste0(
          "%{x:.4g}<br>%{y:.1f} m<br>", dir, "cast<extra></extra>"))
    }

    # the flagged scan: ringed, not recoloured, so the profile still reads as a
    # profile. Matched on depth within half a metre — the finding carries the
    # depth, not the scan's row number.
    #
    # AND ON DIRECTION. Both casts have a scan at that depth, so a depth-only
    # match rings two points and implies the upcast was flagged when it was not.
    # The finding's subject_key names the direction, so use it.
    if (!is.na(rv$flag_depth)) {
      hit <- filter(d, abs(depth_m - rv$flag_depth) <= 0.5)
      if (!is.null(rv$flag_key) && any(hit$sample_key == rv$flag_key))
        hit <- filter(hit, sample_key == rv$flag_key)
      if (nrow(hit)) p <- add_trace(
        p, data = hit, x = ~measurement_value, y = ~depth_m,
        type = "scatter", mode = "markers", name = "flagged",
        marker = list(size = 13, color = "rgba(0,0,0,0)",
                      line = list(color = "#d03b3b", width = 2.5)),
        hovertemplate = "flagged<br>%{x:.4g} at %{y:.1f} m<extra></extra>")
    }

    # the scan the reviewer has selected, in either direction
    if (!is.null(rv$sel_scan)) {
      pick <- filter(d, .row == rv$sel_scan)
      if (nrow(pick)) p <- add_trace(
        p, data = pick, x = ~measurement_value, y = ~depth_m,
        type = "scatter", mode = "markers", name = "selected",
        marker = list(size = 10, color = "#0d6efd", symbol = "circle-open",
                      line = list(width = 2.5)),
        hovertemplate = "selected<br>%{x:.4g} at %{y:.1f} m<extra></extra>")
    }

    p |> layout(
      xaxis  = list(title = input$prof_type, side = "top"),
      # depth increases downward: the convention every oceanographer expects, and
      # getting it wrong makes a thermocline look like a bottom feature
      yaxis  = list(title = "depth (m)", autorange = "reversed"),
      legend = list(orientation = "h", y = -0.06),
      hovermode = "closest",
      margin = list(t = 40)) |>
      # registered LAST, on the fully built plot: registering on an empty plot_ly()
      # and then piping through add_trace() loses it, and the click handler then
      # silently never fires
      event_register("plotly_click")
  })

  output$tbl_profile <- renderDT({
    d <- profile()
    validate(need(nrow(d) > 0, "No scans for this cast and measurement."))
    d |>
      transmute(dir = cast_dir, `depth (m)` = round(depth_m, 2),
                value = signif(measurement_value, 6),
                qual = measurement_qual, time = datetime) |>
      datatable(rownames = FALSE, selection = "single",
                options = list(pageLength = 10, scrollX = TRUE, dom = "tip"))
  }, server = FALSE)

  # plot click -> table row. Kept as a proxy update so the table is not re-rendered
  # (which would drop the reviewer's page and scroll position).
  observeEvent(rv$sel_scan, {
    i <- rv$sel_scan
    req(!is.null(i))
    if (!identical(as.integer(input$tbl_profile_rows_selected %||% -1L), i))
      DT::selectRows(DT::dataTableProxy("tbl_profile"), i)
  })

  output$map_cast <- renderMaplibre({
    d <- qc_cruise_casts(input$prof_cruise) |>
      filter(!is.na(longitude), !is.na(latitude))
    validate(need(nrow(d) > 0, "No positions for this cruise."))
    sf_d <- st_as_sf(d, coords = c("longitude", "latitude"), crs = 4326) |>
      mutate(tooltip = glue("{grid_key}\n{sample_key}"))

    maplibre(style = carto_style("voyager")) |>
      add_navigation_control() |>
      fit_bounds(sf_d, animate = FALSE) |>
      add_circle_layer(
        id = "casts", source = sf_d, circle_color = "#4c78a8",
        circle_radius = 4, circle_opacity = 0.75,
        circle_stroke_color = "white", circle_stroke_width = 0.6,
        tooltip = "tooltip") |>
      add_circle_layer(
        # THE FILTER IS SET AT CREATION, not only by the proxy observer below.
        # The observer is ignoreInit, so on first render an unfiltered pin layer
        # rings EVERY cast on the cruise — which reads as "all of these are
        # selected" rather than as a bug.
        id = "pin", source = sf_d, circle_color = "#00000000",
        circle_radius = 9, circle_opacity = 0,
        circle_stroke_color = "#d03b3b", circle_stroke_width = 2.5,
        filter = list("==", list("get", "sample_key"),
                      isolate(input$prof_cast) %||% ""))
  })

  # ring the selected cast without re-rendering the map
  observeEvent(list(input$prof_cast, input$prof_cruise), {
    proxy <- maplibre_proxy("map_cast")
    proxy |> set_filter(
      "pin", list("==", list("get", "sample_key"), input$prof_cast %||% ""))
  }, ignoreInit = TRUE)

  # map click -> select that cast
  observeEvent(input$map_cast_feature_click, {
    k <- input$map_cast_feature_click$properties$sample_key
    req(k)
    updateSelectInput(session, "prof_cast", selected = k)
  }, ignoreInit = TRUE)

  output$prof_links <- renderUI({
    req(input$prof_cruise)
    tagList(
      hr(),
      # ctd-viz answers a different question — it interpolates ACROSS stations to
      # draw a section — so it is linked, not reimplemented
      tags$a(
        href = ctd_viz_url(cruise_key = input$prof_cruise),
        target = "_blank", rel = "noopener", class = "btn btn-sm btn-outline-primary w-100",
        bsicons::bs_icon("box-arrow-up-right"), " Section view in ctd-viz"),
      div(class = "small text-muted mt-2",
          "ctd-viz interpolates across stations along a line; this tab is one ",
          "cast at full resolution."))
  })

  # -- upload -------------------------------------------------------------------
  # The design principle that makes this cheap: every rule targets obs/sample, so
  # projecting an upload into that shape runs the whole registry unchanged. What
  # happens here is parsing and mapping — no rule knows the data came from a file.

  upload <- reactive({
    fi <- input$up_file
    req(fi)
    fmt <- tolower(tools::file_ext(fi$name))
    # fileInput gives the upload a temp name with NO EXTENSION, and the reader
    # dispatches on the extension — so it is pointed at a copy that keeps the
    # original name. The copy lives in its own directory so two uploads with the
    # same name in one session cannot collide, and so a caller that already has
    # the file at that path is not asked to copy it onto itself.
    dir_up <- file.path(tempdir(), "ctd-upload", as.integer(Sys.time()))
    dir.create(dir_up, recursive = TRUE, showWarnings = FALSE)
    p <- file.path(dir_up, fi$name)
    if (!normalizePath(fi$datapath, mustWork = FALSE) ==
        normalizePath(p, mustWork = FALSE))
      file.copy(fi$datapath, p, overwrite = TRUE)

    d <- try(calcofi4db::read_ctd_upload(p), silent = TRUE)
    if (inherits(d, "try-error"))
      return(list(error = trimws(sub("^Error[^:]*:", "", as.character(d)))))

    mapping <- calcofi4db::ctd_map_columns(
      names(d), d_meas_type, d_sbe_map,
      format = if (fmt == "csv") "csv" else fmt)
    core <- try(calcofi4db::ctd_upload_to_core(
      d, mapping, header = attr(d, "sbe_header") %||% list()), silent = TRUE)
    if (inherits(core, "try-error"))
      return(list(error = trimws(sub("^Error[^:]*:", "", as.character(core))),
                  mapping = mapping, data = d, name = fi$name, format = fmt))

    list(data = d, mapping = mapping, core = core, name = fi$name, format = fmt)
  })

  output$up_summary <- renderUI({
    u <- upload()
    if (!is.null(u$error)) return(div(
      class = "alert alert-danger py-2 px-3 mb-0",
      bsicons::bs_icon("exclamation-octagon"), " ", u$error))

    n_map <- sum(u$mapping$role == "measurement" & !is.na(u$mapping$measurement_type))
    n_un  <- sum(u$mapping$role == "unmapped")
    tagList(
      tags$strong(u$name), " ",
      tags$span(class = "badge text-bg-secondary", toupper(u$format)),
      tags$ul(
        class = "mt-2 mb-0",
        tags$li(glue("{format(nrow(u$data), big.mark = ',')} rows, ",
                     "{ncol(u$data)} columns")),
        tags$li(glue("{n_map} column(s) mapped to a measurement type; ",
                     "{n_un} unmapped")),
        tags$li(glue("projected to {format(nrow(u$core$obs), big.mark = ',')} ",
                     "observations across ",
                     "{length(unique(u$core$obs$measurement_type))} type(s)")),
        tags$li(HTML(glue(
          "<b>{u$core$n_sentinel}</b> value(s) dropped as missing or sentinel ",
          "(<code>-99</code>, <code>-9.99e-29</code>, blank)"))),
        tags$li(glue("cast {u$core$sample$sample_key}"))))
  })

  output$up_mapping <- renderDT({
    u <- upload()
    validate(need(!is.null(u$mapping), u$error %||% "Choose a file."))
    u$mapping |>
      transmute(column, role, measurement_type, units, note) |>
      datatable(rownames = FALSE,
                options = list(pageLength = 15, scrollX = TRUE, dom = "tip")) |>
      formatStyle("role",
                  color = styleEqual(
                    c("measurement", "unmapped", "voltage"),
                    c("#1a7f37", "#d03b3b", "#6e7781"), default = "#6e7781"))
  })

  up_task <- ExtendedTask$new(function(core, rules_dir, wf_dir, gebco) {
    future::future({
      suppressMessages({library(DBI); library(dplyr)})
      cw <- calcofi4db::qc_upload_con(core, wf_dir, gebco_tif = gebco)
      on.exit(try(dbDisconnect(cw, shutdown = TRUE), silent = TRUE), add = TRUE)
      rl  <- calcofi4db::qc_read_rules(rules_dir, active_only = TRUE)
      pt  <- calcofi4db::qc_present_types(cw, core$sample$dataset_key[1])
      res <- calcofi4db::qc_run_all(
        cw, rl, limit = 500L, present_types = pt,
        scope_values = list(cruise_key = core$sample$cruise_key[1]))
      list(results = res, summary = calcofi4db::qc_summarize(res, rl))
    }, seed = TRUE,
    globals = list(core = core, rules_dir = rules_dir, wf_dir = wf_dir,
                   gebco = gebco))
  }) |> bind_task_button("up_run")

  observeEvent(input$up_run, {
    u <- upload()
    if (is.null(u$core)) {
      showNotification(u$error %||% "Choose a file first.", type = "warning")
      return()
    }
    up_task$invoke(u$core, rules_dir, workflows_dir,
                   if (file.exists(gebco_tif)) gebco_tif else NULL)
  })

  observeEvent(up_task$result(), {
    out <- up_task$result()
    rv$up_summary <- out$summary
    rv$up_results <- out$results
    showNotification(glue(
      "{sum(out$summary$status %in% c('FAIL','flag'))} rule(s) with findings, ",
      "{sum(out$summary$status == 'skip')} skipped"), type = "message")
  })

  output$up_summary_tbl <- renderDT({
    s <- rv$up_summary
    validate(need(!is.null(s), "Upload a file and press Run."))
    s |>
      transmute(rule_key, status, findings = n, severity, type = rule_type,
                description, sec = elapsed_s, note) |>
      datatable(rownames = FALSE,
                options = list(pageLength = 20, scrollX = TRUE, dom = "tip")) |>
      formatStyle("status",
                  color = styleEqual(
                    c("pass", "flag", "FAIL", "ERROR", "skip"),
                    c("#1a7f37", "#b06000", "#d03b3b", "#d03b3b", "#6e7781")))
  })

  output$up_dl <- downloadHandler(
    filename = function() {
      u <- isolate(upload())
      glue("ctd-qaqc_upload-findings_{tools::file_path_sans_ext(u$name %||% 'file')}_{Sys.Date()}.csv")
    },
    content = function(file) {
      req(rv$up_results)
      all <- purrr::map(rv$up_results, \(r) {
        if (is.null(r$findings) || !nrow(r$findings)) return(NULL)
        mutate(r$findings, rule_key = r$rule_key, .before = 1)
      }) |> purrr::compact()
      readr::write_csv(
        if (length(all)) bind_rows(all) else tibble(rule_key = character()),
        file, na = "")
    })

  # -- record a verdict ---------------------------------------------------------
  observeEvent(input$save_verdict, {
    k <- sel_rule()
    f <- sel_findings()
    i <- input$tbl_findings_rows_selected

    # every guard here produces a NAMED reason: a review tool that silently does
    # nothing on click teaches reviewers to distrust it
    if (is.null(k) || is.null(f) || !length(i)) {
      output$verdict_msg <- renderUI(
        span(class = "text-danger ms-2", "Select a finding row first."))
      return()
    }
    if (!nzchar(str_trim(input$reviewer %||% ""))) {
      output$verdict_msg <- renderUI(
        span(class = "text-danger ms-2", "Enter a reviewer name — verdicts are attributable."))
      return()
    }

    subject <- as.character(f$subject_key[i])
    ok <- try(qc_write_verdict(
      rule_key = k, subject_key = subject, verdict = input$verdict,
      note = input$verdict_note, reviewer = str_trim(input$reviewer),
      release = release_version), silent = TRUE)

    if (inherits(ok, "try-error")) {
      output$verdict_msg <- renderUI(
        span(class = "text-danger ms-2", "Write failed — is another process holding the DB?"))
    } else {
      rv$review_bump <- rv$review_bump + 1
      updateTextInput(session, "verdict_note", value = "")
      output$verdict_msg <- renderUI(
        span(class = "text-success ms-2", glue("recorded: {subject}")))
    }
  })

  output$tbl_review <- renderDT({
    rv$review_bump
    d <- qc_read_verdicts()
    validate(need(nrow(d) > 0, "No verdicts recorded yet."))
    datatable(d, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  # -- registry -----------------------------------------------------------------
  output$tbl_rules <- renderDT({
    rules_all |>
      mutate(state = if_else(active, "active", "parked")) |>
      select(rule_key, state, rule_type, severity, target, any_of("scope"),
             description, requires_types, source_query, notes) |>
      datatable(rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE)) |>
      formatStyle("state",
                  color = styleEqual(c("active", "parked"), c("#1a7f37", "#6c757d")))
  })

  # -- download -----------------------------------------------------------------
  output$dl_findings <- downloadHandler(
    filename = function() glue("ctd-qaqc_findings_{release_version}_{Sys.Date()}.csv"),
    content  = function(file) {
      req(rv$results)
      # every finding from every rule that produced any, tagged with its rule
      all <- imap(rv$results, \(r, i) {
        if (is.null(r$findings) || !nrow(r$findings)) return(NULL)
        mutate(r$findings, rule_key = r$rule_key, .before = 1)
      }) |> compact()
      readr::write_csv(
        if (length(all)) bind_rows(all) else tibble(rule_key = character()),
        file, na = "")
    })
}
