# server.R — ctd-qaqc

function(input, output, session) {

  rv <- reactiveValues(results = NULL, summary = NULL, review_bump = 0)

  # -- run rules in the background ---------------------------------------------
  # ExtendedTask so a multi-second scan never freezes the session. The worker CANNOT
  # inherit `con` — a DuckDB handle is not portable across processes — so it opens
  # its own read-only handle and closes it. Everything it needs (db path, rules dir)
  # is passed by value.
  run_task <- ExtendedTask$new(function(db_path, rules_dir, keys, app_dir) {
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
        res <- qc_run_all(cw, rl, limit = 500L, present_types = pt)
        list(results = res, summary = qc_summarize(res, rl))
      },
      seed = TRUE,
      globals = list(db_path = db_path, rules_dir = rules_dir,
                     keys = keys, app_dir = app_dir))
  }) |> bind_task_button("run")

  observeEvent(input$run, {
    req(length(input$sel_rules) > 0)
    run_task$invoke(db_file, rules_dir, input$sel_rules, app_dir)
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
      select(rule_key, status, n, severity, rule_type, description, elapsed_s, note) |>
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
    datatable(f, rownames = FALSE, selection = "single",
              options = list(pageLength = 12, scrollX = TRUE, dom = "tip"))
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
      select(rule_key, state, rule_type, severity, target, description,
             requires_types, source_query, notes) |>
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
