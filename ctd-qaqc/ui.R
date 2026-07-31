# ui.R — ctd-qaqc

function(request) {
  page_sidebar(
    title = tags$span(
      "CalCOFI CTD QA/QC",
      tags$small(class = "text-muted ms-2", glue("release {release_version}"))),
    theme = bs_theme(version = 5, preset = "shiny"),

    sidebar = sidebar(
      width = 330,
      title = "Run",

      helpText(
        glue("{format(n_obs, big.mark = ',')} CTD observations, ",
             "{nrow(rules_active)} active rule(s).")),

      # a skipped rule is not a passing rule; say so before anything is run
      if (n_skip > 0) div(
        class = "alert alert-warning py-2 px-2 small",
        bsicons::bs_icon("exclamation-triangle"), " ",
        glue("{n_skip} rule(s) will SKIP — their input measurement types are ",
             "absent from this release. A skip is not a pass.")),

      checkboxGroupInput(
        "sel_rules", "Rules to run",
        choiceNames  = rules_active$rule_key,
        choiceValues = rules_active$rule_key,
        selected     = rules_active$rule_key),

      # obs_ctd_full is 212M rows, hive-partitioned by cruise_key. The profile
      # rules are only meaningful per profile anyway, so they are run one cruise at
      # a time and SKIP (never pass) when no cruise is chosen.
      selectInput(
        "cruise", "Cruise (for full-resolution rules)",
        choices  = c("— none: full-resolution rules will skip —" = "",
                     setNames(cruises$cruise_key,
                              paste0(cruises$cruise_key, "  (", cruises$n_casts, " casts)"))),
        selected = ""),
      helpText(glue(
        "{n_cruise_scoped} rule(s) read the full-resolution scans and need a cruise; ",
        "the rest run over the whole CTD slice.")),

      input_task_button("run", "Run selected", icon = bsicons::bs_icon("play-fill")),
      hr(),
      textInput("reviewer", "Reviewer", placeholder = "your name"),
      helpText("Recorded with every verdict so a review is attributable."),
      hr(),
      downloadButton("dl_findings", "Download findings (CSV)", class = "btn-sm w-100")),

    navset_card_tab(
      id = "tabs",

      nav_panel(
        "Summary", icon = bsicons::bs_icon("list-check"),
        card(
          card_header("Rule results"),
          p(class = "text-muted small px-3",
            "Counts are computed over the full result set; the findings table below ",
            "is capped for display, so a large count is never understated."),
          DTOutput("tbl_summary"))),

      nav_panel(
        "Findings", icon = bsicons::bs_icon("bug"),
        card(
          card_header(uiOutput("findings_header")),
          DTOutput("tbl_findings"),
          hr(),
          layout_columns(
            col_widths = c(4, 8),
            selectInput("verdict", "Verdict",
                        c("accepted (real problem)"   = "accepted",
                          "rejected (not a problem)"  = "rejected",
                          "needs more information"    = "needs_info")),
            textInput("verdict_note", "Note", width = "100%",
                      placeholder = "why — this is the part a future reader needs")),
          div(
            actionButton("save_verdict", "Record verdict",
                         class = "btn-primary", icon = bsicons::bs_icon("check2")),
            uiOutput("verdict_msg", inline = TRUE)))),

      nav_panel(
        "Review log", icon = bsicons::bs_icon("journal-text"),
        card(card_header("Recorded verdicts"), DTOutput("tbl_review"))),

      nav_panel(
        "Rules", icon = bsicons::bs_icon("card-checklist"),
        card(
          card_header("Registry"),
          p(class = "text-muted small px-3",
            HTML(paste0(
              "Rules are data, not code — they live in the workflows repo at ",
              "<code>metadata/qc_rules/</code> so they version with the pipeline ",
              "that produces the data they check. Parked rules are listed with the ",
              "reason they cannot run yet."))),
          DTOutput("tbl_rules"))))
  )
}
