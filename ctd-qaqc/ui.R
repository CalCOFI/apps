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
            # a finding is a key and a number; the profile is where a reviewer can
            # actually judge it, so getting there is one click from the row
            actionButton("go_profile", "See it in the profile",
                         class = "btn-outline-secondary ms-2",
                         icon = bsicons::bs_icon("graph-up")),
            uiOutput("verdict_msg", inline = TRUE)))),

      nav_panel(
        "Profile", icon = bsicons::bs_icon("graph-up"),
        layout_sidebar(
          sidebar = sidebar(
            width = 300, position = "right", open = "open",
            # independently addressable, so a flagged cast is a shareable link
            # rather than something you can only reach by re-running a rule
            selectInput("prof_cruise", "Cruise",
                        choices  = setNames(cruises$cruise_key, paste0(
                          cruises$cruise_key, "  (", cruises$n_casts, " casts)")),
                        selected = cruises$cruise_key[1]),
            selectInput("prof_cast", "Cast", choices = NULL),
            selectInput("prof_type", "Measurement", choices = NULL),
            checkboxInput("prof_show_up", "Overlay the upcast", TRUE),
            helpText(
              "Full-resolution scans from ", tags$code("obs_ctd_full"),
              " — the thinned ", tags$code("obs"), " keeps one direction and ",
              "roughly one sample per 10 m, so neither the up/down difference nor ",
              "a single-scan spike is visible there."),
            uiOutput("prof_links")),
          # each output gets its OWN card rather than sharing one. bslib cards are
          # fill containers: a plotlyOutput with a fixed pixel height inside a
          # shared card collapses to zero, which renders as a blank panel with no
          # error anywhere — the card is what should carry the height.
          layout_columns(
            col_widths = c(7, 5),
            card(
              full_screen = TRUE, height = 540,
              card_header(uiOutput("profile_header")),
              plotlyOutput("plot_profile")),
            card(
              full_screen = TRUE, height = 540,
              card_header("Cruise context"),
              maplibreOutput("map_cast"),
              card_footer(
                class = "small text-muted",
                "Casts on this cruise; the selected one is ringed. Click one to ",
                "switch to it."))),
          card(
            card_header("Scans"),
            p(class = "text-muted small px-3 mb-1",
              "Click a scan in the plot to select it in the table, or a table row ",
              "to highlight it in the plot. A flagged scan is ringed in red."),
            DTOutput("tbl_profile")))),

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
          # the protocol is GENERATED from this same registry, so the link cannot
          # go stale the way a hand-written help page would: what a rule checks,
          # where its threshold came from and what it cannot see are all harvested
          # from the rule's own SQL header at render time
          p(class = "text-muted small px-3",
            HTML(paste0(
              "For what each rule means — the reasoning, the threshold and how it ",
              "was derived, the known limitations, and how this compares with ",
              "QARTOD and <code>oce</code> — see the ",
              "<a href='https://calcofi.io/workflows/qc_protocol.html' ",
              "target='_blank' rel='noopener'>QA/QC protocol</a>, which is ",
              "generated from this registry."))),
          DTOutput("tbl_rules"))))
  )
}
