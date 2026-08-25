# ui.R — ctd-qaqc

function(request) {
  # page_sidebar() spelled out (page_fillable + layout_sidebar, same arguments it
  # passes) so the calcofi.io brand header can be the page's top bar instead of
  # bslib's title: logo -> calcofi.io, the release, and the dark/light switch
  # (id "dark_toggle"; the map and profile plot follow it in server.R)
  page_fillable(
    padding = 0, gap = 0,
    class = "bslib-page-sidebar",
    theme = bs_theme(version = 5, preset = "shiny"),

    tags$head(calcofi4r::cc_brand_head("CalCOFI CTD QA/QC")),
    calcofi4r::cc_brand_header(
      "CalCOFI CTD QA/QC",
      release  = release_version,
      mode     = calcofi4r::cc_theme(request)),

    layout_sidebar(
    fillable = TRUE, border = FALSE, border_radius = FALSE,

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
            card_header("Cast span — every variable this cast recorded"),
            p(class = "text-muted small px-3 mb-1",
              "Full-resolution min/max across both directions, with the declared ",
              tags$code("valid_min"), "/", tags$code("valid_max"),
              " beside it; anything outside is red. ",
              tags$strong("Orientation, not a check:"),
              " this is recomputed from the values we published, so a corrupted ",
              "value just widens the span and cannot disagree with itself. The ",
              "real cross-check is against the processor's own ",
              tags$code("YYMM_span_###-###.csv"),
              ", which is not yet ingested (question Q15)."),
            DTOutput("tbl_span")),
          card(
            card_header("Scans"),
            p(class = "text-muted small px-3 mb-1",
              "Click a scan in the plot to select it in the table, or a table row ",
              "to highlight it in the plot. A flagged scan is ringed in red."),
            DTOutput("tbl_profile")))),

      nav_panel(
        "Upload", icon = bsicons::bs_icon("upload"),
        layout_sidebar(
          sidebar = sidebar(
            width = 330, position = "right", open = "open",
            fileInput("up_file", "CTD file",
                      accept = UPLOAD_ACCEPT, buttonLabel = "Browse…"),
            helpText(
              HTML(paste0(
                "<b>.csv</b> CalCOFI cast file · <b>.cnv</b> Sea-Bird converted ",
                "(preferred — its header names every column) · <b>.asc</b> / ",
                "<b>.btl</b> Sea-Bird ASCII and bottle summary."))),
            div(class = "small text-muted",
                bsicons::bs_icon("info-circle"), " ",
                HTML(paste0(
                  "<b>.hex is not accepted.</b> It is raw A/D counts; converting ",
                  "it needs the instrument configuration (.xmlcon) with the ",
                  "calibration coefficients. Run SBE Data Processing and upload ",
                  "the .cnv."))),
            hr(),
            input_task_button("up_run", "Run all rules on this file",
                              icon = bsicons::bs_icon("play-fill")),
            hr(),
            div(class = "small text-muted",
                "Nothing here touches the release. The file is projected into ",
                tags$code("obs"), " / ", tags$code("sample"), " in an in-memory ",
                "database that dies with this session.")),
          card(
            card_header("File"),
            uiOutput("up_summary")),
          card(
            card_header("Column mapping"),
            p(class = "text-muted small px-3 mb-1",
              HTML(paste0(
                "Every column, and what it became. <b>Unmapped columns are the ",
                "point of this table</b> — a renamed sensor or a new instrument ",
                "shows up here first. Raw voltages are unmapped deliberately: ",
                "which sensor a <code>V0</code> belongs to depends on the wiring."))),
            DTOutput("up_mapping")),
          card(
            card_header("Rule results"),
            p(class = "text-muted small px-3 mb-1",
              HTML(paste0(
                "The same registry the release is checked with, run unchanged — ",
                "every rule targets <code>obs</code>/<code>sample</code>, so ",
                "projecting the upload into that shape is all it takes. A rule ",
                "whose input this file does not carry reports <b>skip</b>, ",
                "never pass."))),
            DTOutput("up_summary_tbl"),
            downloadButton("up_dl", "Download findings (CSV)",
                           class = "btn-sm mt-2")))),

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
              "was derived, the known limitations, what the source documentation ",
              "says about the corrections behind these values, and how this ",
              "compares with QARTOD and <code>oce</code> — see the ",
              "<a href='https://calcofi.io/workflows/ctd-cast_qa-qc-protocol.html' ",
              "target='_blank' rel='noopener'>QA/QC protocol</a>, which is ",
              "generated from this registry."))),
          # the quality codes are the single most misreadable thing in this data,
          # and the app is where someone looks at a flag
          div(class = "alert alert-info py-2 px-3 mx-3 small",
              HTML(paste0(
                "<b>Quality codes are dataset-scoped.</b> The CTD files use ",
                "<code>0</code>/blank = good, <code>1</code> = use the primary ",
                "sensor, <code>2</code> = use the secondary, <code>8</code> = ",
                "questionable, <code>9</code> = bad <i>or</i> missing. ",
                "<code>1</code> and <code>2</code> are sensor-<i>selection</i> ",
                "instructions, not quality grades — and because <code>9</code> ",
                "covers \"bad\", a 9-flagged row that carries a number is bad data, ",
                "not a contradiction. The bottle database uses a different set ",
                "(including <code>6</code>, which has no CTD meaning)."))),
          DTOutput("tbl_rules")))))
  )
}
