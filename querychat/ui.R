# ui.R - querychat app for CalCOFI database ----
# note: qc (QueryChat instance) is created in global.R after views are created

library(shiny)

tagList(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    # handler to populate and submit chat input
    tags$script(HTML("
      Shiny.addCustomMessageHandler('submitChatQuestion', function(question) {
        var chatInput = document.querySelector('textarea[id*=\"user_input\"]') ||
                        document.querySelector('.chat-input textarea') ||
                        document.querySelector('textarea.form-control');
        if (chatInput) {
          chatInput.value = question;
          chatInput.dispatchEvent(new Event('input', { bubbles: true }));
          chatInput.dispatchEvent(new Event('change', { bubbles: true }));
          setTimeout(function() {
            var submitBtn = document.querySelector('button[type=\"submit\"]') ||
                            document.querySelector('.chat-input button') ||
                            document.querySelector('button.btn-primary');
            if (submitBtn) {
              submitBtn.click();
            }
          }, 150);
        }
      });
    "))),

page_navbar(
  id     = "nav",
  title  = "CalCOFI Database Explorer",
  theme  = theme,
  fillable = TRUE,

  # data tab ----
  nav_panel(
    title = "Data",
    icon  = bs_icon("table"),
    value = "nav_data",

    layout_sidebar(
      fillable = TRUE,

      sidebar = sidebar(
        width = 350,
        qc$ui(id = "qc"),
        hr(),
        # sample questions
        h6("Sample Questions:"),
        tags$ul(
          class = "list-unstyled small",
          lapply(sample_questions[1:4], function(q) {
            tags$li(
              actionLink(
                inputId = paste0("q_", digest::digest(q, algo = "md5")),
                label   = q,
                class   = "text-primary"))
          }))
      ),

      # main panel
      navset_card_tab(
        id = "data_tabs",

        # results table
        nav_panel(
          title = "Results",
          icon  = bs_icon("grid-3x3"),
          card_body(
            fillable = TRUE,
            p(class = "text-muted small",
              "Query results appear in the chat. This table shows the underlying data - use filters to explore."),
            DT::dataTableOutput("tbl_results"))
        ),

        # sql query
        nav_panel(
          title = "SQL",
          icon  = bs_icon("code-square"),
          card_body(
            fillable = TRUE,
            h5("Generated SQL Query"),
            verbatimTextOutput("txt_sql"),
            hr(),
            h5("Query Title"),
            textOutput("txt_title"))
        ),

        # visualization
        nav_panel(
          title = "Visualize",
          icon  = bs_icon("bar-chart"),
          card_body(
            fillable = TRUE,
            layout_column_wrap(
              width = 1/2,
              card(
                card_header("Chart"),
                card_body(
                  selectInput(
                    "sel_chart_type",
                    "Chart Type",
                    choices = c("Bar", "Line", "Scatter", "Histogram"),
                    selected = "Bar"),
                  selectInput("sel_x", "X Axis", choices = NULL),
                  selectInput("sel_y", "Y Axis", choices = NULL),
                  selectInput("sel_color", "Color By (optional)", choices = NULL))),
              card(
                card_header("Preview"),
                card_body(
                  plotlyOutput("plt_chart", height = "350px")))))
        ),

        # map
        nav_panel(
          title = "Map",
          icon  = bs_icon("geo-alt"),
          card_body(
            fillable = TRUE,
            leafletOutput("map_results", height = "100%"))
        )
      )
    )
  ),

  # schema tab ----
  nav_panel(
    title = "Schema",
    icon  = bs_icon("diagram-3"),
    value = "nav_schema",

    layout_column_wrap(
      width = 1/2,
      heights_equal = "row",

      # erd diagram
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Entity Relationship Diagram",
          popover(
            bs_icon("info-circle"),
            title = "About the ERD",
            "This diagram shows the relationships between tables in the CalCOFI database. Arrows indicate foreign key relationships.")),
        card_body(
          class = "erd-container",
          style = "min-height: 500px; overflow: auto;",
          DiagrammeR::DiagrammeROutput("erd_diagram", height = "500px"))
      ),

      # table list
      card(
        card_header("Database Tables"),
        card_body(
          fillable = TRUE,
          DT::dataTableOutput("tbl_tables"))
      )
    ),

    # column definitions
    card(
      card_header("Column Definitions"),
      card_body(
        fillable = TRUE,
        selectInput(
          "sel_table",
          "Select Table",
          choices  = NULL,
          selected = NULL,
          width    = "300px"),
        DT::dataTableOutput("tbl_columns"))
    )
  ),

  # history tab ----
  nav_panel(
    title = "History",
    icon  = bs_icon("clock-history"),
    value = "nav_history",

    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Query History",
        actionButton("btn_clear_history", "Clear History", class = "btn-sm btn-outline-danger")),
      card_body(
        fillable = TRUE,
        DT::dataTableOutput("tbl_history"))
    )
  ),

  # about tab ----
  nav_panel(
    title = "About",
    icon  = bs_icon("info-circle"),
    value = "nav_about",

    card(
      card_header("About CalCOFI Database Explorer"),
      card_body(
        markdown("
## Overview

This application allows you to explore the **California Cooperative Oceanic Fisheries Investigations (CalCOFI)** database using natural language queries powered by AI.

## Data Source

The CalCOFI database contains oceanographic and biological data from one of the longest-running fisheries and oceanographic time series in the world, dating back to 1949.

**Database URL:** [https://file.calcofi.io/data/calcofi.duckdb](https://file.calcofi.io/data/calcofi.duckdb)

## Key Features

- **Natural Language Queries**: Ask questions in plain English and get SQL queries automatically generated
- **Interactive Results**: View, sort, and filter query results in an interactive table
- **Data Visualization**: Create charts and maps from your query results
- **Schema Documentation**: Browse the database structure and column definitions

## Queryable Data

The chat interface queries the **species_summary** view which includes:
- **species_id**: CalCOFI species identifier
- **scientific_name**: Species scientific name
- **common_name**: Common name
- **itis_id**: ITIS taxonomic serial number
- **worms_id**: WoRMS AphiaID
- **total_larvae**: Sum of all larvae observations
- **larva_records**: Number of larva observation records
- **total_eggs**: Sum of all egg observations
- **egg_records**: Number of egg observation records

The Schema tab shows the full database structure with all tables.

## Technology

This app uses:
- [querychat](https://posit-dev.github.io/querychat/) for natural language to SQL conversion
- [DuckDB](https://duckdb.org/) for fast analytical queries
- Claude AI (claude-sonnet-4-5) for query generation

## Links

- [CalCOFI.org](https://calcofi.org) - Official CalCOFI website
- [CalCOFI.io](https://calcofi.io) - Data portal and documentation
        "))
    )
  ),

  # footer ----
  nav_spacer(),
  nav_item(
    tags$a(
      href   = "https://calcofi.org",
      target = "_blank",
      class  = "nav-link",
      bs_icon("box-arrow-up-right"),
      "CalCOFI.org"))
)
) # end tagList
