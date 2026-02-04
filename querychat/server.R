# server.R - querychat app for CalCOFI database ----

library(shiny)

function(input, output, session) {

  # reactive values ----
  rv <- reactiveValues(
    history = tibble(
      timestamp = character(),
      question  = character(),
      sql       = character(),
      n_rows    = integer()))

  # querychat server ----
  qc_vals <- qc$server(id = "qc")


  # sample question clicks ----
  lapply(sample_questions[1:4], function(q) {
    id <- paste0("q_", digest::digest(q, algo = "md5"))
    observeEvent(input[[id]], {
      # send question to chat via custom javascript handler
      session$sendCustomMessage("submitChatQuestion", q)
    }, ignoreInit = TRUE)
  })

  # data results ----
  output$tbl_results <- DT::renderDataTable({
    d <- qc_vals$df()

    if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) {
      return(NULL)
    }

    # add to history if there's an applied filter
    sql_val <- qc_vals$sql()
    title_val <- qc_vals$title()

    if (nrow(d) > 0 && !is.null(sql_val) && sql_val != "") {
      new_entry <- tibble(
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        question  = title_val %||% "Applied filter",
        sql       = sql_val,
        n_rows    = nrow(d))

      if (nrow(rv$history) == 0 ||
          rv$history$sql[nrow(rv$history)] != sql_val) {
        rv$history <- bind_rows(rv$history, new_entry)
      }
    }

    DT::datatable(
      d,
      options = list(
        pageLength = 15,
        scrollX    = TRUE,
        dom        = "frtip"),
      filter   = "top",
      rownames = FALSE,
      class    = "compact stripe hover")
  })

  # sql display ----
  output$txt_sql <- renderText({
    sql <- qc_vals$sql()
    if (is.null(sql) || sql == "") {
      "-- Query results are shown in the chat sidebar.\n-- The Results tab shows the underlying data table.\n-- Use the table filters above to narrow down the data."
    } else {
      sql
    }
  })

  output$txt_title <- renderText({
    title <- qc_vals$title()
    if (is.null(title) || title == "") {
      "Showing full species_summary table"
    } else {
      title
    }
  })

  # update visualization inputs based on data columns ----
  observe({
    d <- qc_vals$df()
    req(d, nrow(d) > 0)

    cols     <- names(d)
    num_cols <- cols[sapply(d, is.numeric)]

    updateSelectInput(session, "sel_x", choices = cols, selected = cols[1])
    updateSelectInput(session, "sel_y",
                      choices  = c("(count)" = ".count", num_cols),
                      selected = if (length(num_cols) > 0) num_cols[1] else ".count")
    updateSelectInput(session, "sel_color",
                      choices  = c("(none)" = "", cols),
                      selected = "")
  })

  # chart output ----
  output$plt_chart <- renderPlotly({
    d <- qc_vals$df()
    req(d, nrow(d) > 0, input$sel_x, input$sel_y)

    x_col <- input$sel_x
    y_col <- input$sel_y
    color_col <- if (input$sel_color == "") NULL else input$sel_color

    # handle count aggregation
    if (y_col == ".count") {
      d_plot <- d |>
        count(!!sym(x_col), name = "count")
      y_col <- "count"
    } else {
      d_plot <- d
    }

    # limit categories for readability
    if (is.character(d_plot[[x_col]]) || is.factor(d_plot[[x_col]])) {
      top_vals <- d_plot |>
        count(!!sym(x_col), sort = TRUE) |>
        head(20) |>
        pull(!!sym(x_col))
      d_plot <- d_plot |>
        filter(!!sym(x_col) %in% top_vals)
    }

    # safely reorder for bar charts (only if y is numeric)
    x_for_bar <- if (is.numeric(d_plot[[y_col]])) {
      reorder(d_plot[[x_col]], -d_plot[[y_col]])
    } else {
      d_plot[[x_col]]
    }

    p <- switch(
      input$sel_chart_type,

      "Bar" = {
        if (is.null(color_col)) {
          ggplot(d_plot, aes(x = x_for_bar, y = !!sym(y_col))) +
            geom_col(fill = "#2C3E50")
        } else {
          ggplot(d_plot, aes(x = x_for_bar, y = !!sym(y_col), fill = !!sym(color_col))) +
            geom_col()
        }
      },

      "Line" = {
        if (is.null(color_col)) {
          ggplot(d_plot, aes(x = !!sym(x_col), y = !!sym(y_col))) +
            geom_line(color = "#2C3E50") +
            geom_point(color = "#2C3E50")
        } else {
          ggplot(d_plot, aes(x = !!sym(x_col), y = !!sym(y_col), color = !!sym(color_col))) +
            geom_line() +
            geom_point()
        }
      },

      "Scatter" = {
        if (is.null(color_col)) {
          ggplot(d_plot, aes(x = !!sym(x_col), y = !!sym(y_col))) +
            geom_point(color = "#2C3E50", alpha = 0.6)
        } else {
          ggplot(d_plot, aes(x = !!sym(x_col), y = !!sym(y_col), color = !!sym(color_col))) +
            geom_point(alpha = 0.6)
        }
      },

      "Histogram" = {
        if (is.null(color_col)) {
          ggplot(d_plot, aes(x = !!sym(x_col))) +
            geom_histogram(fill = "#2C3E50", bins = 30)
        } else {
          ggplot(d_plot, aes(x = !!sym(x_col), fill = !!sym(color_col))) +
            geom_histogram(bins = 30, position = "dodge")
        }
      }
    )

    p <- p +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
      labs(x = input$sel_x, y = if (input$sel_y == ".count") "count" else input$sel_y)

    ggplotly(p) |>
      layout(legend = list(orientation = "h", y = -0.2))
  })

  # map output ----
  output$map_results <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -120, lat = 33, zoom = 6)
  })

  # update map when data changes ----
  observe({
    d <- qc_vals$df()
    req(d, nrow(d) > 0)

    # look for coordinate columns
    lat_col <- NULL
    lon_col <- NULL

    # common latitude column names
    lat_names <- c("latitude", "lat", "y", "latidude")
    lon_names <- c("longitude", "lon", "lng", "x", "long")

    for (nm in lat_names) {
      if (nm %in% names(d)) {
        lat_col <- nm
        break
      }
    }
    for (nm in lon_names) {
      if (nm %in% names(d)) {
        lon_col <- nm
        break
      }
    }

    # only update map if we have coordinates
    if (!is.null(lat_col) && !is.null(lon_col)) {
      d_map <- d |>
        filter(
          !is.na(!!sym(lat_col)),
          !is.na(!!sym(lon_col))) |>
        head(5000) # limit points for performance

      if (nrow(d_map) > 0) {
        # create popup content
        popup_cols <- setdiff(names(d_map), c(lat_col, lon_col))[1:min(5, ncol(d_map) - 2)]
        popup_content <- apply(d_map, 1, function(row) {
          paste(
            paste0("<b>", popup_cols, ":</b> ", row[popup_cols]),
            collapse = "<br>")
        })

        leafletProxy("map_results", session) |>
          clearMarkers() |>
          clearMarkerClusters() |>
          addCircleMarkers(
            data    = d_map,
            lng     = ~get(lon_col),
            lat     = ~get(lat_col),
            radius  = 5,
            color   = "#2C3E50",
            fillOpacity = 0.6,
            popup   = popup_content,
            clusterOptions = markerClusterOptions())
      }
    }
  })

  # schema tab ----

  # render mermaid ERD from file
  output$erd_diagram <- DiagrammeR::renderDiagrammeR({
    DiagrammeR::mermaid(readLines(here::here("querychat/schema.mmd")) |> paste(collapse = "\n"))
  })

  # tables list
  output$tbl_tables <- DT::renderDataTable({
    DT::datatable(
      d_tbls,
      options  = list(
        pageLength = 20,
        dom        = "ft"),
      colnames = c("Table Name", "Description"),
      rownames = FALSE,
      selection = "single",
      class     = "compact stripe hover")
  })

  # update table selector
  observe({
    updateSelectInput(
      session,
      "sel_table",
      choices  = d_flds$table_name |> unique() |> sort(),
      selected = "species_summary")
  })

  # column definitions for selected table
  output$tbl_columns <- DT::renderDataTable({
    req(input$sel_table)

    d_cols <- d_flds |>
      filter(table_name == input$sel_table) |>
      select(-table_name)

    DT::datatable(
      d_cols,
      options  = list(
        pageLength = 20,
        dom        = "t"),
      colnames = c("Column Name", "Type", "Description"),
      rownames = FALSE,
      class    = "compact stripe hover")
  })

  # history tab ----
  output$tbl_history <- DT::renderDataTable({
    DT::datatable(
      rv$history |> arrange(desc(timestamp)),
      options = list(
        pageLength = 20,
        scrollX    = TRUE,
        dom        = "frtip"),
      colnames = c("Timestamp", "Question", "SQL Query", "Rows"),
      rownames = FALSE,
      selection = "single",
      class     = "compact stripe hover")
  })

  # clear history button
  observeEvent(input$btn_clear_history, {
    rv$history <- tibble(
      timestamp = character(),
      question  = character(),
      sql       = character(),
      n_rows    = integer())
  })

  # click on history row to rerun query
  observeEvent(input$tbl_history_rows_selected, {
    req(input$tbl_history_rows_selected)

    idx <- input$tbl_history_rows_selected
    history_sorted <- rv$history |> arrange(desc(timestamp))

    if (idx <= nrow(history_sorted)) {
      sql <- history_sorted$sql[idx]
      # set the SQL to rerun the query
      qc_vals$sql(sql)
      # navigate to data tab
      updateNavbarPage(session, "nav", selected = "nav_data")
    }
  })

  # cleanup on session end ----
  session$onSessionEnded(function() {
    # connection cleanup handled by global.R onStop
  })
}
