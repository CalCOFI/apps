# datacheck server — pick a cruise, see every dataset's stations for it on a
# map + table + space-time plot, all colored by dataset. The whole view state
# (cruise, dataset filter, pinned observation) round-trips through the URL query
# string so any view is shareable — the feature ctd-viz lacks.
#
# state flow: the URL is the source of truth at load (parsed once on connect),
# then user actions (cruise change, pill toggle, map click) write back to the
# URL via updateQueryString. rv$pin holds the deep-linked observation key
# ("<dataset>:<id>").

server <- function(input, output, session) {

  rv <- reactiveValues(
    pending_datasets = NULL,   # dataset filter from the URL, applied once pills render
    pin              = NULL)    # pinned observation key "<dataset>:<id>"

  # --- parse the URL once on connect --------------------------------------
  observeEvent(session$clientData$url_search, once = TRUE, {
    q <- getQueryString(session)
    if (!is.null(q$cruise) && q$cruise %in% cruise_summary$cruise_key) {
      updateSelectInput(session, "sel_cruise", selected = q$cruise)
    }
    if (!is.null(q$datasets) && nzchar(q$datasets)) {
      rv$pending_datasets <- strsplit(q$datasets, ",")[[1]]
    }
    if (!is.null(q$id) && nzchar(q$id)) rv$pin <- q$id
  })

  # --- observations for the selected cruise -------------------------------
  obs_all <- reactive({
    req(input$sel_cruise)
    obs_for_cruise(input$sel_cruise)
  })

  # datasets actually present in this cruise, in palette order
  present_datasets <- reactive({
    d <- obs_all()
    # alphabetical, matching the schema site's chip order
    sort(intersect(names(dataset_pal), unique(d$dataset)))
  })

  # --- dataset filter pills (rebuilt per cruise) --------------------------
  output$ds_pills_ui <- renderUI({
    present <- present_datasets()
    req(length(present) > 0)
    # selected: URL filter (intersected with present) on first render, else all
    sel <- if (!is.null(rv$pending_datasets)) {
      intersect(rv$pending_datasets, present)
    } else present
    if (length(sel) == 0) sel <- present
    rv$pending_datasets <- NULL   # consume the URL hint after first use
    # label = the provider_dataset key (matches the schema-site chips); the
    # swatch color + pill styling are applied via CSS keyed on the input value
    checkboxGroupInput(
      "sel_datasets", label = NULL, inline = TRUE,
      choices  = present,
      selected = sel)
  })

  # filtered observations (by the dataset pills)
  obs_filt <- reactive({
    d   <- obs_all()
    sel <- input$sel_datasets
    if (is.null(sel)) sel <- present_datasets()
    d |> filter(dataset %in% sel)
  })

  # --- keep the URL in sync with the view ---------------------------------
  observe({
    cruise <- input$sel_cruise
    req(cruise)
    sel    <- input$sel_datasets
    present <- isolate(present_datasets())
    parts  <- c(cruise = cruise)
    # only record the dataset filter when it's a real subset (keeps URLs short)
    if (!is.null(sel) && length(sel) > 0 && !setequal(sel, present)) {
      parts["datasets"] <- paste(sel, collapse = ",")
    }
    if (!is.null(rv$pin)) parts["id"] <- rv$pin
    qs <- paste0("?", paste0(names(parts), "=", unname(parts), collapse = "&"))
    updateQueryString(qs, mode = "replace")
  })

  # --- map ----------------------------------------------------------------
  output$map_obs <- renderMaplibre({
    d  <- obs_filt()
    sf <- obs_to_sf(d)
    req(!is.null(sf))

    fit_target <- if (nrow(sf) == 1) {
      lon <- d$longitude[1]; lat <- d$latitude[1]; pad <- 0.5
      c(lon - pad, lat - pad, lon + pad, lat + pad)
    } else sf

    init_style <- isolate(
      if (identical(input$dark_toggle, "dark")) "dark-matter" else "voyager")

    maplibre(style = carto_style(init_style)) |>
      add_navigation_control() |>
      add_fullscreen_control() |>
      add_scale_control(position = "bottom-left") |>
      fit_bounds(fit_target) |>
      add_circle_layer(
        id = "obs", source = sf,
        circle_color        = get_column("color"),   # data-driven, per dataset
        circle_radius       = 5,
        circle_opacity      = 0.8,
        circle_stroke_color = "white",
        circle_stroke_width = 0.8,
        tooltip             = "tooltip") |>
      add_circle_layer(            # pinned observation — drawn on top, ringed
        id = "pin", source = sf,
        circle_color        = get_column("color"),
        circle_radius       = 9,
        circle_opacity      = 0,
        circle_stroke_color = "#111111",
        circle_stroke_width = 2.5)
  })

  # dark / light basemap swap (preserve the layers we added)
  observeEvent(input$dark_toggle, {
    style <- if (identical(input$dark_toggle, "dark")) "dark-matter" else "voyager"
    maplibre_proxy("map_obs") |> set_style(carto_style(style))
  }, ignoreInit = TRUE)

  # map click -> pin that observation (+ URL gains &id=)
  observeEvent(input$map_obs_feature_click, {
    key <- input$map_obs_feature_click$properties$obs_key
    req(key)
    rv$pin <- key
  }, ignoreInit = TRUE)

  # store -> map: light up the pinned observation via the proxy (no re-render)
  observeEvent(list(rv$pin, obs_filt()), {
    proxy <- maplibre_proxy("map_obs")
    if (is.null(rv$pin)) {
      proxy |> set_paint_property("pin", "circle-opacity", 0)
    } else {
      proxy |>
        set_filter("pin", list("==", list("get", "obs_key"), rv$pin)) |>
        set_paint_property("pin", "circle-opacity", 1)
    }
  }, ignoreInit = TRUE)

  # --- table --------------------------------------------------------------
  output$txt_table_caption <- renderText({
    d <- obs_filt()
    glue("{nrow(d)} observations across {length(unique(d$dataset))} dataset(s) ",
         "on cruise {input$sel_cruise}")
  })

  tbl_df <- reactive({
    obs_filt() |>
      transmute(
        dataset  = ds_label,
        id, site_key,
        datetime = if_else(is.na(datetime), NA_character_,
                           format(datetime, "%Y-%m-%d %H:%M")),
        latitude = round(latitude, 4),
        longitude = round(longitude, 4),
        obs_key)
  })

  output$tbl_obs <- DT::renderDT(
    {
      d <- tbl_df()
      req(nrow(d) > 0)
      d |> select(-obs_key)
    },
    selection = "single", rownames = FALSE, filter = "top",
    options = list(dom = "tlip", pageLength = 10, scrollX = TRUE,
                   order = list(list(0, "asc"))))
  outputOptions(output, "tbl_obs", suspendWhenHidden = FALSE)

  # table row click -> pin (mirrors a map click)
  observeEvent(input$tbl_obs_rows_selected, {
    key <- tbl_df()$obs_key[input$tbl_obs_rows_selected]
    req(length(key) == 1)
    rv$pin <- key
  }, ignoreInit = TRUE)

  # pin -> select the matching table row (skip if it triggered the change)
  observeEvent(rv$pin, {
    if (is.null(rv$pin)) return()
    rows <- which(tbl_df()$obs_key == rv$pin)
    if (length(rows) == 1) DT::dataTableProxy("tbl_obs") |> DT::selectRows(rows)
  }, ignoreInit = TRUE)

  # --- download -----------------------------------------------------------
  output$ui_download <- renderUI({
    req(nrow(obs_filt()) > 0)
    downloadButton("dl_data", "Download CSV", class = "btn-sm btn-link p-0")
  })
  output$dl_data <- downloadHandler(
    filename = function() glue("datacheck_{input$sel_cruise}.csv"),
    content  = function(file) {
      obs_filt() |>
        select(dataset, tbl, id, cruise_key, site_key, datetime,
               latitude, longitude) |>
        readr::write_csv(file)
    })

  # --- plot: space-time coverage (date x latitude, colored by dataset) ----
  output$plot_spacetime <- renderPlotly({
    d <- obs_filt() |> filter(!is.na(datetime))
    if (nrow(d) == 0) {
      return(plotly::plot_ly(type = "scatter", mode = "markers") |>
        plotly::layout(
          annotations = list(
            text = "No time-stamped observations for this selection.",
            showarrow = FALSE, xref = "paper", yref = "paper", x = 0.5, y = 0.5,
            font = list(color = "#888", size = 14)),
          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
    }
    present <- intersect(names(dataset_pal), unique(d$dataset))
    d <- d |> mutate(ds_label = factor(ds_label,
      levels = unname(dataset_label[present])))
    plotly::plot_ly(
      data    = d,
      x       = ~datetime, y = ~latitude,
      color   = ~ds_label,
      colors  = unname(dataset_pal[present]),
      type    = "scatter", mode = "markers",
      marker  = list(size = 8, opacity = 0.75,
                     line = list(color = "white", width = 0.5)),
      text    = ~paste0("<b>", ds_label, "</b><br>id: ", id,
                        "<br>site: ", site_key,
                        "<br>", format(datetime, "%Y-%m-%d %H:%M")),
      hovertemplate = "%{text}<extra></extra>") |>
      plotly::layout(
        xaxis  = list(title = "Date"),
        yaxis  = list(title = "Latitude (°N)"),
        legend = list(orientation = "h", y = -0.2)) |>
      plotly::config(displayModeBar = TRUE)
  })

  # --- tour ---------------------------------------------------------------
  start_tour <- function() {
    tryCatch(tour$init()$start(),
             error = function(e) message("tour failed: ", conditionMessage(e)))
  }
  observeEvent(input$tour_seen, once = TRUE, ignoreNULL = TRUE, {
    if (isTRUE(input$tour_seen)) return()
    start_tour()
    session$sendCustomMessage("datacheck_tour_seen", TRUE)
  })
  observeEvent(input$btn_help, start_tour())
}
