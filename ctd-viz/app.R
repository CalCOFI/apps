# CalCOFI CTD Data Visualization

# packages
librarian::shelf(
  bslib,
  DBI,
  dplyr,
  duckdb,
  glue,
  leaflet,
  lubridate,
  plotly,
  shiny,
  sf,
  tidyr
)

# Remote DuckDB connection
db_url <- "https://file.calcofi.io/data/calcofi-ctd.duckdb"

dir_db <- switch(
  Sys.info()["sysname"],
  Linux = "/share/public/data",
  Darwin = "~/My Drive/projects/calcofi/data/calcofi.org/ctd-cast/download"
)
db_local <- glue("{dir_db}/calcofi-ctd.duckdb")

# Download database if not already cached
if (!file.exists(db_local)) {
  download.file(db_url, db_local, mode = "wb")
}

# Connect to database
con <- dbConnect(duckdb::duckdb(), dbdir = db_local, read_only = TRUE)
dbExecute(con, "INSTALL spatial; LOAD spatial;")

# Get cruise list
cruise_list <- tbl(con, "ctd") |>
  distinct(cruise_id) |>
  arrange(cruise_id) |>
  collect() |>
  pull(cruise_id)

# Get numeric variable names for transect viz
numeric_vars <- tbl(con, "ctd") |>
  head(1) |>
  collect() |>
  select(where(is.numeric)) |>
  names() |>
  setdiff(c("event_num", "lat_dec", "lon_dec"))

# UI
ui <- page_navbar(
  title = "CalCOFI CTD Data Viewer",
  theme = bs_theme(bootswatch = "cosmo"),

  # Tab 1: Gantt Chart
  nav_panel(
    "Cruise Timeline",
    card(
      card_header("Interactive Gantt Chart"),
      plotlyOutput("gantt_chart", height = "700px")
    )
  ),

  # Tab 2: Map
  nav_panel(
    "Cruise Tracks",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "map_cruise",
          "Select Cruise(s):",
          choices = c("All" = "all", cruise_list),
          selected = "all",
          multiple = TRUE
        ),
        dateRangeInput(
          "map_date_range",
          "Date Range:",
          start = "1998-01-01",
          end = Sys.Date()
        ),
        actionButton("map_update", "Update Map", class = "btn-primary")
      ),
      card(
        card_header("Cruise Track Map"),
        leafletOutput("cruise_map", height = "700px")
      )
    )
  ),

  # Tab 3: Transect
  nav_panel(
    "Transect View",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "transect_cruise",
          "Select Cruise:",
          choices = cruise_list,
          selected = cruise_list[1]
        ),
        dateRangeInput(
          "transect_date_range",
          "Date Range (optional):",
          start = NULL,
          end = NULL
        ),
        selectInput(
          "transect_var",
          "Select Variable:",
          choices = numeric_vars,
          selected = "t_deg_c"
        ),
        selectInput(
          "transect_cast",
          "Cast Direction:",
          choices = c("Downcast" = "D", "Upcast" = "U"),
          selected = "D"
        ),
        actionButton(
          "transect_update",
          "Update Transect",
          class = "btn-primary"
        )
      ),
      card(
        card_header("Transect Visualization"),
        plotlyOutput("transect_plot", height = "700px")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # gantt_chart ----
  output$gantt_chart <- renderPlotly({
    cruise_spans <- tbl(con, "ctd") |>
      group_by(cruise_id, data_stage) |>
      summarize(
        beg_date = min(dtime_utc, na.rm = TRUE),
        end_date = max(dtime_utc, na.rm = TRUE),
        .groups = "drop"
      ) |>
      collect() |>
      filter(!is.na(beg_date), !is.na(end_date)) |>
      mutate(
        beg_date = as.Date(beg_date),
        end_date = as.Date(end_date)
      )

    # Split multi-year cruises
    cruise_spans <- cruise_spans |>
      rowwise() |>
      mutate(
        years_spanned = list(year(beg_date):year(end_date))
      ) |>
      unnest(years_spanned) |>
      mutate(
        year = years_spanned,
        adj_beg_date = if_else(
          years_spanned == year(beg_date),
          beg_date,
          as.Date(paste0(years_spanned, "-01-01"))
        ),
        adj_end_date = if_else(
          years_spanned == year(end_date),
          end_date,
          as.Date(paste0(years_spanned, "-12-31"))
        ),
        begin_jday = yday(adj_beg_date),
        end_jday = yday(adj_end_date),
        hover_text = glue(
          "Cruise: {cruise_id}<br>",
          "Stage: {data_stage}<br>",
          "Begin: {format(beg_date, \'%Y-%m-%d\')}<br>",
          "End: {format(end_date, \'%Y-%m-%d\')}<br>",
          "Duration: {as.numeric(difftime(end_date, beg_date, units = \'days\'))} days"
        )
      ) |>
      ungroup() |>
      arrange(adj_beg_date)

    colors <- c("final" = "#23d355ff", "preliminary" = "#A23B72")

    p <- plot_ly()

    for (i in 1:nrow(cruise_spans)) {
      row <- cruise_spans[i, ]
      p <- p |>
        add_trace(
          type = "scatter",
          mode = "lines",
          x = c(row$begin_jday, row$end_jday),
          y = c(row$year, row$year),
          line = list(
            color = colors[row$data_stage],
            width = 8
          ),
          text = row$hover_text,
          hoverinfo = "text",
          showlegend = FALSE,
          legendgroup = row$data_stage
        )
    }

    p <- p |>
      add_trace(
        type = "scatter",
        mode = "lines",
        x = c(NA, NA),
        y = c(NA, NA),
        line = list(color = colors["final"], width = 8),
        name = "Final",
        showlegend = TRUE
      ) |>
      add_trace(
        type = "scatter",
        mode = "lines",
        x = c(NA, NA),
        y = c(NA, NA),
        line = list(color = colors["preliminary"], width = 8),
        name = "Preliminary",
        showlegend = TRUE
      )

    p |>
      layout(
        title = "CalCOFI CTD Cruise Timeline",
        xaxis = list(
          title = "Day of Year",
          range = c(1, 365),
          dtick = 30
        ),
        yaxis = list(
          title = "Year",
          autorange = "reversed",
          dtick = 1
        ),
        hovermode = "closest",
        plot_bgcolor = "#f8f9fa",
        paper_bgcolor = "white"
      )
  })

  # map_data() ----
  map_data <- eventReactive(
    input$map_update,
    {
      query <- tbl(con, "ctd")

      # Filter by cruise
      if (!"all" %in% input$map_cruise) {
        query <- query |> filter(cruise_id %in% !!input$map_cruise)
      }

      # Filter by date range
      query <- query |>
        filter(
          dtime_utc >= !!input$map_date_range[1],
          dtime_utc <= !!input$map_date_range[2]
        )

      query |>
        distinct(cruise_id, dtime_utc, lon_dec, lat_dec) |>
        # filter(!is.na(lon_dec), !is.na(lat_dec)) |>
        arrange(cruise_id, dtime_utc) |>
        collect()
    },
    ignoreNULL = FALSE
  )

  # cruise_map ----
  output$cruise_map <- renderLeaflet({
    data <- map_data()
    # TODO: 5,563,426 points is too many for leaflet;
    # need to convert to line and simplify

    # Create color palette
    n_cruises <- n_distinct(data$cruise_id)
    pal <- colorFactor(
      palette = rainbow(n_cruises),
      domain = data$cruise_id
    )

    leaflet(data) |>
      addProviderTiles(providers$Esri.OceanBasemap) |>
      addCircleMarkers(
        lng = ~lon_dec,
        lat = ~lat_dec,
        color = ~ pal(cruise_id),
        radius = 3,
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = ~ paste0(
          "<b>Cruise:</b> ",
          cruise_id,
          "<br>",
          "<b>Date:</b> ",
          format(dtime_utc, "%Y-%m-%d %H:%M"),
          "<br>",
          "<b>Location:</b> ",
          round(lat_dec, 3),
          "°N, ",
          round(lon_dec, 3),
          "°W"
        )
      ) |>
      addLegend(
        "bottomright",
        pal = pal,
        values = ~cruise_id,
        title = "Cruise ID",
        opacity = 1
      )
  })

  # === TRANSECT ===
  transect_data <- eventReactive(
    input$transect_update,
    {
      query <- tbl(con, "ctd") |>
        filter(
          cruise_id == !!input$transect_cruise,
          cast_dir == !!input$transect_cast,
          !is.na(depth),
          !is.na(!!sym(input$transect_var))
        )

      # Filter by date if provided
      if (
        !is.null(input$transect_date_range[1]) &&
          !is.null(input$transect_date_range[2])
      ) {
        query <- query |>
          filter(
            dtime_utc >= !!input$transect_date_range[1],
            dtime_utc <= !!input$transect_date_range[2]
          )
      }

      query |>
        select(
          dtime_utc,
          lat_dec,
          lon_dec,
          depth,
          value = !!sym(input$transect_var)
        ) |>
        collect() |>
        mutate(
          station_num = as.integer(factor(dtime_utc))
        )
    },
    ignoreNULL = FALSE
  )

  output$transect_plot <- renderPlotly({
    data <- transect_data()

    if (nrow(data) == 0) {
      return(
        plot_ly() |>
          layout(
            title = "No data available for selected parameters",
            xaxis = list(title = "Station"),
            yaxis = list(title = "Depth (m)")
          )
      )
    }

    # Create contour plot
    plot_ly(
      data = data,
      x = ~station_num,
      y = ~ -depth,
      z = ~value,
      type = "contour",
      colorscale = "Viridis",
      reversescale = FALSE,
      contours = list(
        showlabels = TRUE
      ),
      colorbar = list(
        title = input$transect_var
      ),
      hovertemplate = paste(
        "<b>Station:</b> %{x}<br>",
        "<b>Depth:</b> %{y:.1f} m<br>",
        "<b>Value:</b> %{z:.2f}<br>",
        "<extra></extra>"
      )
    ) |>
      layout(
        title = glue(
          "Transect: {input$transect_cruise} - {input$transect_var}"
        ),
        xaxis = list(
          title = "Station Number"
        ),
        yaxis = list(
          title = "Depth (m)",
          autorange = "reversed"
        )
      )
  })

  # Cleanup on session end
  onStop(function() {
    dbDisconnect(con, shutdown = TRUE)
  })
}

# Run app
shinyApp(ui, server)
