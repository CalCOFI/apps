librarian::shelf(
  bslib, mapgl, sf, shiny)

nc <- st_read(system.file("shape/nc.shp", package="sf"))

ui <- page_sidebar(
  title = "mapgl with Shiny",
  sidebar = sidebar(),
  card(
    full_screen = TRUE,
    maplibreOutput("map")
  )
)

server <- function(input, output, session) {
  output$map <- renderMaplibre({
    # maplibre(style = carto_style("positron")) |> 
    #   fit_bounds(nc, animate = FALSE) |> 
    #   add_fill_layer(id = "nc_data",
    #                  source = nc,
    #                  fill_color = "blue",
    #                  fill_opacity = 0.5)
    
    url = "https://inspide.github.io/h3j-h3t/examples/h3j/sample.h3j"
    
    maplibre(center=c(-3.704, 40.417), zoom=15, pitch=30) |>
      add_h3j_source(
        "h3j_testsource", 
        url = url)  |>
      add_fill_extrusion_layer(
        id = "h3j_testlayer",
        source = "h3j_testsource",
        fill_extrusion_color = interpolate(
          column = "value",
          values = c(0, 21.864),
          stops = c("#430254", "#f83c70")),
        fill_extrusion_height = list(
          "interpolate",
          list("linear"),
          list("zoom"),
          14,
          0,
          15.05,
          list("*", 10, list("get", "value"))),
        fill_extrusion_opacity = 0.7 )
  })
}

shinyApp(ui, server)
