function(input, output, session) {

    output$map <- leaflet::renderLeaflet({

      leaflet() |> 
        addTiles() |> 
        setView(-122.4, 33.8, 4) |> 
        add_places()

    })

}
