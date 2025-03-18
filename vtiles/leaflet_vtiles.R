librarian::shelf(
  htmltools, htmlwidgets, leaflet)

##javascript source
places_tileurl <- "https://tile.calcofi.io/public.places/{z}/{x}/{y}.pbf"
places_tilelayer <- "places"

# plugins <- {
#   list(
#     vgplugin =
#       htmltools::htmlDependency(
#         name = "leaflet.vectorgrid",
#         version = "1.3.0",
#         src = system.file("htmlwidgets", package = "ccissr"),
#         script = "lfx-vgrid-prod.js") ) }

leafletPlugin <- htmlDependency(
  "leaflet.plugins", "2.2.2",
  src = normalizePath("./js"),
  script = "Leaflet.VectorGrid.bundled.min.js")

# <!-- Leaflet plugin for vector tiles support -->
#   <script type="text/javascript"  src="https://unpkg.com/leaflet.vectorgrid@1.2.0"></script>

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

# addPlugin <- function(map) {
#   map <- registerPlugin(map, leafletPlugin)
#   map
# }

add_places <- function(map) {
  map |> 
    registerPlugin(leafletPlugin) |> 
    htmlwidgets::onRender('
      function(el, x) {
        var vectorServer  = "https://tile.calcofi.io/";
        var vectorLayerId = "public.places";
        var vectorUrl     = vectorServer + vectorLayerId + "/{z}/{x}/{y}.pbf?filter=station=90";
        
        console.log("Reading tiles from " + vectorUrl);
        
        var vectorTileStyling = {};
        var vectorTileColor   = "green";
        
        vectorTileStyling[vectorLayerId] = {
          "fill":        true,
          "fillColor":   vectorTileColor,
          "fillOpacity": 0.1,
          "color":       vectorTileColor,
          "opacity":     0.7,
          "weight":      2
        };
        
        var vectorTileOptions = {
          // "rendererFactory":       L.canvas.tile,
          "interactive":           true,  // Make sure that this VectorGrid fires mouse/pointer events
          // "attribution":           "&copy; OpenStreetMap",
          "vectorTileLayerStyles": vectorTileStyling,
          "getFeatureId": function(f) {
             console.log(f)
             return f.properties.key
           }
        };
        
        function featureHtml(f) {
          var p = f.properties;
          var h = "<p>";
          for (var k in p) {
            h += "<b>" + k + ":</b> " + p[k] + "<br/>"
          }
          h += "</p>";
          return h
        }
        
        var vectorLayer = L.vectorGrid.protobuf(vectorUrl, vectorTileOptions)
          .on("click", function (e) {
            console.log("click")
            
            let fields = e.layer._renderer._layers;
            console.log(fields);
            
            console.log(f)
            L.popup()
            .setContent(featureHtml(e))
            //.setLatLng(e.latlng)
            .openOn(this)
          });
        
        vectorLayer.addTo(this);
    ')
    
    # paste0('
    # function(el, x) {
    # 
    #   var vectorTileOptions=function(
    #     layerName, layerId, activ, prop, id) {
    #       return {
    #         vectorTileLayerName: "places",
    #         interactive: activ, // makes it able to trigger js events like click
    #         vectorTileLayerStyles: {
    #           [layerId]: function(properties, zoom) {
    #             return {
    #               weight: 0,
    #               fillColor: "green",
    #               fill: true,
    #               fillOpacity: 0.1,
    #               color: "green",
    #               opacity: 0.7,
    #               weight: 2
    #             }
    #           }
    #         },
    #         getFeatureId: function(f) {
    #             return f.properties[id];
    #         }
    #       }
    #   };
    #   
    #   var lyr = L.vectorGrid.protobuf(
    #     "', places_tileurl, '",
    #     vectorTileOptions("places", "', places_tilelayer, '", true,
    #                       "tilePane", "MAP_LABEL", "MAP_LABEL")
    #   )
    #   this.layerManager.addLayer(lyr, "tile", "places", "places");
    #   
    #   lyr.bindTooltip(function(e) {
    #     return e.properties.MAP_LABEL
    #   }, {sticky: true, textsize: "10px", opacity: 1});
    #   lyr.bringToFront();
    # }'))
}

