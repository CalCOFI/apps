library(shiny)

# js <- "
#   // https://shiny.posit.co/r/articles/build/js-events/#input-events
#   $(document).on('shiny:inputchanged', function(event){
#     if (event.name === 'sel_tree') {
#       // console.log('event.name == ui_tree');
#       // debugger;
#       $('span.treejs-label:contains(\\\"genus;\\\")').each(function(){
#         $(this)
#         .css('background-color', 'darkgray')
#         .css('font-style', 'italic') });
#         // .text($(this).text().replace('genus; ', ''))
#       } } )"
# TODO: text replace taxa_rank; do for rest of taxa_ranks

page_fillable(
  title = "CalCOFI Taxa Mapper",
  theme = dark,
  
  navset_card_pill(
    id        = "nav",
    placement = "above",
    selected  = "nav_configure",
    
    # tags$head(tags$script(HTML(js))),

    # nav_configure ----
    nav_panel(
      title = "Configure",
      value = "nav_configure",
      icon = icon("gear"),
      
      layout_column_wrap(
        # width         = 1/2,
        # heights_equal = "row",
        
        # TODO: sensor
        # selectInput(
        #   "sel_sensor",
        #   "Sensor [TODO]",
        #   c("eDNA","Larval tow", "Visual survey"),
        #   "Larval tow"),
        
        # treeInput(
        #   "sel_taxa",
        #   label       = "Taxa",
        #   choices     = tr_taxa,
        #   # returnValue = "id",
        #   closeDepth  = 5 ),
        
        # TODO: helptext("Add taxa here")
        virtualSelectInput(
          "sel_taxa",
          "Taxa",
          v_taxa,
          multiple                 = T,
          search                   = T,
          autoSelectFirstOption    = F,
          showSelectedOptionsFirst = T,
          showValueAsTags          = F, # T,
          disableSelectAll         = T,
          updateOn                 = "change",
          html                     = T),
          # options = list(
          #   escape = T) ),
          # options = list(
          #   render = I(
          #   '{item: function(item, escape) {
          #       return "<div>" + item.value + "</div>"; },
          #     option: function(item, escape) {
          #       return "<div>" + item.value + "</div>"; } }') ) ),
        # https://stackoverflow.com/questions/73716725/is-there-a-way-to-display-html-inside-a-selectinput-in-an-r-shiny-app
        
        # TODO: helptext("Uncheck any taxa to exclude here")
        jsoneditOutput("json_tree")
      ) ),
    
    # nav_map ----
    nav_panel(
      title = "Map",
      value = "nav_map",
      icon  = icon("map"),
      
      leafletOutput("map")
      #, downloadButton("downloadData", label = "Download")
      ),
    
  )
)
