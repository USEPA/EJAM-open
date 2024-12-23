
if ("execute this code now? during package loading?" == "yes") {
  
  
  ######################## ######################### ######################### #
  ##         source this bit to test/try it out here:
  # 
  #    library(EJAM)
  #    library(dplyr); library(leaflet.extras); library(htmlwidgets)
  library(shiny); library(leaflet)
  outerapptest_mod_ui_lasso <- fluidPage(
    mod_ui_lasso("testid"),
    shiny::textOutput("selected_markers_out")
  )
  outerapptest_mod_server_lasso <- function(input, output, session) {
    selected_markers_react <- reactive({
      mod_server_lasso("testid")
    })
    output$selected_markers_out <- renderText(selected_markers_react())
  }
  
  shinyApp(outerapptest_mod_ui_lasso, outerapptest_mod_server_lasso)
  
  ######################## ######################### ######################### #
  
}

######################################################### #
mod_ui_lasso <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    leafletOutput(ns("map")),
    actionButton(ns("clear"), "Clear Selection"),
    verbatimTextOutput(ns("selected_output"))
  )
}
######################################################### #
mod_server_lasso <- function(id, test_data = NULL) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      if (is.null(test_data)) {
        require(EJAM)
        ## to use  test map data
        # set.seed(123)
        # test_data = testpoints_n(20, ST = 'DE', weighting = "bg")
        # data.table::setnames(test_data, "lon", "lng")
        # test_data = data.frame(test_data)
        test_data <- structure(list(
          bgid = c(44312L, 44360L, 44076L, 44423L, 44092L, 44015L, 44196L, 44126L, 44141L, 43911L, 44271L,
                   44562L, 44499L, 44500L, 43988L, 44245L, 44546L, 44252L, 43923L, 44416L), 
          lat = c(39.6483827039613, 39.6389517984224, 39.8186178784672, 38.8710306277565, 39.7817731015261, 
                  39.753135978637, 39.7481802878728, 39.8007228124424, 39.7712039879519, 39.27273975, 39.672973248986, 38.5630155824874, 38.7619318960277, 
                  38.7470563949542, 39.1747479275832, 39.6629958174207, 38.5700570726592, 39.6789358167621, 39.1657176160184, 38.8999756204517), 
          lng = c(-75.6491838380721, -75.6448678522451, -75.458495364051, -75.3999796478927, -75.4786317108433, 
                  -75.5444082023591, -75.7317288622763, -75.5385701125345, -75.6252073773275, -75.5994849689436, -75.7303337368954, -75.1092344083361, -75.215319744905, 
                  -75.2000010279816, -75.4547592414185, -75.6703971605671, -75.0857810978776, -75.7060563204415, -75.5271944952707, -75.3989802855709), 
          bgfips = c("100030149041", "100030163061", "100030101061", "100050501052", "100030105025", 
                     "100030005004", "100030135062", "100030115001", "100030119003", "100010402051", "100030147031", "100050513071", "100050508072", 
                     "100050508081", "100010432021", "100030139061", "100050512012", "100030141001", "100010409001", "100050501032"), 
          blockcount = c(30L, 15L, 10L, 39L, 28L, 13L, 18L, 12L, 25L, 33L, 16L, 78L, 38L, 24L, 88L, 17L, 25L, 19L, 35L, 45L)), 
          class = "data.frame", 
          row.names = c(NA, -20L))
      }
      test_data$id = 1:NROW(test_data)
      
      output$map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addMarkers(data = test_data, layerId = ~id, label = ~id) %>%
          leaflet.extras::addDrawToolbar(
            targetGroup = "draw",
            editOptions = leaflet.extras::editToolbarOptions(
              selectedPathOptions = leaflet.extras::selectedPathOptions())
          ) %>%
          htmlwidgets::onRender("
        function(el, x) {
          var myMap = this;
          var drawnItems = new L.FeatureGroup();
          myMap.addLayer(drawnItems);

          myMap.on('draw:created', function(e) {
            var layer = e.layer;
            drawnItems.addLayer(layer);

            var selectedMarkers = [];
            myMap.eachLayer(function(layer) {
              if (layer instanceof L.Marker) {
                if (drawnItems.getLayers()[0].getBounds().contains(layer.getLatLng())) {
                  selectedMarkers.push(layer.options.layerId);
                  layer.setIcon(L.icon({iconUrl: 'https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png'}));
                }
              }
            });
            Shiny.onInputChange(ns('selected_markers'), selectedMarkers);
          });
        }
      ")
      })
      
      observeEvent(input$clear, {
        leafletProxy("map") %>%
          clearGroup("draw") %>%
          clearMarkers() %>%
          addMarkers(data = test_data, layerId = ~id, label = ~id)
      })
      
      output$selected_output <- renderPrint({
        if (!is.null(input$selected_markers)) {
          cat("Selected marker IDs:\n")
          print(input$selected_markers)
        } else {
          cat("No markers selected")
        }
      })
      
      selected_markers_react <- reactive({
        
        (input$selected_markers)
        
      })
      
      return(selected_markers_react())
    }
  )
}
######################################################### #
# shinyApp(ui_lasso, server_lasso) 

