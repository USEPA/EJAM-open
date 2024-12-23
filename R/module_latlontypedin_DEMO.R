################################################ ################################################# #
#
# Play with a simplified, demo module similar to the MODULE_SERVER_latlontypedin module ####

try_demo_module_here <-  FALSE  # try_demo_module_here <-  TRUE
if (try_demo_module_here) {
  # set up packages/functions for it to work here ####
  # This test only would work after sourcing this whole file first 
  ################################################ #
  
  cat('also see MODULE_latlontypedin.R \n')
  
  ## to start from a clean slate: 
  
  golem::detach_all_attached()
  rm(list = ls())
  pkgs <- c('shiny', 'dplyr', 'rhandsontable', 'data.table', 'leaflet') 
  # 
  for (pkg in pkgs) {require(pkg, character.only = TRUE)}
  
  ################################################ ################################################# #
  ################################################ ################################################# #
  
  # SIMPLE DEMO MODULE ####
  # (simpler than the full module MODULE_SERVER_latlontypedin ) 
  
  MODULE_UI_latlontypedin_DEMO <- function(id, ...) {
    ns <- NS(id)
    rHandsontableOutput(outputId = ns("TYPED_IN_DATA"), ...)
  }
  ################################################ #
  MODULE_SERVER_latlontypedin_DEMO <-
    function(id,
             reactdat,  # a reactive object
             allowColumnEdit = FALSE, allowRowEdit    = TRUE, manualRowMove   = TRUE,
             ...) {
      moduleServer(id,
                   function(input, output, session) {
                     ns <- session$ns
                     
                     output$TYPED_IN_DATA <- renderRHandsontable({
                       tmp <- isolate(reactdat())  # must isolate it or causes infinite loop -- avoid the issue described [here](https://github.com/jrowen/rhandsontable/issues/166)
                       rownames(tmp) <- NULL
                       rhandsontable(tmp, allowRowEdit = allowRowEdit, allowColumnEdit = allowColumnEdit, manualRowMove = manualRowMove, ...)
                     })
                     
                     observeEvent(input$TYPED_IN_DATA, {  # seems strange to use observeEvent when event is just update of data in TYPED_IN_DATA, and could simply use observe
                       tmp <- rhandsontable::hot_to_r(input$TYPED_IN_DATA)  # Update the reactive values for this user-manipulated data to pass back to main environment
                       reactdat(tmp)  # !!! update the value of reactdat()  based on new value of input$TYPED_IN_DATA
                     })
                     return( reactdat )
                   })
    } # end MODULE_SERVER_latlontypedin_DEMO
  
  ################################################ ################################################# #
  ################################################ ################################################# #
  #
  # SIMPLE DEMO OVERALL APP ####
  
  ## overall app UI 
  
  APP_UI_DEMO <-  function(request) { fluidPage(
    
    titlePanel("DEMO OF AN APP using a module for data table entry"),
    p("Instructions in outer app:"), p("Double-click a cell to edit."), p("Right-click to add/remove rows. Click-drag to move a row."),  
    h2("Data entry module itself:"),
    #  MODULE_UI_latlontypedin_DEMO <- 
    MODULE_UI_latlontypedin_DEMO(id = "tab1"),  # **************** THE UI OF THE MODULE IS INSERTED HERE
    
    br(),
    h3("Example of outer app showing live map of edited data from module"),
    leaflet::leafletOutput( ('map_typedin'), height = '600px', width = '100%'),
    
    h3("Example of outer app showing live view of edited data from module"),
    tableOutput("data1"),
    br()
  )}
  ################################################ #
  
  ## overall app SERVER  
  
  APP_SERVER_DEMO <- function(input, output) {
    
    init_data <- structure(list(
      lat = c(47,    46, 33.7477, 26, 40.814),
      lon = c(-123, -69, -118,   -81,  -96.7),
      sitenumber = c(1,  2,     3,     4,      5),
      sitename = c("Site in upper northwest", "Site in Maine", "Site near Los Angeles", "Site in south FL", "Site near Lincoln Nebraska")
    ), row.names = 1:5, class = "data.frame")
     # init_data <-  testpoints_10[1:2, ]
    reactive_data1 <-  reactiveVal(init_data)
    
    MODULE_SERVER_latlontypedin_DEMO(id = "tab1", reactdat = reactive_data1)  # ************************* THE server logic OF THE MODULE IS used HERE
    
    ## to view actual table in rendered form to be ready to display it in app UI 
    observe({
      tmp <- reactive_data1()
      output$data1 <- tmp %>% renderTable()
    })
    
    
    ## To map those points here ####
    
    # output$map_typedin <- reactive(mapfast(reactive_data1()))  # would not work like this - func would need to return map not just draw it
    
    output$map_typedin <- leaflet::renderLeaflet({
      mypoints <- reactive_data1()
      names(mypoints) <- gsub('lon','longitude', names(mypoints)); names(mypoints) <- gsub('lat','latitude', names(mypoints))
      if (length(mypoints) != 0) {
        isolate({ # not needed for this simple example, but would prevent  redraw of entire map when reactives inside isolate change; only redraw if reactive_data1() changes.
          mymap <- leaflet(mypoints) %>% addTiles()  %>%
            # addCircleMarkers(lat = ~latitude, lng = ~longitude,  # for addCircleMarkers, radius is in pixels, for addCircles, radius is in meters 
            addCircles(      lat = ~latitude, lng = ~longitude,
                             radius = 50000, # 50000 meters is 50km   #  3 * meters_per_mile ,  # radius_miles() * meters_per_mile, 
                             color = "red", fillColor = "red",   fill = TRUE,  
                             popup = popup_from_any(mypoints)     
            )
          #### %>% clearBounds() # clears the bound, so that the view will be automatically determined by the range of latitude/longitude data in the map layers if provided;
          mymap
        })
      } else {  # length(mypoints) == 0
        mymap <- leaflet() %>% addTiles() %>% setView(-110, 46, zoom = 3)
        mymap
      }
    })
    
  }
  
  # Run the simplified app ####
  
  shinyApp(ui = APP_UI_DEMO, server = APP_SERVER_DEMO)
  
  
  
  
}
