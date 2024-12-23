################################################ ################################################# #

# *How to use reactives as inputs/outputs of a module  ####
#
# If a module needs to use a reactive expression, the outer function should take the reactive expression as a parameter.
# If a module wants to return reactive expressions to the calling app, then return a list of reactive expressions from the function.
# If a module needs to access an input that isn’t part of the module, the
#   containing app should pass the input value wrapped in a reactive expression (i.e. reactive(...)):
#   myModule("myModule1", reactive(input$checkbox1))

################################################ ################################################# #

# *How to call a module from outer overall app ####
## copied to the UI
# MODULE_UI_latlon_from_map_click("pts_entry_table2")

## copied to the server
# testpoints_template <-  testpoints_5[1:2, ]
# reactive_data1 <-  reactiveVal(testpoints_template)
# MODULE_SERVER_latlon_from_map_click(id = "pts_entry_table2", reactdat = reactive_data1)

################################################ ################################################# #

# THE MODULE ####

##################################################### # 

# MODULE UI

#' MODULE_UI_latlon_from_map_click- latlon_from_map_click UI code
#'
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#'
MODULE_UI_latlon_from_map_click <- function(id) {
  
  radius_default = 3
  
  ns <- shiny::NS(id)
  tagList(
    
    # show SLIDER & MAP  
    
    shiny::sliderInput(
      ns('pointradius_input'),
      'Point Radius', min = 1, max = 10, value = radius_default),
    
    leaflet::leafletOutput(
      ns("mymap")
    ),
    
    shiny::br()
  )
}
##################################################### # 

# MODULE SERVER code

#' MODULE_SERVER_latlon_from_map_click - latlon_from_map_click Server code
#'
#' @noRd
#'
MODULE_SERVER_latlon_from_map_click <- function(id,
                                                reactdat,  # this is a reactive value that could be passed here as the default data.frame of lat,lon of 1 point
                                                ...) {
  
  # if instead of this being a param passed here, you were to initialize the reactive data.frame of lat,lon of 1 point that will be output of this function
  # reactdat <- shiny::reactiveVal(data.frame(lat = NA, lon = NA))  
  
  #################################### #
  shiny::moduleServer(
    id = id,
    function(input, output, session) {
      ns <- session$ns
      
      output$mymap <- leaflet::renderLeaflet({  # DRAW BASIC MAP
        # Use leaflet() here, and only include aspects of map that won't need to change dynamically 
        # (at least, not unless the entire map is being torn down and recreated).
        leaflet::leaflet() %>% 
          leaflet::setView(-99, 40, zoom = 4)  %>% 
          leaflet::addProviderTiles(
            leaflet::providers$CartoDB.Positron,
            options = leaflet::providerTileOptions(noWrap = TRUE)
          )
      })
      
      shiny::observe({   # WHEN CLICK ON MAP, GET LAT LON VALUE AND SHOW IT ON THE MAP
        
        # Each kind of Incremental change to the map should be performed in its own observer.
        leaflet::leafletProxy("mymap", session) %>% clearMarkers()
        # to remove just a specific one they click on, use its id # removeMarker("mycircle") # or input$mymap_marker_click$id
        event <- input$mymap_click
        if (is.null(event)) {return()}
        shiny::isolate({
          
          # UPDATE THE REACTIVE DATAFRAME WHEN POINT IS CLICKED
          reactdat(data.frame(lat = input$lat, lon = input$lon))
          
          # UPDATE THE MAP TO SHOW THE NEW POINT
          leaflet::leafletProxy("mymap", session)  %>%
            leaflet::addCircles(lng = input$mymap_click$lng, lat = input$mymap_click$lat,     # new point is drawn here when map is clicked
                                radius = input$pointradius_input * meters_per_mile, fillOpacity = 0.1,
                                highlightOptions = highlightOptions(fillOpacity = 0.5), # this just makes it shaded when mouse hovers above the circle
                                layerId = "mycircle",
            ) %>%  
            leaflet::addPopups(lng = input$mymap_click$lng, lat = input$mymap_click$lat, 
                               popup = paste0("lat,lon: ", input$mymap_click$lat, ", ", input$mymap_click$lng))
        })
      })
      
      shiny::observe({   # WHEN RADIUS SLIDER MOVED, CHANGE RADIUS SHOWN ON MAP
        shiny::req(input$mymap_click)
        # Each kind of Incremental change to the map should be performed in its own observer.
        leaflet::leafletProxy("mymap", session) %>% clearMarkers()  
        event <- input$pointradius_input
        if (is.null(event)) {return()}
        shiny::isolate({
          leaflet::leafletProxy("mymap", session)  %>%
            leaflet::addCircles(lng = input$mymap_click$lng, lat = input$mymap_click$lat, 
                                radius = input$pointradius_input * meters_per_mile,   # radius changes here if slider is used
                                fillOpacity = 0.1,
                                layerId = "mycircle",
                                highlightOptions = leaflet::highlightOptions(fillOpacity = 0.5, bringToFront = TRUE) ) %>%# this just makes it shaded when mouse hovers above the circle
            leaflet::addPopups(lng = input$mymap_click$lng, lat = input$mymap_click$lat, 
                               popup = paste0("lat,lon: ", input$mymap_click$lat, ", ", input$mymap_click$lng))
        })
      })
      
      # return a reactive data.frame of lat lon values for 1 point
      return( reactdat ) # no parentheses here - return the reactive object not just its current value
    })
}
################################################ ################################################# #


################################################ ################################################# #
# . ####
# Try it out (from a simplified app, a test version of outer overall app) ####

try_this_module_here <- FALSE # so that installation will not source this and launch the module as a mini app

#   try_this_module_here <- TRUE
if (try_this_module_here) {
  ## Set up so it works here (if the packages were not attached, etc.) ####
  ## This test only would work after sourcing this whole file first, or after installing and loading EJAM to have the module and the sourcing this simplified outer overall app
  
  # cat('also see  EJAM/R/module_latlontypedin_DEMO.R \n')
  
  ## to start from a clean slate:
  
  # rm(list = ls())
  # golem::detach_all_attached()
  # pkgs <- 'EJAM'
  ### pkgs <- c('shiny', 'leaflet', 'magrittr')
  # for (pkg in pkgs) {require(pkg, character.only = TRUE)}
  ### must attach all of those for this to work when testing the app separate from EJAM package
  # source(system.file("global.R", package = "EJAM"))
  ################################################# #
  
  # SIMPLIFIED OVERALL APP ####
  ################################################# #
  
  #  UI of an overall outer app
  
  APP_UI_TEST <- function(request) {
    
    shiny::fluidPage(
      shiny::h2('module for EJAM'),
      
      ################################# # 
      ##  THE MODULE'S UI SHOULD APPEAR HERE, NAMELY THE MAP YOU CAN CLICK ON AND THE SLIDER
      MODULE_UI_latlon_from_map_click("TESTID_latlon_from_map_click_module"),
      ################################# # 
      
      # shiny::actionButton(inputId =  "latlon_from_map_click_submit_button_TEST", 
      # label = "Done picking point (may not want this button - just a way to close any modal)"),
      
      # h3("Example of a live map of results of module, drawn in the parent app"),
      # leaflet::leafletOutput( ('map_click_module'), height = '600px', width = '100%'),
      
      h3("Example of a live view of the point (data) updated by the module, as seen in the parent app as a data.table"),
      DT::DTOutput(outputId =  "latlon_from_map_click_TEST" ),
      br()
    )}
  ################################################# #
  
  #  SERVER of an overall app
  
  APP_SERVER_TEST <- function(input, output, session) {
    
    init_data = testpoints_10[1,]
    # Send  initial value, but the module updates that reactive_data1 as the user types
    reactive_data1 <-  shiny::reactiveVal(init_data)
    
    ################################# # 
    ##  THE MODULE'S SERVER CODE SHOULD GET RUN HERE
    MODULE_SERVER_latlon_from_map_click( id = "TESTID_latlon_from_map_click_module"  ,  reactdat = reactive_data1 )
    # if you had to pass a points table that is the reactive reactive_data1() ,  must pass it with with NO parens
    # and it should get changed by the module as the user clicks on the map in the module
    ################################# # 
    
    # to view actual table in rendered form to be ready to display it in app UI
    shiny::observe({
      tmp <- reactive_data1() # reactiveVal(out())  # WHEN THIS VALUE CHANGES, THE OUTER APP SHOULD UPDATE THE RENDERED TABLE 
      output$latlon_from_map_click_TEST <- DT::renderDT(DT::datatable(  tmp  ))
    })  # %>%  bindEvent(input$latlontypedin_submit_button_TEST)   # (when the "Done entering points" button is pressed? but that is inside the module)
    
    
    # ## to map those points here also ?
    # output$map_click_module <- leaflet::renderLeaflet({
    #   mypoints <- reactive_data1()
    #   names(mypoints) <- gsub('lon','longitude', names(mypoints)); names(mypoints) <- gsub('lat','latitude', names(mypoints))
    #   if (length(mypoints) != 0) {
    #     isolate({ # do not redraw entire map and zoom out and reset location viewed unless...?
    #       mymap <- leaflet(mypoints) %>% addTiles()  %>%
    #         addCircles(lat = ~latitude, lng = ~longitude,
    #                    radius = 10000 ,  # radius_miles() * meters_per_mile,
    #                    color = "red", fillColor = "red", fill = TRUE,
    #                    # color = base_color(), fillColor = base_color(), fill = TRUE, weight = circleweight,
    #                    popup = popup_from_any(mypoints)   )
    #       mymap
    #     })
    #   } else {  # length(mypoints) == 0
    #     mymap <- leaflet() %>% addTiles() %>% setView(-110, 46, zoom = 3)
    #     mymap
    #   }
    # }) # end map
    
    
  } # end of test server
  
  ## Run the simplified app ####
  
  shiny::shinyApp(ui = APP_UI_TEST, server = APP_SERVER_TEST)
  
  
}
################################################ ################################################# #

 
################################################ ################################################# #

## example of Module code to re-add to app_server.R when using this module
## *Latitude Longitude* click on LOCATION  (conditional panel)  ------------------------------------- - ####

# conditionalPanel(
#   condition = "input.ss_choose_method == 'upload' && input.ss_choose_method_upload == 'latlon_from_map_click'",
#   ### input: latlon_from_map_click
#   ## _+++ MODULE_UI_latlon_from_map_click  ####
#   tags$p("Click to specify latitude and longitude of point to analyze"),
#   column(
#     6,
#     ## on button click, show modal with   lat lon value?
#     actionButton('show_latlon_from_map_click_module_button', label = "Click to specify lat lon value", class = 'usa-button usa-button--outline'),
#     shinyBS::bsModal(
#       trigger = 'show_latlon_from_map_click_module_button',
#       id = 'view_latlon_from_map_click',
#       size = 'large',
#       title = 'Location data',
#       br(),
#
#       MODULE_UI_latlon_from_map_click(id = "pts_entry_table2"),  # this shows the thing here
#
#       # actionButton('latlon_from_map_click_submit_button', label = 'Done selecting point', class = 'usa-button usa-button--outline'),
#       ## use download buttons for speed and handling larger data
#       # downloadButton('download_preview_data_csv', label = 'CSV',   class = 'usa-button'),
#       # downloadButton('download_preview_data_xl',  label = 'Excel', class = 'usa-button'),
#       
#       # verbatimTextOutput("test_textout"),
#       br()
#     ),
#   ),
#   # tags$span(
#   #   tags$ul(
#   #     tags$li('Required Columns: lat, lon'),
#   #     tags$li('Optional Columns: siteid')
#   #   )
#   # ),
#   # actionButton('latlon_help', label='More Info', class = 'usa-button usa-button--outline'),
#   # HTML(latlon_help_msg)
#   br()
# ),     # end   latlon_from_map_click   conditionalPanel
################################################################# #
