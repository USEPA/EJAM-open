
# example of basics of getting lat/lon from a map click in shiny

# library(shiny)
# library(leaflet)

latlon_from_map_click_demo <- function(radius_default = 3) {
  
  ui <- shiny::fluidPage(
    sliderInput('pointradius_input', 'Point Radius', min = 1, max = 10, value = radius_default),
    leaflet::leafletOutput("mymap")
  )
  
  server <- function(input, output, session) {
    
    
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
    
    shiny::observe({   # WHEN CLICK ON MAP, GET LAT LON
      # Each kind of Incremental change to the map should be performed in its own observer.
      leaflet::leafletProxy("mymap", session) %>% clearMarkers()  
      # to remove just a specific one they click on, use its id # removeMarker("mycircle") # or input$mymap_marker_click$id
      event <- input$mymap_click
      if (is.null(event)) {return()}
      isolate({
        leaflet::leafletProxy("mymap", session)  %>%
          leaflet::addCircles(lng = input$mymap_click$lng, lat = input$mymap_click$lat,     # new point is added here when map is clicked
                              radius = input$pointradius_input * meters_per_mile, fillOpacity = 0.1,
                              layerId = "mycircle",
                              highlightOptions = highlightOptions(fillOpacity = 0.5) ) %>%  # this just makes it shaded when mouse hovers above the circle
          leaflet::addPopups(lng = input$mymap_click$lng, lat = input$mymap_click$lat, 
                             popup = paste0("lat,lon: ", input$mymap_click$lat, ", ", input$mymap_click$lng))
      })
    })
    
    shiny::observe({   # WHEN RADIUS SLIDER MOVED, CHANGE RADIUS SHOWN ON MAP
      req(input$mymap_click)
      # Each kind of Incremental change to the map should be performed in its own observer.
      leaflet::leafletProxy("mymap", session) %>% clearMarkers()  
      event <- input$pointradius_input
      if (is.null(event)) {return()}
      isolate({
        leaflet::leafletProxy("mymap", session)  %>%
          leaflet::addCircles(lng = input$mymap_click$lng, lat = input$mymap_click$lat, 
                              radius = input$pointradius_input * meters_per_mile,   # radius changes here if slider is used
                              fillOpacity = 0.1,
                              layerId = "mycircle",
                              highlightOptions = highlightOptions(fillOpacity = 0.5, bringToFront = TRUE) ) %>%# this just makes it shaded when mouse hovers above the circle
          leaflet::addPopups(lng = input$mymap_click$lng, lat = input$mymap_click$lat, 
                             popup = paste0("lat,lon: ", input$mymap_click$lat, ", ", input$mymap_click$lng))
      })
    })
    
    
  } # end server
  
  
  shiny::shinyApp(ui, server )
  
  
}
