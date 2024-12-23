#' get_shape_from_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_get_shape_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    ## upload shapefile ####
    shiny::fileInput(inputId = ns('shapefile'),
                     placeholder = "test_shapefile.shp",
                     multiple = FALSE,
                     # placeholder = 'folder that contains files like shapefile.shp', multiple = FALSE, # if from a whole folder
                     label = 'Upload shapefile of areas to analyze',
                     # add hover tips here maybe, or even a button to get examples of valid formats and details on that.
                     )
  )
}

#' get_shape_from_upload Server Functions
#'
#' @noRd
mod_get_shape_upload_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    myshape <- shiny::reactive({
      # THIS GETS UPDATED WHEN THERE IS A CHANGE IN input$shapefile
      ## if not uploaded yet, have default example?? ####
      if (is.null(input$shapefile)) {
        shapefile_contents <- NULL # default_shapefile_shown_at_startup  # would be defined in global
      } else {
        shapefile_contents <- sf::st_read(input$shapefile$datapath)
        # shapefile_contents <- shapefile_from_folder(input$shapefile$datapath) # if from a whole folder
      }
    shapefile_contents
  })
})
}

## in the UI
# mod_get_shape_upload_ui("get_shape_from_upload_1")

## in the server
# mod_get_shape_upload_server("get_shape_from_upload_1")

#############################################################
################################################ ################################################# #
################################################ ################################################# #
#
# To test the module ####

testing_this_module <- FALSE # so that installation will not source this and launch the module as a mini app
if (testing_this_module) {

  #  (EJAM)
  # lib x ra ry(   shiny); lib x ra ry(   magrittr); lib x rary(   leaflet)  # maybe? must attach all of those manually for this to work unless EJAM attached already?

  #  (  EJAM) # for testpoints_10, e.g., BUT THAT WOULD REPLACE AN UPDATED MODULE BELOW IF NOT ALREADY REBUILT/RELOADED WITH UPDATE

  default_calculate_ratios <- TRUE
  use_ejscreenit_tf <- FALSE
  ######################### #
  TEST_UI <- function(request) {
    shiny::fluidPage(
      tabsetPanel(
        tabPanel(
          title = "api app",
          shiny::h2('EJScreen API batch tool packaged with EJAM'),
          # verbatimTextOutput("testinfo1"),
          # verbatimTextOutput("testinfo_radius"),
          # shiny::textOutput("testinfo2"),
          # EJAM:::mod_ejscreenapi_ui("TESTID", simpleradius_default_for_ui = 2),

          EJAM:::mod_ejscreenapi_ui("TESTID",

                             simpleradius_default_for_ui = 2
          ),
          br()
        ),
        tabPanel(
          title = "results in overall app", br(),

          h3("you could show them here"),
          ## redundant since module already shows it, but here as example of module returning a table.
          ##  module server function returns a table to overall server/app, which itself can then use/display that table.
          # DT::DTOutput("results"),
          ## Demo of how you might send radius from outer app to module as starting value of module radius slider:
          # sliderInput("outerappradius", label = "Radius we could preset", min = 1, max = 10, step = 1, value = 1.2),
          br()
        )))
  }
  ######################### #
  TEST_SERVER <- function(input, output, session) {

    x <- EJAM:::mod_ejscreenapi_server(

      "TESTID",

      default_points_shown_at_startup_react = reactive(testpoints_5[1:2,]),
      use_ejscreenit = use_ejscreenit_tf
    )
# check this belongs here: ***
    source(system.file("global_EJAMejscreenapi.R", package = "EJAM"))
    
    # output$testinfo1 <- renderPrint( ("info can go here "))
    # output$testinfo_radius <- renderPrint(paste0("Radius is ", x()$radius.miles, " miles"))
    # output$testinfo2 <- renderText(cat("x names:  ", paste0(names(x()), collapse = ", "), "\n"))

    output$results <- DT::renderDT({x()},
                                          options = list(
                                            selection = 'multiple',
                                            dom = 'rtip', # specify 4 DOM elements:
                                            # processing, table, info, pagination
                                            # per https://datatables.net/examples/basic_init/dom.html
                                            scrollX = TRUE,
                                            searchPanes = TRUE  # does this work?
                                          ),
                                          escape = FALSE
    )
    # *** CAUTION ***
    # escape=TRUE is better for security reasons (XSS attacks).
    # escape=FALSE lets ejscreen URL links work,
    #   but not links from ECHO table download.
  }
  ######################### #

  shinyApp(ui = TEST_UI, server = TEST_SERVER) # Try module in mini/test app


  # note you cannot wrap ui and server code in a single function when using modules

  ##  in the main app UI
  # mod_ejscreenapi_ui("x2")

  ##   in the main app server
  # mod_ejscreenapi_server("x2")
}
################################################ ################################################# #
################################################ ################################################# #
