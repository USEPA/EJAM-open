# THE MODULE ####

# MODULE UI

#' MODULE_UI_latlontypedin - latlontypedin UI code
#'
#' @description A shiny Module.
#' @details Based on example here:
#'   https://stackoverflow.com/questions/75967173/
#'   how-do-i-make-a-simple-user-editable-table-in-an-r-shiny-app-using-modular-desig
#'
#'   but also see for a more complete package that helps provide and excel-like interface:
#'     https://dillonhammill.github.io/DataEditR/
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom rhandsontable rhandsontable hot_to_r rHandsontableOutput renderRHandsontable
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
MODULE_UI_latlontypedin <- function(id) {

  ns <- NS(id)
  tagList(
    rhandsontable::rHandsontableOutput(outputId = ns("TYPED_IN_DATA")), # if you want to display the table output ?
    # actionButton('latlontypedin_submit_button', label='Type in latitudes,longitudes. Click when done.', class = 'usa-button usa-button--outline'),
    shiny::br()
  )
}

# MODULE SERVER code

#' MODULE_SERVER_latlontypedin - latlontypedin Server code
#'
#' @noRd
#'
MODULE_SERVER_latlontypedin <- function(id,
                                        reactdat,
                                        allowColumnEdit = FALSE,
                                        allowRowEdit    = TRUE,
                                        manualRowMove   = TRUE,
                                        ...) {
  shiny::moduleServer(
    id = id,
    function(input, output, session) {
      ns <- session$ns

      output$TYPED_IN_DATA <- rhandsontable::renderRHandsontable({
        tmp <- isolate(reactdat()) # must isolate it or causes infinite loop -- avoid the issue described [here](https://github.com/jrowen/rhandsontable/issues/166)
        rownames(tmp) <- NULL
        rhandsontable::rhandsontable(tmp, allowRowEdit = allowRowEdit, allowColumnEdit = allowColumnEdit, manualRowMove = manualRowMove, ...)
      })

      observeEvent(input$TYPED_IN_DATA, {
        tmp <- rhandsontable::hot_to_r(input$TYPED_IN_DATA) # Update the reactive values for this user-manipulated data to pass back to main environment
        reactdat(tmp) # !!! update the value of reactdat()  based on new value of input$TYPED_IN_DATA
      })
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

  cat('also see  EJAM/R/module_latlontypedin_DEMO.R \n')

  ## to start from a clean slate:

   # rm(list = ls())
   # golem::detach_all_attached()
  # pkgs <- 'EJAM'
   ### pkgs <- c('shiny', 'dplyr', 'rhandsontable', 'data.table', 'leaflet', 'magrittr')
  # for (pkg in pkgs) {require(pkg, character.only = TRUE)}
   ### must attach all of those for this to work when testing the app separate from EJAM package
  # source(system.file("global.R", package = "EJAM"))


  # SIMPLIFIED OVERALL APP ####

  #  UI of an overall app

  APP_UI_TEST <- function(request) {

    shiny::fluidPage(
      shiny::h2('EJAM latlon entry module for EJAM'),

      MODULE_UI_latlontypedin("TESTID_latlonmodule"),

      # shiny::actionButton(inputId =  "latlontypedin_submit_button_TEST", label = "Done entering data (may not want this button - just a way to close any modal)"),

      h3("Example of a live map of those points, drawn in the parent app"),
      leaflet::leafletOutput( ('map_typedin'), height = '600px', width = '100%'),

      h3("Example of a live view of the edited data, as seen in the parent app"),
      DT::DTOutput(outputId =  "typedin_as_datatable_TEST" ),
      br()
    )}

  #  SERVER of an overall app

  APP_SERVER_TEST <- function(input, output, session) {

    # Default initial template of lat lon values table ready for user to type into
    init_data <- structure(list(
      lat = c(47,    46, 33.7477, 26, 40.814),
      lon = c(-123, -69, -118,   -81,  -96.7),
      sitenumber = c(1,  2,     3,     4,      5),
      sitename = c("Site in upper northwest", "Site in Maine", "Site near Los Angeles", "Site in south FL", "Site near Lincoln Nebraska")
    ), row.names = 1:5, class = "data.frame")
    # init_data <-   testpoints_10[1:2, ]

    # Send table template as initial value, but the module updates that reactive_data1 as the user types
    reactive_data1 <-  reactiveVal(init_data)

    MODULE_SERVER_latlontypedin( id = "TESTID_latlonmodule",  reactdat = reactive_data1 )
    # pass the points table that is  the reactive reactive_data1() , but must pass it with with NO parens

    # to view actual table in rendered form to be ready to display it in app UI
    observe({
      tmp <- reactive_data1() # reactiveVal(out())
      output$typedin_as_datatable_TEST <- DT::renderDT(DT::datatable(  tmp  ))
    })  # %>%  bindEvent(input$latlontypedin_submit_button_TEST)   # (when the "Done entering points" button is pressed? but that is inside the module)

    ## to map those points here
    output$map_typedin <- leaflet::renderLeaflet({
      mypoints <- reactive_data1()
      names(mypoints) <- gsub('lon','longitude', names(mypoints)); names(mypoints) <- gsub('lat','latitude', names(mypoints))
      if (length(mypoints) != 0) {
        isolate({ # do not redraw entire map and zoom out and reset location viewed unless...?
          mymap <- leaflet(mypoints) %>% addTiles()  %>%
            addCircles(lat = ~latitude, lng = ~longitude,
                       radius = 10000 ,  # radius_miles() * meters_per_mile,
                       color = "red", fillColor = "red", fill = TRUE,
                       # color = base_color(), fillColor = base_color(), fill = TRUE, weight = circleweight,
                       popup = popup_from_any(mypoints)   )
          mymap
        })
      } else {  # length(mypoints) == 0
        mymap <- leaflet() %>% addTiles() %>% setView(-110, 46, zoom = 3)
        mymap
      }
    }) # end map


  } # end of test server

  ## Run the simplified app ####

  shinyApp(ui = APP_UI_TEST, server = APP_SERVER_TEST)


}
################################################ ################################################# #
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
# MODULE_UI_latlontypedin("pts_entry_table1")

## copied to the server
# testpoints_template <-  testpoints_5[1:2, ]
# reactive_data1 <-  reactiveVal(testpoints_template)
# MODULE_SERVER_latlontypedin(id = "pts_entry_table1", reactdat = reactive_data1)

################################################ ################################################# #

# *How to run automated tests on a module:####
if (1 == 0) {
shiny::testServer(app = MODULE_SERVER_latlontypedin, #  args = list(reactdat = reactive(testpoints_10[1:2, ])),
                  {
                    stopifnot(is.data.frame(
                      input$TYPED_IN_DATA
                    ))
                    stopifnot(is.data.frame(
                      output$TYPED_IN_DATA
                    ))

                    session$setInputs(reactdat = reactive(testpoints_10[1:2, ]))
                    # stopifnot(NROW(  output$TYPED_IN_DATA ) == 2 )

                    # session$setInputs(reactdat = reactive(testpoints_10[1:3, ]))
                    # stopifnot(NROW(  output$TYPED_IN_DATA ) == 3 )

                    #  also can use   testing packages like testthat:
                    #    expect_equal(reactdat(), 2)
                    # Any additional arguments, below, are passed along to the module.
                  })
}
################################################ ################################################# #

## Module code to re-add to app_server.R when using this module
 ## *Latitude Longitude* LOCATIONS TYPED IN (conditional panel)  ------------------------------------- - ####

                    # conditionalPanel(
                    #   condition = "input.ss_choose_method == 'upload' && input.ss_choose_method_upload == 'latlontypedin'",
                    #   ### input: Type into a table, a few facility lat/longs
                    #   ## _+++ MODULE_UI_latlontypedin  ####
                    #   tags$p("Enter / View / Edit latitude(s) and longitude(s) of point(s) to analyze"),
                    #   column(
                    #     6,
                    #     ## on button click, show modal with DT table of lat lon values
                    #     actionButton('show_latlontypedin_module_button', label = "Enter lat lon values on screen", class = 'usa-button usa-button--outline'),
                    #     shinyBS::bsModal(
                    #       trigger = 'show_latlontypedin_module_button',
                    #       id = 'view_latlontypedin',
                    #       size = 'large',
                    #       title = 'Location data',
                    #       helpText('Click or double-click a cell to edit. Right-click to add/remove rows or undo. Click-drag to move a row.'),
                    #       # p("Click or double-click a cell to edit."), p("Right-click to add/remove rows or undo. Click-drag to move a row."),
                    #       br(),
                    #
                    #       MODULE_UI_latlontypedin(id = "pts_entry_table1"),  # this shows the data entry table here
                    #
                    #       # actionButton('latlontypedin_submit_button', label = 'Done entering points', class = 'usa-button usa-button--outline'),
                    #       ## use download buttons for speed and handling larger data
                    #       # downloadButton('download_preview_data_csv', label = 'CSV',   class = 'usa-button'),
                    #       # downloadButton('download_preview_data_xl',  label = 'Excel', class = 'usa-button'),
                    #       # DT::DTOutput("distTable"), # for example, you could put outputs here like this
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
                    # ),     # end   latlontypedin   conditionalPanel
                    ################################################################# #
