
# Now see probably-newer full app server (not module) code here:
#   app_server_EJAMejscreenapi()


################################################ ################################################# #
################################################ ################################################# #
#
# To test the module ####

testing_ejscreenapi_module <- FALSE # so that installation will not source this and launch the module as a mini app
if (testing_ejscreenapi_module) {
  # This test maybe only would work after sourcing this whole file first,
  # or after installing and loading EJAM to have these functions (but not exported)
  # source this file first if not already loaded along with EJAM package

  # library(   shiny); library(   magrittr); library(   leaflet)  # must attach all of those manually for this to work unless EJAM attached already?

  # THIS SHOULD STAY ONLY INSIDE THE MODULE'S NAMESPACE:  ! TO AVOID INTERFERING WITH EJAM globalenv() etc.
   # source(system.file("global_EJAMejscreenapi.R", package = "EJAM"))

  # library( ## EJAM) # for testpoints_10, e.g., BUT THAT WOULD REPLACE AN UPDATED MODULE BELOW IF NOT ALREADY REBUILT/RELOADED WITH UPDATE

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

 EJAM:::mod_ejscreenapi_ui("TESTID",  # since it is not exported and this test is a script not a function, you have to use ::: to access it

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

  x <- EJAM:::mod_ejscreenapi_server(  # since it is not exported and this test is a script not a function, you have to use ::: to access it
    # x <-        mod_ejscreenapi_server(
                  "TESTID",

                  default_points_shown_at_startup_react = reactive(testpoints_5[1:2,]),
                  use_ejscreenit = use_ejscreenit_tf
    )

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

  shinyApp(ui = TEST_UI, server = TEST_SERVER) # This is how you can try the module as a mini/test app


  # note you cannot wrap ui and server code in a single function when using modules

  ##  in the main app UI
  # mod_ejscreenapi_ui("x2")

  ##   in the main app server
  # mod_ejscreenapi_server("x2")
}
################################################ ################################################# #
################################################ ################################################# #

# NOTE  ####
# If a module needs to USE a reactive expression, the 
#   outer function should take the reactive expression as a parameter. 
#
# If a module needs to access an input$ that isn’t part of the module, the
#   containing app should pass the input$ value wrapped in a reactive expression (i.e. reactive(...)):
#   myModule("myModule1", reactive(input$checkbox1))
#
# If a module wants to return reactive expressions to the calling app, then return a list of reactive expressions from the function.

# ____________________________ ### #
# ________________________________________________________ ####
# .####
#    ********************   UI   ******************** <<<< ####
# .####

#' mod_ejscreenapi_ui - ejscreenapi UI Function
#'
#' @description A shiny Module to get EJScreen results via the EJScreen API
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param simpleradius_default_for_ui miles radius to show as initial default selection - not used if reactive version used now
#' @param default_radius miles radius to show as initial default selection
#' @param default_radius_react reactiveVal passed to module UI from EJAM app that is initial radius to show
#'   so that it can reflect what user already set as radius in main EJAM app
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import leaflet
#'
mod_ejscreenapi_ui <- function(id,
                               simpleradius_default_for_ui = 1 #,
                               # default_points_shown_at_startup_react = reactive(1),
                               # default_radius = 1
                               # default_radius_react = reactive(1)
                               ) {
  ns <- NS(id)
  # I think this needs to be here or in outer app that calls the module, not just in mod_ejscreenapi_server(), so it will be available within this function like specifying default values in UI select/radio buttons/etc.
  # source(system.file("global_EJAMejscreenapi.R", package = "EJAM"))

  tagList(
    #  from here on within the brackets is directly copied from ejscreenapi pkg app_ui.R code
    shiny::tabsetPanel(
      id = ns( 'ejscreenapi_tabset'), selected = 'EJScreen API' ,
      # ____________________________ ## ##

      #--------------------- main tab ------------------ #
      shiny::tabPanel(title = "EJScreen API",

                      shiny::fluidRow(
                        # SITE SELECTION  _____ (LH column) ####
                        column(
                          4,
                          #  would need to fix this link and/or edit its text to be relevant for within this module. and add rel="noreferrer noopener" ***
                          # HTML(paste('<a href=\"', 'README.html', '\", target=\"_blank\">', 'About this app',  '</a>', sep = '')),
                          tags$br(), tags$br(),
                          # verbatimTextOutput(outputId = ns("testinfo_sessionuser") ),
                          ##################################################### #
                          ##################################################### #

                          ## >>>>>> RADIUS ####

                          # SIMPLIFIED ENTRY OF RADIUS UNTIL MODULE VERSION OF SLIDE VS TEXTBOX IS FIXED ***
                           shiny::numericInput(ns("SIMPLERADIUS"), 'Radius in miles',
                                               # value = default_radius_react(),
                                               value = simpleradius_default_for_ui,
                                               min = 0.2, max = 10, step = 0.1),
                          # shiny::radioButtons(ns("slider_vs_text_radius"),  label = NULL, choices = list(`Type in radius` = "type_in", `Use slider` = "use_slider"), inline = T), #
                          #
                          # shiny::conditionalPanel(
                            # condition = "input.slider_vs_text_radius == 'use_slider'",
                            # shiny::uiOutput(ns('radius_slider'))  ,
                          # ),
                          # shiny::conditionalPanel(
                            # condition = "input.slider_vs_text_radius == 'type_in'",
                            # shiny::uiOutput(ns('radius_textbox'))  ,
                          # ),

                          ##################################################### #
                          ##################################################### #

                          ## search via ECHO ## ##

                          # shiny::actionButton(ns('echobutton'), label = 'Find sites via ECHO'),

                          ## UPLOAD points ####

                          shiny::fileInput(ns('pointsfile'),
                                           placeholder = 'testpoints_5.xlsx',
                                           multiple = FALSE,
                                           label = 'Upload file (.csv, .xls, or .xlsx) with lat & lon as column headers in row 1',
                                           # add hover tips here maybe, or even a button to view examples of valid formats and details on that.
                                           accept = c('.xls', '.xlsx', ".csv", "text/csv", "text/comma-separated-values,text/plain")),
                          # shiny::textOutput(ns('most_recently_changed_text')), # for testing
                          shiny::textOutput(ns('count')),    # how many were uploaded
                          # shiny::textOutput(ns('max_pts_upload_txt')), # how many allowed for upload - otherwise rejects upload. set in global
                          # shiny::textOutput(ns('max_pts_map_txt')), # how many get mapped (the first n points) - omits any after that cap from map
                          # shiny::textOutput(ns('max_pts_showtable_txt')), # #
                          shiny::textOutput(ns('max_pts_run_txt')),     # how many allowed for analysis, max (but not really enforced even though it says there is a cap)

                          ## START button ####

                          shiny::actionButton(ns('runbutton'), label = 'Start'),
                          # radioButtons(ns('whichip'),label = 'URL/IP', choices = ips, selected = whichip, inline = TRUE),  # placeholder in case want to enable this for testing
                          h5('Click Start, then wait until table appears below'),  # h5 is via shiny via htmltools
                          ## show estimated minutes needed # ###
                          shiny::textOutput(ns('speed'))
                          # bookmarkButton( label = "Save your work to come back later")        # placeholder in case want to work on this later # https://mastering-shiny.org/action-bookmark.html
                        ), # end of column for controls to left of Map ## ##
                        # RESULTS MAP  (RH column) ####
                        column(
                          8,
                          leaflet::leafletOutput(ns('mapout'), height = '600px', width = '100%'),
                          h6('Red on map indicates overlapping buffers, where residents would be double-counted if aggregating across sites'),
                          shiny::radioButtons(ns('cluster_highlighting_on'), label = 'Highlight overlaps?', choiceNames = c('Y', "N"), choiceValues = c(TRUE, FALSE), inline = TRUE)
                        ) # end map column
                        # end fluid row of controls and map, above Results Table ## ##
                      ), # end fluid row of controls and map, above Results Table

                      # can put tabs here, not at top
                      # RESULTS TABLE    ####
                      fluidRow(
                        column(
                          12,

                          ## DOWNLOAD button ####
                          shiny::uiOutput(ns('downloadButton_ui')),
                          # shiny::selectInput(ns("highlight_color_in"),label = 'color for points matching selected table rows', choices = c('red', 'blue', 'green', 'orange', 'purple', 'darkred', 'darkgreen'),multiple = FALSE),
                          br(),
                          # shiny::uiOutput(ns('tabletips_button_ui')),
                          ##                                           shiny::actionButton(ns('tabletips_button'), 'Tip on using this table'), ## tips on using table ### #
                          ## show table ##  ##
                          # #    show uploaded input table after an upload of points OR results once calculated    if that is most recent change
                          #                                DT::DTOutput(ns('rendered_results_table'))
                          shiny::uiOutput(ns('table_ui')),

                          ## pick style for names for columns ##  ##
                          shiny::uiOutput(ns('renameButton_ui'))
                          # end of column for results table ## ##
                        ) # end of column for results table
                      ) # end fluid row results table
      ),  # end main tab of api app
      # *********************************************************************************************************


      #---------------------GRAPHICS/PLOT tab--------------------- ##########
      #  DISABLED FOR NOW - WOULD WANT A BETTER PLOT THAN THIS BOXPLOT OF SITES
      # tabPanel(title =  "Graphics",
      #   tags$br(),
      #   # graphic/plot #
      #   shiny::plotOutput(ns('plot1out')),
      #   tags$br()
      # )  ,
      ######################################################## #
      # --------------------- ADVANCED settings tab--------------------- ##########
      ######################################################## #

      shiny::tabPanel(title =  " ", # semi-hidden tab - if you click on the empty space you can open this tab

        h3("Advanced settings & experimental features for EJScreen API tool - Not all of these are implemented or tested!"),

        # SET DEFAULTS / OPTIONS

        # * Each time a user session is started, the application-level option set is duplicated, for that session.
        # * If the options are set from inside the server function, then they will be scoped to the session.

        ######################################################## #
        ##  ------------------------ Options in general & Testing ## ##

        # checkboxInput(ns('print_uploaded_points_to_log'), label = "Print each new uploaded lat lon table full contents to server log", value = T),

        ######################################################## #
        ## Options in site point uploads, radius  ####

        ### ------------------------ limits on # of points ####




        numericInput(ns('max_pts_upload'), label = "Cap on number of points one can UPLOAD, additional ones in uploaded table get dropped entirely",
                     min = 1000,  step = 500,
                     value = default_max_pts_upload,
                     max =        maxmax_pts_upload),
        numericInput(ns('max_pts_map'), label = "Cap on number of points one can MAP",
                     min = 500,  step = 100,
                     value = default_max_pts_map,
                     max =        maxmax_pts_map),
        numericInput(ns('max_pts_showtable'), label = "Cap on number of points to be rendered for display in DT interactive TABLE (uploads or results)",
                     min = 100, step = 100,
                     value = default_max_pts_showtable,
                     max =        maxmax_pts_showtable),
        numericInput(ns('max_pts_run'), label = "Cap on number of points one can request RESULTS for in one batch",
                     min = 1000,  step = 100,
                     value = default_max_pts_run,
                     max =        maxmax_pts_run),



        ### ------------------------ Options for Radius  #####

        numericInput(ns('default_miles'), label = "Default miles radius",  # this will get ignored here probably, and just set by reactive passed to module
                     min = 0.25,
                     value = default_default_miles, # value =    1, # default_default_miles was set in module-specific global.R
                     #   default_radius_react(),  # default_radius_react() to be used here would need to be passed to UI of module from outer app !
                     max   =     max_default_miles),
        numericInput(ns('max_miles'), label = "Maximum radius in miles",
                     value = default_max_miles,
                     max        = maxmax_miles),
















        ######################################################## #

        ##  ------------------------ Options in calculations & what stats to output ## ##

        ### calculate and/or include in downloaded outputs

        checkboxInput(ns('include_ratios'),
                      label = "Results should include ratios to US and State averages",
                      value = default_calculate_ratios),
        checkboxInput(ns('include_averages'),
                      label = "Results should include US and State Averages - not implemented yet",
                      value = default_include_averages),
        checkboxInput(ns('include_extraindicators'),
                      label = 'Results should include extra indicators from Community Report - not implemented yet',
                      value = default_include_extraindicators),
        ######################################################## #

        ## >Options for viewing results  ####

        textInput(ns('prefix_filenames'), label = "Prefix to use in default file names when downloading [NOT implemented yet]", value = ""),

        ### ------------------------ map colors, weights, opacity ####
        ###
        numericInput(inputId = "circleweight_in", label = "weight of circles in maps", value = default_circleweight),

        # opacitymin   <- 0
        # opacitymax   <- 0.5
        # opacitystep  <- 0.025
        # opacitystart <- 0.5
        # opacityratio <- 2 / 5
        # base_color_default      <- "blue"  ;
        # cluster_color_default   <- "red"   ;
        # highlight_color_default <- 'orange';









        ######################################################## #

        ### Excel formatting options ?? ----------------- #


        # heatmap column names


        # heatmap cutoffs for bins


        # heatmap colors for bins


        # not used:
        checkboxInput(ns("ok2plot"),
                      label = "OK to try to plot graphics and include in Excel download",
                      value = default_ok2plot)

        ######################################## #

        # ## pick names for columns #
        # shiny::uiOutput(ns('renameButton_ui'))
      ) # end advanced tabPanel

      # end ejscreenapi_tabset tabsetpanel
    ) # end ejscreenapi tabsetpanel

  ) # end taglist
} # end UI module function
# ________________________________________________________ ####

############################################################################################################## #
############################################################################################################## #
############################################################################################################## #
############################################################################################################## #
############################################################################################################## #


# .####

#      ********************   SERVER ******************** <<<< ####
# .####

# 1. SPECIFY PLACES #########################
# . ####
# ###################   #####################   #####################   #################### #

#' mod_ejscreenapi_server
#'
#' @noRd
mod_ejscreenapi_server <- function(id, session,
                                   # default_radius_react,
                                   # default_points_react = NULL, # do we wrap NULL in reactive() in this case?
                                   default_points_shown_at_startup_react,
                                   # default_points = NULL,  #
                                   use_ejscreenit = FALSE  #  use_ejscreenit = FALSE # SWITCH TO TRUE WHEN CONFIRM IT WORKS ***
) {
  # figure out what default points to show at startup, which could be a reactive passed here from the parent server/overall app.
  # if (!missing(default_points)) default_points_shown_at_startup <- default_points # stop using this and use reactive input only
  # if (!missing(default_points_react)) {
  #   # ?
  #   stopifnot(is.reactive(default_points_react))
  # }
  # stopifnot(!is.reactive(default_points_shown_at_startup)) # currently this is not supposed to be reactive but would be if we wanted app-controlled default not just passed when calling function
  # stopifnot(!is.reactive(parameter2))

      source(system.file("global_EJAMejscreenapi.R", package = "EJAM"))  ### IMPORTANTLY THESE MUST STAY WITHIN THE MODULE NAMESPACE, AND NOT BE AVAILABLE TO OR INTERFERE WITH MAIN APP'S DEFAULTS/ETC.

  # moduleServer ####

  shiny::moduleServer( id, function(input, output, session) {
    ns <- session$ns

# Compare and consolidate/ merge these - unclear which is newer, and they may each have some updates others dont:
    #  - mod_ejscreenapi_server()      in MODULE_ejscreenapi.R 
    #  - app_server_EJAMejscreenapi()  in app_server_EJAMejscreenapi.R  - was deployable server code
    #  - "standalone" branch of the old EJAMejscreenapi package/ repo, that was the deployed app.
    #
    ## Code in nonexported mod_ejscreenapi_server() was initially based on the former
    ##  EJAMejscreenapi package version of app_server(), which was also the starting point for 
    ##  EJAM package version of app_server_EJAMejscreenapi()
    ##
    ##  So the code below was copied from the app server body, right after this line:
    ##       app_server <- function(input, output, session) {
     
    ###       or....
    ### To use that non-module server function here wrapped as a module, try this: 
    #
    # app_server_EJAMejscreenapi(input = input, output = output, session = session) 
    # stop()
    
    
    output$testinfo_sessionuser <- renderPrint(session$user)

    asynchronous <- FALSE
    # ____________________________________________________________________ ###### #

    # clean up global env when done with app ?### #
    # if the user had any of these on purpose before launching app, this might be a problem to remove them?
    # but check which envt this happens in... globalenv or within function only
    on.exit({ # these are leftovers created by the shiny app via global.R probably
      rm(list = intersect(ls(), c("apptitle", "asExcel", "base_color_default", "circleweight",
                                  "cluster_color_default", "echo_message", "echo_url", "highlight_color_default", "ips",
                                  "default_max_pts_upload",
                                  # "input$max_pts_map",
                                  "default_max_miles",  "maxmax_miles",
                                  "minradius", "opacitymax",
                                  "opacitymin", "opacityratio", "opacitystart", "opacitystep",
                                  "perhourfast", "perhourguess", "perhourslow", "stepradius", "tabletips_message", "whichip")))
    })
    # # ###
    # ________________________________    ##  ##
    # If ECHO search button clicked, Show popup window ####

    bindEvent(observe({
      showModal(
        modalDialog(title = "Use ECHO facility search tools to specify list of sites",
                    echo_message,
                    shiny::HTML(paste('<a href=\"', echo_url, '\", target=\"_blank\" rel=\"noreferrer noopener\">', echo_url,  '</a>', sep = '')),
                    easyClose = TRUE)
      )
    }), input$echobutton)

    #  pts() = (in module's own namespace) the Uploaded points /locations ####
    ############################################################# #
    ############################################################# #
    ############################################################# #
    
    pts <- shiny::reactive({
      
      # THIS GETS UPDATED WHEN THERE IS A CHANGE IN input$pointsfile
      ## if not uploaded yet, show default example. ####
      if (is.null(input$pointsfile)) {
        pts_filecontents <- default_points_shown_at_startup_react()  # defined in global.R
        # }
      } else {

        # try to read the file ####
        cat(paste0('Trying to upload a file',  '\n'), file = stderr()) # just prints to console
        filepath <- input$pointsfile$datapath

        confirmed_read_and_clean_points_works <- FALSE # ***
        # ____________________________________________________________________ ### ## ##

        if (confirmed_read_and_clean_points_works) {
          pts_filecontents <- read_and_clean_points(filepath, default_points = default_points_shown_at_startup)  # once it is confirmed to work
          # SHOULD RESTRICT NUMBER ALLOWED TO UPLOAD HERE IF A FUNCTION GETS USED HERE
        } else {

          ################################################ ***
          ## ____DELETE THIS CHUNK from server.R once confirm read_and_clean_points() will work, but it requires frs datasets. ####
          pts_filecontents <- read_csv_or_xl(filepath)

          n <- NROW(pts_filecontents)
          if (n > input$max_pts_upload) {
            ## We should disable upload of a crazy number of points like 30,000
            cat('Tried to upload ', n, " points, but max upload currently allowed in this app is ",
                format(input$max_pts_upload,big.mark = ',', scientific = FALSE)," points - extra points are being dropped.\n", file = stderr())
            showModal(modalDialog(title = "Error", paste0("Max # of points currently enabled for upload here is ", format(input$max_pts_upload,big.mark = ',', scientific = FALSE), ". Keeping only the first ", input$max_pts_upload, " points!"), easyClose = TRUE))
            exceededmax <- TRUE
            # pts_filecontents <- NULL # to reject the upload and go back to using default points from start of app. would be nice to keep whatever last upload was but did not bother to code that way.
            pts_filecontents <- pts_filecontents[1:input$max_pts_upload, ]
            n <- NROW(pts_filecontents)
          }

          # if uploaded with zero length somehow
          if (0 == length(pts_filecontents)) {
            pts_filecontents <- default_points_shown_at_startup  # defined in global.R # NOT SURE THIS IS POSSIBLE
          } else {

            pts_filecontents <- latlon_df_clean(pts_filecontents)
            #    a set of latlon cleaning functions using latlon_infer(), latlon_as.numeric(), latlon_is.valid()

            if ('lat' %in% names(pts_filecontents) & 'lon' %in% names(pts_filecontents)) {
              # ALL SET - using lat/lon
              if (('registry_id' %in% names(pts_filecontents) ) | ('pgm_sys_id' %in% names(pts_filecontents))) {
                showModal(modalDialog(title = 'Warning', 'lat/lon found, so ignoring registry_id/pgm_sys_id', easyClose = TRUE))
                cat( 'lat/lon found, so ignoring registry_id/pgm_sys_id \n', file = stdout())
              }
            } else {
              if ('FacLong' %in% names(pts_filecontents) & 'FacLat' %in% names(pts_filecontents)) {
                #  ECHO column names  - but latlon_infer() has already renamed them anyway, actually so we can't get here probably
                names(pts_filecontents) <- gsub('FacLat', 'lat', names(pts_filecontents)); names(pts_filecontents) <- gsub('FacLong', 'lon', names(pts_filecontents)) # as used by leaflet, and so names are unique even when uploaded table is merged with EJScreen results
                # the variable names latitude and longitude are compatible with leaflet() but we will not rename them except for that one purpose right when mapping
                # ALL SET - using FacLat/FacLong
                if (('registry_id' %in% names(pts_filecontents) ) | ('pgm_sys_id' %in% names(pts_filecontents))) {
                  showModal(modalDialog(title = 'Warning', 'lat/lon found, so ignoring registry_id/pgm_sys_id', easyClose = TRUE))
                  cat( 'lat/lon found, so ignoring registry_id/pgm_sys_id \n', file = stdout())
                }
              }  else {
                if ('registry_id' %in% names(pts_filecontents)) {

                  ### QUERY FRS for lat lon via facility registry ID ####

                  # THIS COULD BE REPLACED WITH EJAM CODE THAT USES FRS FILE ON SERVER TO DO THIS QUERY, NOT AN API ***
                  showModal(modalDialog(title = "Please Wait", paste0("querying FRS based on facility registry_id to get lat and lon (ignores pgm_sys_id column since registry_id is present)", ''), easyClose = TRUE))
                  cat(paste0("querying FRS based on facility registry_id to get lat and lon (ignores pgm_sys_id column since registry_id is present)", '\n'), file = stdout())
                  x <- try(locate_by_id(id = pts_filecontents$registry_id, type = 'frs'))
                  # error handling could go here
                  pts_filecontents$lat <- as.numeric(x$Latitude83)
                  pts_filecontents$lon <- as.numeric(x$Longitude83)
                  if ('pgm_sys_id' %in% names(pts_filecontents)) {
                    showModal(modalDialog(title = 'Warning', 'registry_id found, so ignoring pgm_sys_id', easyClose = TRUE))
                    cat('registry_id found, so ignoring pgm_sys_id \n', file = stdout())}
                  # SOME CODE ASSUMES INPUT POINTS MATCH 1 TO 1 OUTPUT POINTS- CREATES PROBLEM IF INPUT ROW COUNT DIFFERS FROM OUTPUT ROW COUNT, WHICH MAYBE COULD HAPPEN FOR QUERY ON ID, AND ESPECIALLY IF QUERY ON NAICS, FOR EXAMPLE.
                } else {
                  ### QUERY FRS for lat lon via program system ID ####

                  if ('pgm_sys_id' %in% names(pts_filecontents)) {

                    # THIS COULD BE REPLACED WITH EJAM CODE THAT USED FRS FILE ON SERVER TO DO THIS QUERY, NOT AN API ***
                    showModal(modalDialog(title = "Please Wait", paste0("querying FRS based on facility pgm_sys_id to get lat and lon", ''), easyClose = TRUE))
                    cat(paste0("querying FRS based on facility pgm_sys_id to get lat and lon", ''), file = stdout())
                    x <- try(locate_by_id(id = pts_filecontents$pgm_sys_id, type = 'program'))
                    # error handling could go here
                    pts_filecontents$lat <- as.numeric(x$Latitude83)
                    pts_filecontents$lon <- as.numeric(x$Longitude83)
                    # SOME CODE ASSUMES INPUT POINTS MATCH 1 TO 1 OUTPUT POINTS- CREATES PROBLEM IF INPUT ROW COUNT DIFFERS FROM OUTPUT ROW COUNT, WHICH MAYBE COULD HAPPEN FOR QUERY ON ID, AND ESPECIALLY IF QUERY ON NAICS, FOR EXAMPLE.
                  } else {
                    
                    ## could try to handle FIPS here, via  # ## #
                    if ('fips' %in% names(pts_filecontents)) {
                     pts_filecontents$lat = NA; pts_filecontents$lon = NA # just in case other code looks for them 
                    } else {
                    
                    showModal(modalDialog(title = "Error", paste0("The file must have columns named lat and lon, or registry_id, or pgm_sys_id or fips. Headers must be in row 1, data starting in row 2.", ''), easyClose = TRUE))
                    cat(paste0("The file must have columns named lat and lon, or registry_id, or pgm_sys_id. Headers must be in row 1, data starting in row 2.", '\n'), file = stdout())
                    pts_filecontents <- default_points_shown_at_startup  # defined in global.R   This line is so default example is shown instead of uploaded file that does not have correct columns
                    
                    } ########## #
                    
                  }
                }
              }
            } }

          ############################################### #
          ## ____DELETE the chunk above when replaced by function]_________###############################################
          ##  ____________________________________________________________________ ###   ## ##

        }
        cat('Upload done\n')

        # dump lat lon values etc to server logs for now
        if (!interactive()) {
          cat(' FILE CONTENTS THAT WERE UPLOADED: \n\n', file = stdout())
          print(names(pts_filecontents))
          cat('\n\n\n', file = stdout())
          print(pts_filecontents[ , c("lat", "lon")])
          cat('\n\n\n', file = stdout())
          print(pts_filecontents[ 1:10, ])
          cat('\n\n\n', file = stdout())
        }

        if (n > input$max_pts_run) {
          # we will warn them and claim you cannot analyze this many, but will not disable the attempt to click start
          cat('Uploaded ', n, " points, but max you can analyze at once is ", format(input$max_pts_run,big.mark = ',', scientific = FALSE)," points - extra points are being dropped.\n", file = stdout())
          showModal(modalDialog(title = "Warning", paste0(
            "Maximum number of points you can analyze in 1 batch here is ", format(input$max_pts_run,big.mark = ',', scientific = FALSE),
            " and extra points have been dropped. For a very large number of locations it may be best to run 1 batch at a time, download results for each batch, and simply copy those into one spreadsheet. ",
            speedmessage(500), ", for example. "), easyClose = TRUE))
          exceededmax <- TRUE

          pts_filecontents <- pts_filecontents[1:input$max_pts_run, ]
          n <- NROW(pts_filecontents)
        }
      }

      if ('fips' %in% names(pts_filecontents & all(is.na(pts_filecontents$lat))) ) {
        mapurl <- url_ejscreenmap(wherestr = fips2name( pts_filecontents$fips)) # no  namestr = param
      } else {
        mapurl <- url_ejscreenmap(lat = pts_filecontents$lat, lon = pts_filecontents$lon)  # e.g.,  "https://ejscreen.epa.gov/mapper/index.html?wherestr=35.3827475,-86.2464592"
      }
      pts_filecontents$mapurl  <- paste0('<a href=\"', mapurl, '\", target=\"_blank\" rel=\"noreferrer noopener\">EJScreen Map ', rownames(pts_filecontents), '</a>')
      
      pts_filecontents
    }) # END OF pts() reactive
    ############################################################# #
    ############################################################# #
    ############################################################# #
    
    # >>>>>> RADIUS input: UI and text and limit for radius slider: Miles / Km ####
    # Used either typed in radius or slider ####
    #  allow radius to get typed into a text box as an alternative !!! ***
    
    if (!exists("minradius")) {minradius <- 0.5}
    if (!exists("stepradius")) {stepradius <-  0.05 }
    output$radius_slider <- renderUI({sliderInput(inputId = ns("radius_via_slider"),
                                                  label = paste0("Radius of ",     input$default_miles, " miles ",
                                                                 paste0("(", round(input$default_miles      * meters_per_mile / 1000, 3), ' km)')),
                                                  value =                          input$default_miles,
                                                  min = minradius, max = input$max_miles, step = stepradius
    ) })
    # minradius and stepradius are defined in   global_EJAMejscreenapi.R file

    shiny::observe({     shiny::updateSliderInput(session = session, inputId = ns('radius_via_slider'),
                                                  label = paste0("Radius of ",     input$radius_via_slider, " miles ",
                                                                 paste0("(", round(input$radius_via_slider * meters_per_mile / 1000, 3), ' km)'))
                                                  #   throttle(input$radius_via_slider, millis = 200)   # this would prevent frequent UI updates as slider is moved
    ) })
    radius_via_text_validated <- reactive({
      x = input$radius_via_text
      if (is.null(x))                                  {x = input$default_miles}
      if (is.na(x) | length(x) > 1 | length(x) == 0)   {x = input$default_miles}
      x = as.numeric(x)
      if (is.null(x) | is.na(x) | length(x) > 1 | length(x) == 0) {x = input$default_miles}
      x = max(minradius, min(input$max_miles, x, na.rm = T), na.rm = T)
      if (is.null(x) | is.na(x))                      {x = input$default_miles}
      as.character(x)
    })

    output$radius_textbox <- renderUI({textInput(
      "radius_via_text",
      label = paste0("Radius of ",     input$default_miles, " miles ",
                     paste0("(", round(input$default_miles            * meters_per_mile / 1000, 3), ' km)')),
      value =             as.character(input$default_miles)
    ) })
    shiny::observe({ shiny::updateTextInput(
      session = session,
      inputId =  'radius_via_text',
      label = paste0(
        "Radius of ", radius_via_text_validated(), " miles ",
        ifelse(minradius           == as.numeric(radius_via_text_validated()), "(min.) ", ""),
        ifelse(input$default_miles == as.numeric(radius_via_text_validated()), "(default) ", ""),
        ifelse(input$max_miles     == as.numeric(radius_via_text_validated()), "(max.) ", ""),
        paste0("(",             round(as.numeric(radius_via_text_validated()) * meters_per_mile / 1000, 3), ' km)'))
    ) })

    # USE SIMPLIFIED INPUT OF RADIUS UNTIL FIX MODULE USE OF TEXT VS SLIDER INPUTS ***
    #
    if ("fixed" == "no") {
      # Use typedin radius or slider ####
      #  allow radius to get typed into a text box as an alternative !!! ***
      radius_miles <- reactive({
        if (input$slider_vs_text_radius == "type_in") {
          x = as.numeric(radius_via_text_validated())
          ### cat('radius is ', x, '\n')
        }
        if (input$slider_vs_text_radius == "use_slider") {
          x = input$radius_via_slider
          ### cat('radius is ', x, '\n')
        }
        x
      })
    } else {
      # USE SIMPLIFIED INPUT OF RADIUS UNTIL FIX MODULE USE OF TEXT VS SLIDER INPUTS
      radius_miles <- reactive({ input$SIMPLERADIUS })
    }
    ############################################################# #
    ############################################################# #
    ############################################################# #
    # . ####

    ## _... Output Text about point counts, radius in km, etc. ####

    output$count       <- renderText({paste0(NROW(pts()),    ' points have been uploaded.')})
    # is this always identical to number of points in results table, since fills in row even if bad lat/lon? not sure it does!
    # or no blockpoint in circle?
    # and if upload is NAICS etc. then pts() and results_table() will have different counts!
    # output$max_pts_upload_txt  <- renderText({paste0(format(input$max_pts_upload,big.mark = ',', scientific = FALSE), ' points max. can be uploaded.')})
    # output$max_pts_map_txt     <- renderText({paste0(format(input$max_pts_map,   big.mark = ',', scientific = FALSE), ' points max. can be shown on a map.')})
    # output$max_pts_showtable_txt <- renderText({paste0(format(input$max_pts_showtable, big.mark = ',', scientific = FALSE), ' points max. can be shown on the interactive table.')})
    # output$max_pts_run_txt       <- renderText({paste0(format(input$max_pts_run   ,big.mark = ',',     scientific = FALSE), ' points max. can be analyzed.')})
    output$speed <- renderText({speedmessage(NROW(pts()), perhourslow = perhourslow, perhourfast = perhourfast, perhourguess = perhourguess)})  # speedmessage defined as function in R folder, default variables defined in global.R
    ################################################################################################### #


    # ####################   #####################   #####################   #################### #


    ####################   #####################   #####################   #################### #

    ### DISPLAY input TABLE     #####################
    ####################   #####################   #####################   #################### #

    output$rendered_input_table <- DT::renderDT({
      if (NROW(pts()) > input$max_pts_showtable) {
        pts()[1:(input$max_pts_showtable), ]
        warning('Cap on rows to display in interactive table is limiting how many can be seen but all can be downloaded.')
      } else {
        pts()
      }
    },
    options = list(
      dom = 'rtip', # default table has 5 DOM elements: search box, length menu, info summary, pagination control, table. Display a subset of elements via dom=
      scrollX = TRUE
    ),
    escape = FALSE
    ) # *** CAUTION -- MAY NEED TO CHANGE THIS ***  end of output$rendered_input_table <- DT::renderDT
    # escape=TRUE is better for security reasons (XSS attacks).
    # escape=FALSE fixes ejscreen pdf URL to be a link,
    # but it breaks links from ECHO table download (wont be clickable in table) ***
    ############################################################## #


    # ####################   #####################   #####################   #################### #

    # ______________________  ####
    # . ####
    # 2. USE EJSCREEN API    ####
    # . ####
    # using  params: pts() , maxpts , whichip , radius_miles(), but not blockgroupdata, since that is via the API    #################### #
    ####################   #####################   #####################   #################### #

    # GET RESULTS ######################################

    results_table <- reactive({
      # bindEvent makes it wait until click run/start button
      # ignoreInit = TRUE, If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
      # ignoreNULL = FALSE would be desirable if you did want to initially perform the action and just let user re-initiate it (like a "Recalculate" button).
      results_table <- 0
      exceededmax <- FALSE
      results_table <- isolate({
        # Not sure isolate is still essential now that it uses bindEvent(),
        # but isolates this to avoid table refresh every time pts or radius changes but Start button not yet pushed -
        # not isolating it would be a problem espec where it does cbind(pts(), batchtableout)
        #  since new list of uploaded points would have different number of rows than last set of results...
        # Do not want to prepend new uploaded points to existing batch outputs from last set of points.

        if (length(pts()) != 0) {
          n <- NROW(pts())

          ## *_Progress Bar* ####
          progress <- shiny::Progress$new(min = 0, max = n)
          progress$set(message = "Please Wait - Requesting buffer reports", value = 0)
          on.exit(progress$close()) # to close progress bar when this reactive exits (even if by error)
          updateProgress <- function(value = NULL, message_detail = NULL, message_main = "Please wait") {
            # Create a callback function - When called, it sets progress bar to value.
            if (is.null(value)) { # - If value is NULL, it will move the progress bar 1/5 of the remaining distance.
              value <- progress$getValue()
              value <- value + (progress$getMax() - value) / 5
            }
            progress$set(value = value, message = message_main, detail = message_detail)
          }
          showModal(modalDialog(
            title = "RESULTS TABLE LOADING - PLEASE WAIT...",
            speedmessage(NROW(pts()), perhourslow = perhourslow, perhourfast = perhourfast, perhourguess = perhourguess),
            size = "l", footer = NULL
          ))
          ############################################################## #

          # NOTE MAY REPLACE THIS SECTION OF CODE WITH ejscreenapi_plus() which does this itself  :
          #   or maybe even use  ejscreenit() or ejscreenit_for_ejam()  

          # *_EJScreen API Get Results*  ####
          # BUT IT DOES NOT SUPPORT updateProgress like ejscreenapi() does, so far.
          if (use_ejscreenit) {
            allout <- ejscreenit(
              x = pts(), # ignored if fips not NULL
              radius = radius_miles(),
              fips = pts()$fips, # will be NULL if no such column
              # namestr = namestr,
              nosave = T, nosee = T, save_map = F, save_plot = F, save_table = T, interactiveprompt = F,
              calculate_ratios = input$include_ratios
            )
            results_table <- allout$table
            allout <- NULL # unless you want to also use allout$map for example
          } else {
            # not using ejscreenit() so using escreenapi() and urls_clusters_and_sort_cols() etc directly
            results_table <- ejscreenapi(
              lon = pts()$lon, lat = pts()$lat,  # ignored if fips not NULL
              radius = radius_miles(), unit = 'miles', wkid = 4326 ,
              fips = pts()$fips, # will be NULL if no such column
              # namestr = namestr,
              format_report_or_json = 'pjson', ipurl = whichip,  # was  input$whichip
              report_every_n = report_every_n_default,
              save_when_report = FALSE, on_server_so_dont_save_files = TRUE,
              updateProgress = updateProgress, # updateProgress is a function that has been defined already and gets passed here to the slow function that needs to report progress bar updates
              drop_redundant_indicators = TRUE,
              getstatefromplacename = TRUE, verbose = FALSE
            )

            # if (attr(results_table, 'noresults') ) { # needs to be fixed ****
            #   showModal(modalDialog(title = "Warning", "No results - API might not be accessible or no site has data available", easyClose = TRUE))
            # }

            ########################## #
            # . ####
            # ADD MORE COLUMNS  to basic batch results ####
            ### Combine input (from user) + output (from EJ stat buffering) ####
            #      (e.g., that user input uploaded may have site name, address, etc.)
            #      & return combined table: 1 row per site (buffer), 1 col per indicator (variable).
            ### Add links to EJScreen ####
            ### Flag sites near others ####
            ### Put best cols 1st ####
            results_table <- cbind(pts(), results_table) # this would be a problem if we did not isolate or use bindEvent
            results_table <- urls_clusters_and_sort_cols(results_table)
            
            results_table <- results_table[, names(results_table) != 'mapurl']   # drop this column that was only useful while viewing uploaded points but is redundant in final results here
          }
          ############################################################## #
          # NOTE THE VARIABLE NAMES HERE ARE AS PROVIDED BY API, NOT RENAMED TO LOWERCASE MORE OBVIOUS NAMES
          # WHILE ejscreenapi_plus() or ejscreenit()  provide renamed indicators (by default).
          # }
        }
      }) # end isolated part
      # print("names of columns in results table")
      # print(names(results_table))
      results_table

    }) %>% bindCache(pts(), radius_miles()) %>%
      bindEvent(input$runbutton ) # this makes it react only when button is pushed (or if other non-isolated reactives above change, like input$usewhichnames)
    # end of results_table() ####
    ####################   #####################   #####################   #################### #

    ##################### remove modal window once results ready ####

    observe({
      req(results_table())
      removeModal()
      # also, maybe remove UI here, to not show the input table anymore

      # maybe save the results to some server here, for large batches where the user may not see it finished! ***
    }) # removes the modal when results ready

    ############################################################# #





    # >>>>>>>  table_as_displayed_reactive(), built from results_table() ####
    table_as_displayed_reactive <- reactive({  # params: input$usewhichnames map_headernames and results_table()
      req(results_table())

      table_as_displayed <- results_table() # this results_table() is always in original names format, with no added columns.

      if (!use_ejscreenit) {

        ## Toggle column names ####
        if (is.null(input$usewhichnames)) {usewhichnames <- 'long'} else {
          usewhichnames <- input$usewhichnames
        }

        # cat('....................... usewhichnames is ', usewhichnames, '\n')
        # almost identical to code in ejscreenapi_plus() and ejamit() which relies on that:

        # colnames in results table are always api version of names
        #_Rename columns from original format to use R-friendly names ## ##
        
        names(table_as_displayed) <- fixcolnames(
          namesnow = names(table_as_displayed),
          oldtype = 'api', # because results_table()  never has renamed colnames
          newtype = 'r',
          mapping_for_names = map_headernames
        )
        ############################################################## #

        ############################################################# ############################################################## #
        ############################################################# ############################################################## #
        ############################################################# ############################################################## #

        # note: ejscreenapi_plus() vs app_server_EJAMejscreenapi vs MODULE ############################################################# #
        
        # ### Add Ratios to us or state averages ####
        #
        if (input$include_ratios ) {
          table_as_displayed <- calc_ratios_to_avg_for_ejscreenapi(table_as_displayed = table_as_displayed)
        } # end of ratio calculations

        ############################################################## #

        
        
        
        ############################################################## #
        
        #-_Commas for pop count ####
        if ('totalPop' %in% names(table_as_displayed)) table_as_displayed$totalPop <- prettyNum(round(table_as_displayed$totalPop, 0), big.mark = ',')
        if ('pop'      %in% names(table_as_displayed)) table_as_displayed$pop      <- prettyNum(round(table_as_displayed$pop, 0),      big.mark = ',')
      } # end of if (!use_ejscreenit())

      table_as_displayed

    }) # end of table_as_displayed_reactive
    ## end of table_as_displayed_reactive  <<<<<<<< ####

    #   output$rendered_results_table <- DT::renderDT ####
    output$rendered_results_table <- DT::renderDT({
      if (NROW(table_as_displayed_reactive()) > input$max_pts_showtable) {
        table_as_displayed_reactive()[1:(input$max_pts_showtable), ]
        warning('Cap on rows to display in interactive table is limiting how many can be seen but all can be downloaded.')
      } else {
        table_as_displayed_reactive()
      }

      ############################################################## #
      # TRICKY SETTING OPTIONS FOR TABLE VIEW (filter per col, fixed header, etc.)
      #
      # DT::renderDT is used here.
      #   If instead use DT::datatable(), easier to set some options like filter,
      #   like this      DT::datatable(table_as_displayed, filter = 'top')
      #   but then DT::renderDT() ignores options set below!
      # https://datatables.net/reference/option/fixedHeader.header  # to fix header row in place during scrolling down list (like freeze panes for first row)
      # https://datatables.net/reference/option/searchPanes  # a search/filter box above each column
    },
    options = list(
      selection = 'multiple',
      dom = 'rtip', # default table has 5 DOM elements: search box, length menu, info summary, pagination control, table. Display a subset of elements via dom=
      scrollX = TRUE,
      # fixedHeader = TRUE,                 # does not seem to work.  (also, does it need  scrollY = TRUE, ?)
      searchPanes = TRUE                      # does not seem to work
    ),
    # filter='top', orderCellsTop = TRUE),  # does not seem to work.
    # filter=list(position='top')           # does not seem to work.  is this how?
    # server = TRUE, # should already be the default for DT::renderDT, and is better if table is large

    escape = FALSE
    ) # *** CAUTION -- MAY NEED TO CHANGE THIS ***   end of output$rendered_results_table <- DT::renderDT
    # escape=TRUE is better for security reasons (XSS attacks).
    # escape=FALSE fixes ejscreen pdf URL to be a link,
    # but it breaks links from ECHO table download (wont be clickable in table) ***
    ############################################################## #



    ## Show inputs or results, whatever was just updated ####
    # keeps track of which is latest one updated, to display that
    most_recently_changed <- reactiveVal('upload')
    observe(label = 'results_changed', {
      results_table() # when this changes, update most_recently_changed() to be "results"
      most_recently_changed('results') #; cat('results changed\n', file = stderr())
    })  # results of analysis was the last change
    observe(label = 'upload_changed', {
      pts() # when uploaded table of points changes, update most_recently_changed() to be "upload"
      most_recently_changed('upload') # ; cat('upload changed\n', file = stderr())
    }) # uploaded dataset was the last change
    # Just for testing:
    output$most_recently_changed_text <-  renderText("") # renderText( paste0(most_recently_changed(), ' are shown in the map' ))

    ## Show inputs or results, whatever was just updated ##  ##
    output$table_ui <-  renderUI({
      # if (most_recently_changed() == 'run') DT::DTOutput('rendered_results_table')
      if (most_recently_changed() == 'upload'   ) {
        DT::DTOutput(ns('rendered_input_table'))
      } else {
        DT::DTOutput(ns('rendered_results_table'))
      }
    })

    #####################   #####################   #####################   #################### #
    #####################   #####################   #####################   #################### #
    # . ####
    ## Downloading the Table + Excel format + Links!* ####
    
    ### Buttons to Rename table header row -show only if results ready ####
    output$renameButton_ui <- renderUI({

      shiny::radioButtons(
        ns( 'usewhichnames'), label = 'Rename columns?',
        inline = TRUE,
        choiceNames = list(
          'Plain English: "National percentile for Ozone (ppb)"',
          'for analysis: "pctile.o3"',
          'from API: "N_E_O3_PER"'
        ),
        choiceValues = list(
          'long',
          'r',
          'original'
        ),
        selected = 'long'
      )
    })

    ### Button to Download table -show only if results ready and no new upload yet ####
    output$downloadButton_ui <- renderUI({
      req(results_table())
      shiny::downloadButton(ns('downloadbutton'), 'Download these results')
    })

    ### Download Handler: Excel formatting etc. ####
    output$downloadbutton <- shiny::downloadHandler(
      filename = paste0(
        #renderPrint({ invisible(input$prefix_filenames)}),
        # input$prefix_filenames, # not working yet !
        # "-",
        ifelse(asExcel, 'ejscreenapi_output.xlsx', 'ejscreenapi_output.csv')), contentType = NULL, #if NULL, it guesses based on extension
      content = function(file) {
        if (0 == length(table_as_displayed_reactive())) {
          showModal(modalDialog(title = "Warning", "Results are not created until Start is clicked", easyClose = TRUE))
        } else {
          if (NROW(pts()) != NROW(table_as_displayed_reactive())) {
            # this might mean they uploaded a new dataset and did not hit Start again. could check for that more specifically, but this is probably good enough for now. might happen if API fails to return some rows? or maybe ejscreenapi function fixed that so it always returns same number as submitted.
            showModal(modalDialog(title = "Warning", "Uploaded points and results table have different numbers of rows", easyClose = TRUE))
          }
          table_as_displayed <- table_as_displayed_reactive()

          ########################################################  #
          # prep for excel
          #
          currentnametype <- input$usewhichnames
          pctile_colnums <-  which('pctile' ==  fixcolnames(names(table_as_displayed), oldtype = currentnametype, newtype = 'jsondoc_shortvartype') ) # this should get what type each is.
         
          # fix URLs to work in csv pulled into Excel or in Excel files (as opposed to datatable shown in web brwsr)
          table_as_displayed$`EJScreen Report` <- gsub('.*(http.*)\", target=.*', '\\1', table_as_displayed$`EJScreen Report`)
          table_as_displayed$`EJScreen Map` <- gsub('.*(http.*)\", target=.*', '\\1', table_as_displayed$`EJScreen Map`)

          if (!asExcel) {
            table_as_displayed$`EJScreen Report` <- paste0('=HYPERLINK("', table_as_displayed$`EJScreen Report`,'", "EJScreen Report ', rownames(table_as_displayed), '")')
            table_as_displayed$`EJScreen Map`    <- paste0('=HYPERLINK("', table_as_displayed$`EJScreen Map`,'", "EJScreen Map ',    rownames(table_as_displayed), '")')
            readr::write_excel_csv(table_as_displayed, file)
            # write.csv(table_as_displayed, file, row.names = FALSE) # simplest
            # note despite name of function, write_excel_csv() saves it as a csv, NOT ACTUALLY excel,
            #  but writes csv faster and indicates it is UTF8 encoding to help import to excel
          } else {
            wb <- table_xls_format_api(df = table_as_displayed,
                                     hyperlink_cols = c('EJScreen Map', 'EJScreen Report'),
                                     heatmap_colnames = names(table_as_displayed)[pctile_colnums],
                                     heatmap_cuts = c(80, 90, 95),
                                     heatmap_colors = c('yellow', 'orange', 'red'))
            openxlsx::saveWorkbook(wb, file = file) # like  openxlsx::write.xlsx(table_as_displayed, file = file)
          }
          ########################################################  #
        }
      }
    ) # end of download handler


    ### Table tips - show only if results ready ####
    # output$tabletips_button_ui <- renderUI({
    #   req(results_table())
    #   shiny::actionButton(ns('tabletips_button'), 'Tip on using this table')  ## tips on using table
    # })
    ####################   #####################   #####################   #################### #
    ### Table tips - text box (about interactive data table) ####

    bindEvent(observe({
      showModal(
        modalDialog(
          title = "Using the interactive table of results",
          tabletips_message,
          # HTML(paste('<a href=\"', echo_url, '\", target=\"_blank\" rel=\"noreferrer noopener\">', echo_url,  '</a>', sep = '')),
          easyClose = TRUE
        )
      )
    }), input$tabletips_button )

    # ____________________________________________________________________ ####### 
    
    # ______________________  ####
    # . ####
    # 3. MAP  ####################
    # . ####
    # using  params:  ####    ### #
    ####################   #####################   #####################   #################### #

    ##  Map set up params ####

    ### colors to use for points (circles) on map ####
    base_color      <- reactiveVal(base_color_default)
    cluster_color   <- reactiveVal(cluster_color_default)
    highlight_color <- reactiveVal(highlight_color_default)

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ### is_clustered (where one may double-count people):  ####

    is_clustered <- reactive({
      # i.e., which sites have residents that might also be near others sites?
      # circles overlap if 2 facilities are twice the radius apart  # in miles
      # check either pts() or results_table() depending on value of most_recently_changed() being 'upload' or 'results'
      # Do not limit by cap on mapping yet, since this info gets used in table download also which has a larger cap or no cap.
      if (most_recently_changed() == 'results') {x <-  distance_near_eachother(lat = results_table()$lat, lon = results_table()$lon, distance = 2 * radius_miles()) }
      if (most_recently_changed() == 'upload')  {x <-  distance_near_eachother(lat = pts()$lat,           lon = pts()$lon,           distance = 2 * radius_miles()) }
      x
    })

    ### Data for popups for map #############################################

    popup_to_show <- reactive({
      # Whether to display popups on map using uploaded point data or results of EJ stats run.
      # cat('\n', 'The value of most_recently_changed() is now ', most_recently_changed(), '\n', file = stderr())
      if (most_recently_changed() == 'results') {x <- (popup_ejstats())}
      if (most_recently_changed() == 'upload')  {x <- (popup_uploadedpoints())}
      x
    })
    
    popup_uploadedpoints <- reactive({
      #  RESTRICT NUMBER OF POINTS MAPPED AND POPUPS TOO, HERE, BASED ON CAP input$max_pts_map
      popup_from_uploadedpoints(
        pts()[1:min( input$max_pts_map, NROW(pts())), ]
      )
    })
    
    popup_ejstats <- reactive({
      # cat('recalculating popup_ejstats since results table changed\n')
      #  create popups for map, using EJ stats from buffer analysis
      req(results_table())
      out <- results_table()
      #  RESTRICT NUMBER OF POINTS MAPPED AND POPUPS TOO, HERE, BASED ON CAP  input$max_pts_map
      if (NROW(out) > 0) {
        out <- out[1:min(input$max_pts_map, NROW(out)), ]
        names(out) <- fixcolnames(names(out), oldtype = 'api', newtype = 'r', mapping_for_names = map_headernames)
        mypopup <- popup_from_ejscreen(out = out)
      } else {
        warning('results_table() has zero rows of data, so cannot create popups')
        mypopup <- NULL # popup_uploadedpoints()
      }
      mypopup
    })

    ################################################### #
    ## CREATE FRESH MAP if new pts(), in renderLeaflet() ####
    ################################################### #
    output$mapout <- leaflet::renderLeaflet({
      mypoints <- pts()

      if (NROW(mypoints) > input$max_pts_map) {
        mypoints <- mypoints[1:input$max_pts_map, ]
        showModal(modalDialog(title = "Warning", paste0("The map can display only the first ", input$max_pts_map, ' all ', NROW(mypoints),' points.'), easyClose = TRUE))
        warning( paste0("The map can display only the first ", format(input$max_pts_map,big.mark = ',', scientific = FALSE), ' all ', NROW(mypoints), ' points.'))
      }
      names(mypoints) <- gsub('lon','longitude', names(mypoints)); names(mypoints) <- gsub('lat','latitude', names(mypoints))
      if (length(mypoints) != 0) {
        isolate({ # do not redraw entire map and zoom out and reset location viewed just because radius changed?
          mymap <- leaflet(mypoints) %>% addTiles()  %>%
            addCircles(lat = ~latitude, lng = ~longitude,
                       radius = radius_miles() * meters_per_mile,
                       color = base_color(), fillColor = base_color(), fill = TRUE, weight = input$circleweight_in,
                       # opacity = input$opaquecircles, fillOpacity = max(0, opacityratio * input$opaquecircles - opacitystep),
                       popup = popup_to_show() #  already have RESTRICTED NUMBER OF POPUPS BASED ON CAP input$max_pts_map
            )
          #### %>% clearBounds() # clears the bound, so that the view will be automatically determined by the range of latitude/longitude data in the map layers if provided;
          mymap
        })
      } else {  # length(mypoints) == 0
        mymap <- leaflet() %>% addTiles() %>% setView(-110, 46, zoom = 3)
        mymap
      }
      ### Button to print map ####
      leaflet.extras2::addEasyprint(map = mymap, options = leaflet.extras2::easyprintOptions(exportOnly = TRUE, title = 'Save Map Snapshot'))
    })

    ################################################### #
    ## UPDATE MAP VIA leafletProxy() ####
    ################################################### #
    ### Update popups and/or radius of circles ####
    observe({ # this displays the proxy map that changes if radius or selected rows change, etc.
      mypoints <- pts(); names(mypoints) <- gsub('lon','longitude', names(mypoints)); names(mypoints) <- gsub('lat','latitude', names(mypoints))

      if (NROW(mypoints) > input$max_pts_map) {
        mypoints <- mypoints[1:input$max_pts_map, ]
      }

      if (length(mypoints) != 0) {
        # radius_now_meters <- throttle(r = radius_miles(), millis = 200) * meters_per_mile # not quite right. tried radius_now_meters() in 3 places below.
        radius_meters <- radius_miles() * meters_per_mile
        mylivemap <- leaflet::leafletProxy("mapout", data = mypoints) %>% clearShapes() %>%
          addCircles(lat = ~latitude, lng = ~longitude,
                     radius = radius_meters,
                     color = base_color(), fillColor = base_color(), fill = TRUE, weight = input$circleweight_in,
                     # opacity = input$opaquecircles, fillOpacity = max(0, opacityratio * input$opaquecircles - opacitystep),
                     popup = popup_to_show() #  already have RESTRICTED NUMBER OF POPUPS BASED ON CAP input$max_pts_map
          )
        ### Button to print map ####
        leaflet.extras2::addEasyprint(map = mylivemap, options = leaflet.extras2::easyprintOptions(exportOnly = TRUE, title = 'Save Map Snapshot'))

        ### color points if clustered  ####
        #
        if (input$cluster_highlighting_on) {
          some <- mypoints[is_clustered()[  1:min(input$max_pts_map, NROW(is_clustered()))  ], ]
          addCircles(lat = some$latitude, lng = some$longitude,
                     map = mylivemap, radius = radius_meters,
                     color = cluster_color(), fillColor = cluster_color(), fill = TRUE, weight = input$circleweight_in ,
                     # opacity = input$opaquecircles, fillOpacity = max(0, opacityratio * input$opaquecircles - opacitystep),
                     popup = popup_to_show()[ is_clustered()[  1:min(input$max_pts_map, NROW(is_clustered()))  ] ] # must restrict count of is_clustered here but not overall since all kept for table output  #  already have RESTRICTED NUMBER OF POPUPS BASED ON CAP input$max_pts_map
          )
        }

        ### color points if selected in table   ####
        #
        if (most_recently_changed() == 'upload')  {selected_rows <- input$rendered_input_table_rows_selected}
        if (most_recently_changed() == 'results') {selected_rows <- input$rendered_results_table_rows_selected}
        # cat('selected rows: \n'); print(selected_rows)
        # limit these, in case only mapping the max allowed - can select rows bigger than map cap
        selected_rows <- selected_rows[selected_rows <= input$max_pts_map]
        # cat('selected rows: \n'); print(selected_rows)
        # selected_rows <- ifelse(most_recently_changed() == 'upload',
        #                         input$rendered_input_table_rows_selected,
        #                         input$rendered_results_table_rows_selected)
        # cat('rows selected: ', selected_rows, '\n') #
        # To enable highlighting points on map based on row selected in table,
        # Selected rows in table have these row numbers: input$results_table_cells_selected
        if (length(selected_rows) > 0) {
          # cat('some rows are selected\n')
          highlighted <- mypoints[selected_rows, ] #
          addCircles(
            map = mylivemap, lat = highlighted$lat , lng = highlighted$lon , radius = radius_meters,
            # color = input$highlight_color_in, fillColor = input$highlight_color_in,  # enable in ui if want to use this option
            color = highlight_color(), fillColor = highlight_color(),
            opacity = 0.9, fill = TRUE, weight = input$circleweight_in,
            # opacity = input$opaquecircles, fillOpacity = max(0, opacityratio * input$opaquecircles - opacitystep),
            popup = popup_to_show()[ selected_rows ]   #  already have RESTRICTED NUMBER OF POPUPS BASED ON CAP input$max_pts_map, and same for selected_rows
          )
        }

        ### color points by category? *** ####
        ## example of using Categorical colors (using the "RdYlBu" colorbrewer palette, mapped to categories)
        # RdYlBu <- colorFactor("RdYlBu", domain = categories)
        # color = ~RdYlBu(category)

        ### color points by indicator score? *** ####
        ### example of using Continuous colors (using the "Greens" colorbrewer palette, mapped to value)
        # greens <- colorNumeric("Greens", domain = NULL)
        # color = ~greens(value)

        mylivemap # displays the map
      }
    })

    ### example of adding a layer
    # addTiles(
    #   "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
    #   attribution = paste(
    #     "&copy; <a href=\"https://openstreetmap.org\">OpenStreetMap</a> contributors",
    #     "&copy; <a href=\"https://cartodb.com/attributions\">CartoDB</a>"
    #   )
    # )

    # # mapview package function  mapView() - Another option might be to use mapView function in mapview package, which is easier in general but a bit trickier in shiny apps it seems
    # projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #  lat / lon
    # try({mapview  ::  mapView(   (sf  ::  st_as_sf(x = (pts()), coords = c("lon", "lat"), crs = projcrs)))@map}, silent = TRUE)

    # ______________________  ####
    # . ####
    # 4. GRAPHICS   ####################
    # . ####
    ## Boxplots ####
    #
    # output$plot1out <- shiny::renderCachedPlot({
    
    output$plot1out <- shiny::renderPlot({
      req(results_table())
      out <- results_table()
      names(out) <- fixcolnames(names(out), oldtype = 'api', newtype = 'r', mapping_for_names = map_headernames)
      us.ratios    <- calc_ratios_to_avg(out)
      #state.ratios <- calc_ratios_to_avg(out = out, zone.prefix = 'state')
      ## boxplots_ratios(us.ratios$ratios_d)
      outplot <- boxplots_ratios(us.ratios$ratios_d, 'pctlowinc', '% low income', wheretext = paste0("Within ", out$radius.miles[1]," miles of"))
      outplot
      # plot(us.ratios)
    } )
    # }, cache = "session", cacheKeyExpr = {list(out)})

    # } # original server has this bracket

    ## Use Alt-O in RStudio to fold code, then expand top level to see sections.
    ## Use Ctrl-Shift-O in RStudio to view the document Outline panel

    # ______________________  ####
    # . ####

    ################################################################################################### # # 
    #   END OF WHAT WAS IN app_server_EJAMejscreenapi.R
    ################################################################################################### # # 
    
    
    
    
    
    
    
    
    ## RETURN a TABLE as OUTPUT OF moduleServer() function that is within the overall server code of the module MODULE ####
    # see example at https://shiny.posit.co/r/articles/improve/modules/

    #
    observe(print("testing info here"))
    observe({
      msg <- sprintf("Table class is %s", class(table_as_displayed_reactive()) )
      cat(msg, "\n")
      print(table_as_displayed_reactive())
      print(class(table_as_displayed_reactive()))
      print(is.data.frame(table_as_displayed_reactive()))
      print(names(table_as_displayed_reactive()))
    })

    return(table_as_displayed_reactive)

  }) # >>>>>>>>>  END OF moduleServer() function within mod_ejscreenapi_server() code
  # >>>>>>  END OF moduleServer() function within mod_ejscreenapi_server() code  ####

} # end of mod_ejscreenapi_server() END OF MODULE'S SERVER CODE (which has moduleServer() function inside it)
#>>> end of mod_ejscreenapi_server() END OF MODULE'S SERVER CODE (which has moduleServer() function inside it)  ####
# ________________________________________________________ # ###

