#' app_server - EJAM app server
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @import DT
#' @import data.table
#' @importFrom data.table ":="
#' @import foreach
#' @import ggplot2
#' @import glue
#' @import golem
#' @import leaflet
#' @import readxl
#' @import RMySQL
#' @import SearchTrees
#' @import shinycssloaders
#' @importFrom shinyjs reset
#' @importFrom shinyjs disable
#' @importFrom shinyjs enable
#' @import sf
#' @import sp
#' @import tidyverse
#' @import tidyr
#' @importFrom magrittr '%>%'
#' @importFrom grDevices dev.off png
#' @importFrom graphics abline barplot legend points
#' @importFrom methods Summary as
#' @importFrom stats aggregate density na.omit quantile runif setNames
#' @importFrom utils data download.file installed.packages object.size read.csv stack tail
#' @rawNamespace import(dplyr, except = c(first, last, between))
#'
app_server <- function(input, output, session) {
  
  # notes ####
  #############################################################################  #
  # Note: whether/how to avoid or still use a global.R file in the golem approach. and/or use options and yaml file or golem approaches to options/settings...
  # https://github.com/ThinkR-open/golem/issues/6 then https://github.com/ThinkR-open/golem/issues/920 then https://github.com/ThinkR-open/golem/discussions/932
  # but latest seems to be here: https://cran.r-project.org/web/packages/golem/vignettes/c_deploy.html
  #   Here is the methodology for integrating what you've got in globals.R inside your golem app:
  # - all the datasets that are fixed should be added as a package dataset
  # - functions should be integrated as package functions
  # - things that need to be launched at runtime should be listed in the app_server function
  # + You can also use golem_opts for things needed at runtime, to have something like run_app(param = this).
  #   get_golem_options("default_default_miles") or get_golem_options("radius")
  # to check if app was launched like run_app(default_default_miles = 3.1) or run_app(radius=3.1)
  #############################################################################  #
  # Key reactives = data_uploaded(), data_processed(), data_summarized() ####
  ##  data_uploaded   reactive holds selected latlon points or shapefiles. It is defined later.
  ##  data_processed  reactive holds results of analysis, like output of doaggregate(getblocksnearby(points))
  ##  data_summarized reactive holds results of batch.summarize()
  ##  Note that  ejamit(points)  would do all of those steps in one function, essentially.
  data_processed <-  reactiveVal(NULL) # initialized so it can be set later in reaction to an event, using data_processed(newvalue)
  data_summarized <- reactiveVal(NULL) # initialized so it can be set later in reaction to an event, using
  
  sanitized_standard_analysis_title <- reactive({
    sanitize_text(input$standard_analysis_title)
  })
  
  sanitized_analysis_title <- reactive({
    sanitize_text(input$analysis_title)
  })
  
  sanitized_an_threshgroup1 <- reactive({
    sanitize_text(input$an_threshgroup1)
  })
  
  sanitized_an_threshgroup2 <- reactive({
    sanitize_text(input$an_threshgroup2)
  })
  
  sanitized_bt_rad_buff <- reactive({
    req(input$bt_rad_buff)
    sanitize_numeric(input$bt_rad_buff)
  })
  
  
  #
  # provide nice message if disconnected, via shinydisconnect package
  observeEvent(input$disconnect, {session$close()})
  ################################################################### #
  # testing/dev mode settings ####
  observe({
    if (length(get_golem_options("shiny.testmode")) > 0  ) { # allow params in run_app() to override default
      if (!get_golem_options("shiny.testmode")) {
        updateRadioButtons(session = session, inputId = "shiny.testmode", selected = FALSE)
        if(!isTRUE(getOption("shiny.testmode"))) {
          options(shiny.testmode = FALSE)
        }
      } else {
        updateRadioButtons(session = session, inputId = "shiny.testmode", selected = TRUE)
        options(shiny.testmode = TRUE)
      }
    }
  }, priority = 1) 
  observe({
    if (input$shiny.testmode) {
      options(shiny.testmode = TRUE)
      cat('shiny.testmode == TRUE\n') 
    } else {
      if (!isTRUE(getOption("shiny.testmode"))) {
        options(shiny.testmode = FALSE)
        cat('shiny.testmode == FALSE\n')
      }
    }
  }, priority = 2)  
  
  
  ################################################################### #
  # *** As of Shiny 1.6.0, we recommend using bindEvent() instead of eventReactive() and observeEvent().
  # When bindEvent() is used with reactive() and observe(), it does the same as eventReactive() and observeEvent().
  # When bindEvent() is used with reactive(), it creates a new reactive expression object.
  # When bindEvent() is used with observe(), it alters the observer in place. It can only be used with observers which have not yet executed.
  # In many cases, it makes sense to use bindEvent() along with bindCache(), because they each can reduce the amount of work done on the server. For example, you could have sliderInputs x and y and a reactive() that performs a time-consuming operation with those values. Using bindCache() can speed things up, especially if there are multiple users. But it might make sense to also not do the computation until the user sets both x and y, and then clicks on an actionButton named go.
  # To use both caching and events, the object should first be passed to bindCache(), then bindEvent().
  # and can also be used with render functions (like renderText() and renderPlot()).
  # To use both caching and events, the object should first be passed to bindCache(), then bindEvent(). For example:
  #
  #   r <- reactive({
  #     Sys.sleep(2)  # Pretend this is an expensive computation
  #     input$x * input$y
  #   }) %>%
  #     bindCache(input$x, input$y) %>%
  #     bindEvent(input$go)
  #   Anything that consumes r() will take a reactive dependency on the event expression given to bindEvent(), and not the cache key expression given to bindCache(). In this case, it is just input$go.
  
  #. ####
  # ___ BUTTONS/TABS (events: Go to tab/ Help/ Start analysis) ####
  
  ## outline of tabs
  # at one point was this:
  #
  # tabsetPanel(                         id = 'all_tabs',     ##
  #   tabPanel(title = 'About EJAM',
  #   tabPanel(title = 'Site Selection',
  #   tabPanel(title =   , # ??
  #   tabsetPanel(                       id = 'results_tabs', ##
  #       tabPanel(title = 'Summary',
  #       tabPanel(title = 'Details',
  #          tabPanel(title = 'Site-by-Site Table',
  #          tabPanel(title = 'Plot Average Scores',
  #          tabPanel(title = 'Plot Full Range of Scores',
  #       tabPanel(title = 'Written Report',
  #   tabPanel(title = 'EJScreen Batch Tool',
  #   tabPanel(title = 'Advanced Settings',
  
  ##     -------------------------- BUTTONS to switch tabs ---------------------- #
  
  # (to navigate if clickable tab controls are "hidden") 
  
  ## "About" button/link  (might not be used)
  # observeEvent(input$link_to_about_page, {updateTabsetPanel(session, inputId = "all_tabs", selected = "About EJAM")})
  
  ## "back to site selection" buttons (to go back to site selection tab from results tab)
  observeEvent(input$back_to_site_sel,  {updateTabsetPanel(session, inputId = 'all_tabs', selected = 'Site Selection')})  # might not  be used
  observeEvent(input$back_to_site_sel2, {updateTabsetPanel(session, inputId = 'all_tabs', selected = 'Site Selection')})
  ## "return to results" button (to go from site selection to results)
  observeEvent(input$return_to_results, {updateTabsetPanel(session, inputId = "all_tabs", selected = "See Results")})  # updateTabsetPanel(session, 'results_tabs', 'Summary')
  ## "return to results" button, shown once data_processed()
  observe({
    if (isTruthy(data_processed())) {
      shinyjs::show(id = 'return_to_results')
    } else {
      shinyjs::hide(id = 'return_to_results')
    }
  })
  ##    --------------------------  TABS to show/hide    -------------------------- -
  
  ## start app without showing Results since no analysis done yet
  hideTab(inputId = 'all_tabs', target = 'See Results')
  ## note the app will show and select the results tab once results are finished processing
  
  ## hide vs show ADVANCED tab at start  ---------------------- #   ***
  
  if (!is.null(get_golem_options('advanced'))) { # option provided to run_app()
    if (get_golem_options("advanced")) {
      print("showing advanced tab")
      showTab(inputId =  "all_tabs", target = 'Advanced Settings')
    }
  } else {
    if (default_hide_advanced_settings) {
      hideTab(inputId = 'all_tabs', target = 'Advanced Settings')
    }
  }
  ## hide vs show ADVANCE tab on button click (button in 'About EJAM' tab) ***
  
  observeEvent(input$ui_show_advanced_settings,
               {showTab(inputId = 'all_tabs', target = 'Advanced Settings')})
  observeEvent(input$ui_hide_advanced_settings,
               {hideTab(inputId = 'all_tabs', target = 'Advanced Settings')})
  
  ## hide vs show ABOUT tab  ---------------------- #   ***
  if (default_hide_about_tab) {
    hideTab(inputId = 'all_tabs', target = 'About') 
  }
  ## hide vs show WRITTEN REPORT tab ---------------------- #   ***
  if (default_hide_written_report) {
    hideTab(inputId = 'results_tabs', target = 'Written Report') 
  }
 
  ## hide vs show BARPLOTS tab  ---------------------- #   ***
  if (default_hide_plot_barplots_tab) {
    hideTab(inputId = 'details_subtabs', target = 'Plot Average Scores')
  }
    
  ## hide vs show HISTOGRAMS tab  ---------------------- #   ***
  if (default_hide_plot_histo_tab) {
    hideTab(inputId = 'details_subtabs', target = 'Plot Full Range of Scores')
  }
  
  ## advanced tab provides size cap on file uploads  ---------------------- #
  
  max_mb_upload_react <- reactive({
    x <- as.numeric((input$max_mb_upload))
    if (is.null(x) || is.na(x) || length(x) == 0) {
      x <- default_max_mb_upload
      shiny::updateNumericInput(inputId = "max_mb_upload", value = x)
    } else {
      if (x > maxmax_mb_upload) {x <- maxmax_mb_upload}
      if (x < minmax_mb_upload) {x <- minmax_mb_upload}
      shiny::updateNumericInput(inputId = "max_mb_upload", value = x)
    }
    x
  })
  observe({
    # Adjusts cap on file size user can upload, and resets file inputs in case
    #   last attempt failed due to size, so you can retry with new cap.
    options(shiny.maxRequestSize = max_mb_upload_react() * 1024^2)
    ids_of_fileInput_lines <- c("ss_upload_latlon", "ss_upload_shp", "ss_upload_frs", "ss_upload_program","ss_upload_fips")
    for (idx in ids_of_fileInput_lines) {shinyjs::reset(idx)}
    # *** and also note   ns("pointsfile") and  ns("shapefile")  in  modules should be added and handled if those modules get used
  })
  
  ## buttons to see help info  ---------------------- #
  
  observeEvent(input$latlon_help, {
    showModal(modalDialog(HTML(latlon_help_msg), easyClose = TRUE))})
  observeEvent(input$frs_help, {
    showModal(modalDialog(HTML(frs_help_msg),    easyClose = TRUE))})
  observeEvent(input$epa_program_help, {
    showModal(modalDialog(HTML(epa_program_help_msg), easyClose = TRUE))})
  observeEvent(input$fips_help, {
    showModal(modalDialog(HTML(fips_help_msg),   easyClose = TRUE))})
  observeEvent(input$shp_help, {
    showModal(modalDialog(HTML(shp_help_msg),    easyClose = TRUE))})
  # . --------------------------------------------------------------- ####
  
  #. ## ##
  
  # ______ SELECT SITES ________####
  #. ####
  
  naics_counts_filtered <- reactive({
    if (input$naics_digits_shown == "detailed") {
      naics_counts # [nchar(naics_counts$NAICS) %in% 2:6, ]
    } else {
      naics_counts[nchar(naics_counts$NAICS) == 3, ]
    }
  })
  
  # update ss_select_NAICS input options ###
  observeEvent(eventExpr = {
    input$add_naics_subcategories
    input$naics_digits_shown
  },
  handlerExpr = {
    #req(input$add_naics_subcategories)
    
    ## switch labels based on subcategory radio button
    
    if (input$add_naics_subcategories) {
      naics_choices <- setNames(naics_counts_filtered()$NAICS, naics_counts_filtered()$label_w_subs)
    } else{
      naics_choices <- setNames(naics_counts_filtered()$NAICS, naics_counts_filtered()$label_no_subs)
    }
    
    vals <- input$ss_select_naics
    # update ss_select_NAICS input options ###
    updateSelectizeInput(session, inputId = 'ss_select_naics',
                         ## use named list version, grouped by first two code numbers
                         choices = naics_choices, # need to keep formatting
                         selected = vals,
                         #choices = NAICS, # named list of codes, data loaded with EJAM package
                         server = TRUE)
  })
  
  
  # update ss_select_SIC input options ###
  updateSelectizeInput(session, inputId = 'ss_select_sic',
                       choices = SIC, # named list of codes
                       server = TRUE)
  
  #############################################################################  #
  
  # SELECT Facility Type vs UPLOAD Latlon/ id/ fips/ shape (radio button) ####
  # keep track of currently used method of site selection
  current_upload_method <- reactive({
    x <- switch(
      input$ss_choose_method,
      'dropdown' = switch(input$ss_choose_method_drop,
                          NAICS =          "NAICS",     # 'NAICS (industry name or code)'
                          EPA_PROGRAM =    "EPA_PROGRAM_sel",
                          SIC =            "SIC" ,
                          MACT =           "MACT"),
      'upload'   = switch(input$ss_choose_method_upload,
                          SHP =              "SHP",
                          latlon =           "latlon",    # 'Location (lat/lon)',
                          #latlontypedin =   "latlontypedin",
                          EPA_PROGRAM =      "EPA_PROGRAM_up",
                          FRS =              "FRS",       # 'FRS (facility ID)',
                          #ECHO =            "ECHO",      # 'ECHO Search Tools',
                          FIPS =             "FIPS")
    )
    if (input$testing) {
      cat('current_upload_method reactive is ', x, '\n')
      cat('current input$ss_choose_method      is ', input$ss_choose_method,      '\n')
      if (input$ss_choose_method == "dropdown") {cat('current input$ss_choose_method_drop is ', input$ss_choose_method_drop, '\n')}
      if (input$ss_choose_method == "upload") {cat('current input$ss_choose_method_upload is ', input$ss_choose_method_upload, '\n')}
    }
    x
  })
  # reactive to keep track of data type used in last analysis
  submitted_upload_method <- reactiveVal(NULL)
  # observeEvent(input$bt_get_results, {
  #   submitted_upload_method(current_upload_method())
  # })
  
  
  
  
  observeEvent(input$show_data_preview,
               {
                 showModal( shiny::modalDialog(title = 'Selected location data', size = 'l', easyClose = TRUE,
                                               helpText('View or download data corresponding to your upload/selections.'),
                                               ## use download buttons for speed and handling larger data
                                               downloadButton('download_preview_data_csv', label = 'CSV', class = 'usa-button'),
                                               downloadButton('download_preview_data_xl', label = 'Excel', class = 'usa-button'),
                                               br(),br(),
                                               DT::DTOutput('print_test2_dt', width = '100%')))
               })
  
  #############################################################################  #
  
  ## HTML for alert for invalid sites
  #invalid_alert <- reactiveVal(NULL)
  
  invalid_alert <- reactiveValues('latlon' = 0, 'NAICS' = 0, 'SIC' = 0,
                                  'FRS' = 0, 'EPA_PROGRAM_up' = 0,
                                  'EPA_PROGRAM_sel' = 0,
                                  'MACT' = 0, 'FIPS' = 0, 'SHP' = 0)
  
  ## reactive: SHAPEFILES uploaded ####
  
  num_valid_pts_uploaded <- reactiveValues('SHP' = 0)
  
  data_up_shp <- reactive({

    req(input$ss_upload_shp)
    infiles <- input$ss_upload_shp$datapath # get path and temp (not original) filename of the uploaded file
    print(infiles)
    
    if (use_shapefile_from_any) { # newer way
      
      shp <- shapefile_from_any(infiles, cleanit = FALSE)
      
      if (!is.null(attr(shp, "disable_buttons_SHP")))        {disable_buttons[['SHP']] <- attr(shp, "disable_buttons_SHP")}
      if (!is.null(attr(shp, "num_valid_pts_uploaded_SHP"))) {num_valid_pts_uploaded[['SHP']] <- attr(shp, "num_valid_pts_uploaded_SHP")}
      if (!is.null(attr(shp, "invalid_alert_SHP")))          {invalid_alert[['SHP']] <- attr(shp, "invalid_alert_SHP")}
      if (!is.null(attr(shp, "an_map_text_shp")))            {an_map_text[['SHP']] <- attr(shp, "an_map_text_shp")}
      
    } else {
      
      # older way
      
      # detect type of file(s) specified, and read it
      infile_ext <- tools::file_ext(infiles)
      if (!all(c('shp','shx','dbf','prj') %in% infile_ext) && !('zip' %in% infile_ext) && !('json' %in% infile_ext)) {
        # cant read file type specified ____________________________________
        disable_buttons[['SHP']] <- TRUE
        shiny::validate('Not all required file extensions found.')
      }
      if (length(infile_ext) == 1 & any(grepl("json", infile_ext))) {
        # read json file ____________________________________
        shp <- shapefile_from_json(infiles)
        
      } else {
        if (length(infile_ext) == 1 & any(grepl("zip", infile_ext))) {
          # read zip file____________________________________
          shp <- shapefile_from_zip(infiles)
          
        } else {
          # read .shp etc.____________________________________
          dir <- unique(dirname(infiles)) # get folder (a temp one created by shiny for the uploaded file)
          outfiles <- file.path(dir, input$ss_upload_shp$name) # create new path\name from temp dir plus original filename of file selected by user to upload
          name <- strsplit(input$ss_upload_shp$name[1], "\\.")[[1]][1] # ??? get filename minus extension, of 1 file selected by user to upload
          purrr::walk2(infiles, outfiles, ~file.rename(.x, .y)) # rename files from ugly tempfilename to original filename of file selected by user to upload
          shp <- sf::read_sf(file.path(dir, paste0(name, ".shp"))) # read-in shapefile
        }
      }
 
      # if shp is null, present message in app
      if (is.null(shp)) {
        disable_buttons[['SHP']] <- TRUE                                # a reactiveValues object
        shiny::validate("Uploaded file should contain the following file extensions: shp,shx,dbf,prj or json or zip")
      }
      
      # if shp contains point features, present message in app
      if (any(sf::st_geometry_type(shp) == "POINT")) {
        disable_buttons[['SHP']] <- TRUE                                # a reactiveValues object
        shiny::validate("Shape file must be of polygon geometry.")
      }
      
      # Drop Z and/or M dimensions from feature geometries, resetting classes appropriately
      shp <- sf::st_zm(shp)
      
      # standardize colname to "geometry" since standard name not always seen. Can be "Shape" for example: shp <- shapefile_from_any(system.file('testdata/shapes/portland.gdb.zip', package = "EJAM"))  
      if (any(grepl("sfc", lapply(shp, class)))) {
        colnames(shp)[grepl("sfc", lapply(shp, class))] <- "geometry"
        st_geometry(shp) <- "geometry"
      }
      
      # check if shp is valid, and
      # add "valid" and "invalid_msg" columns re invalid rows/polygons
      
      if (nrow(shp) > 0) {
        ## terra provides faster valid check than sf
        shp_valid_check <- terra::is.valid(terra::vect(shp), messages = T)
        shp_is_valid <- shp_valid_check$valid
        numna <- sum(!shp_is_valid)
        num_valid_pts_uploaded[['SHP']] <- length(shp_is_valid) - sum(!shp_is_valid)    # a reactiveValues object
        invalid_alert[['SHP']] <- numna                                                 # a reactiveValues object 
        #shp_valid <- shp[sf::st_is_valid(shp),] # old way to check which shapes valid 
        shp_valid <- dplyr::mutate(shp, siteid = dplyr::row_number())
        shp_proj <- sf::st_transform(shp_valid,crs = 4269)
      } else {
        # zero polygons found
        invalid_alert[['SHP']] <- 0  # hides invalid site warning                       # a reactiveValues object
        an_map_text_shp(HTML(NULL))  # hides the count of uploaded sites/shapes         # a reactiveValues object
        disable_buttons[['SHP']] <- TRUE                                                # a reactiveValues object
        ## if not matched, return this message
        shiny::validate('No shapes found in file uploaded.')
      }
      disable_buttons[['SHP']] <- FALSE                                                 # a reactiveValues object
      shp_proj$valid <- shp_is_valid
      shp_proj <- cbind(ejam_uniq_id = 1:nrow(shp_proj), shp_proj)   #  UNIQUE ID HERE
      shp_proj$invalid_msg <- NA
      shp_proj$invalid_msg[shp_proj$valid == F] <- shp_valid_check$reason[shp_proj$valid == F]
      shp_proj$invalid_msg[is.na(shp_proj$geometry)] <- 'bad geometry'
      class(shp_proj) <- c(class(shp_proj), 'data.table')
      shp_proj
    }
    
  }) # END OF SHAPEFILE UPLOAD
  
  #############################################################################  #
  
  ## *** note: repeated file reading code below could be replaced by  latlon_from_anything() ####
  #  ext <- latlon_from_anything(input$ss_upload_latlon$datapath)
  
  ## reactive: latlon is typed in on-screen via MODULE *** ####
  # see also  EJAM/R/mod_dataentry_EXAMPLE.R
  #############################################################################  #   #############################################################################  #
  #############################################################################  #   #############################################################################  #
  
  # # ***   DISABLE UNTIL WORKING RIGHT?
  ####      IDEA IS TO LET USER TYPE FROM SCRATCH LAT LONS, AND
  ####      ALSO MAYBE EDIT LATLONS ALREADY UPLOADED FROM A FILE!
  
  # # Use a default initial template of lat lon values table ready for user to type into
  # # and then the module updates that reactive_data1 object as the user types
  # latlon_template <- data.table(lat = 0, lon = 0, sitenumber = 1, sitename = "")  # default_points_shown_at_startup[1:2, ] #  testpoints_5[1:2, ] # could  be put in global.R
  # reactive_data1 <-  reactiveVal(latlon_template)
  # ## or... try something like this:   Try to pass to module as param the last uploaded pts() ?
  # observe(
  #   # if data_up_latlon() gets updated, then also update this reactive for use in the edited module
  #   reactive_data1(data_up_latlon())
  # )
  #
  # MODULE_SERVER_latlontypedin(id = "pts_entry_table1", reactdat = reactive_data1) # pass points table that is reactive_data1(), but must pass it with NO parens
  #
  # If a module needs to use any reactive expressions, the outer function should take the reactive expression as a parameter.
  # If a module needs to access an input that isn’t part of the module, the
  #   containing app should pass the input value wrapped in a reactive expression (i.e. reactive(...)):
  #   myModule("myModule1", reactive(input$checkbox1))
  # If a module wants to return reactive expressions to the calling app, then return a list of reactive expressions from the function.
  
  # #   )
  # # }) %>%
  # #   bindEvent(input$latlontypedin_submit_button) # (updates only when the "Done entering points" button is pressed)
  
  # DISABLED UNTIL FIXED?
  
  #   data_typedin_latlon <- reactive({
  #     #   ## wait for typed in data to be submitted, then return cleaned lat lon table data.frame, as data_typedin_latlon() which eventually becomes data_uploaded()
  #       req(reactive_data1() )
  #     ext <- reactive_data1()  # NEED TO TEST THAT THIS IS ACTUALLY THE USER-EDITED OUTPUT OF THE MODULE   # ss_typedin_latlon()
  #     #   # ext <- data.frame( sitenumber = 1, lat = 0, lon = 0) # dummy data for testing
  #     ###   # another approach, not used:   # ext <- DataEditR::data_edit(latlon_template)
  # cat("COUNT OF ROWS IN TYPED IN DATA: ", NROW(ext),"\n")
  #     ## Validate the lat lon values. If column names are found in lat/long alias comparison, clean and return the table of lat lon values
  #     if (any(tolower(colnames(ext)) %in% lat_alias) & any(tolower(colnames(ext)) %in% lon_alias)) {
  #       sitepoints <- ext %>%
  #         EJAM::latlon_df_clean() #%>%   # This does latlon_infer() and latlon_as.numeric() and latlon_is.valid()
  #       cat("COUNT OF VALID LAT/LON POINTS IN TYPED IN DATA: ", NROW(sitepoints),"\n")
  #       sitepoints
  #       # returns it here, as the last thing in the reactive
  #     } else {
  #       ## if not matched, show this message instead
  #       shiny::validate('No lat lon coordinate columns found.')
  #     }
  #   })
  #############################################################################  #   #############################################################################  #
  #############################################################################  #   #############################################################################  #
  
  
  #############################################################################  #
  ## reactive: latlon is in table passed as parameter to run_app()  ####
  ## NOT YET IMPLEMENTED
  
  data_up_tablepassed_latlon <- reactive({
    
    ################################# #
    prepare_table_from_run_app <- function(sitepoints, input_max_pts_upload) {
      if (all(sitepoints == 0)) {return(NULL)}
      if (NROW(sitepoints) > input_max_pts_upload) {
        cat("ROW COUNT TOO HIGH IN FILE THAT SHOULD provide lat lon: ", NROW(sitepoints), "\n")
        return(NULL)
      }
      cat("ROW COUNT IN FILE THAT SHOULD provide lat lon: ", NROW(sitepoints), "\n")
      ## if column names are found in lat/long alias comparison, process
      if (any(tolower(colnames(sitepoints)) %in% lat_alias) & any(tolower(colnames(sitepoints)) %in% lon_alias)) {
        sitepoints[, ejam_uniq_id := .I]
        data.table::setcolorder(sitepoints, 'ejam_uniq_id')
        sitepoints <- sitepoints %>%
          latlon_df_clean() #%>%   # This does latlon_infer() and latlon_as.numeric() and latlon_is.valid()
        sitepoints$invalid_msg <- NA
        sitepoints$invalid_msg[is.na(sitepoints$lon) | is.na(sitepoints$lat)] <- 'bad lat/lon coordinates'
        cat("ROW COUNT after latlon_df_clean(): ", NROW(sitepoints), "\n")
        return(sitepoints)
      } else {
        return("No coordinate columns found.")
      }
    }
    ################################# #
    
    sitepoints <- 0 # since never set in global.R, only exists if at all via  get_golem_options()
    sitepoints <- global_or_param("sitepoints")
    if (all(sitepoints == 0)) {
      cat('no input sitepoints\n')
      return(NULL) # ?
    } else {
      cat('got input sitepoints\n')
    }
    
    sitepoints <- prepare_table_from_run_app(sitepoints, input$max_pts_upload)
    
    if (all(is.null(sitepoints))) {
      invalid_alert[['latlon']] <- 0 # hides the invalid site warning
      an_map_text_pts[['latlon']] <- NULL # hides the count of uploaded sites
      disable_buttons[['latlon']] <- TRUE
      validate(paste0('Max allowed upload of points is ', as.character(input$max_pts_upload)))
    } 
    
    if (all(sitepoints == "No coordinate columns found.")) {
      # problem with columns
      invalid_alert[['latlon']] <- 0 # hides the invalid site warning
      an_map_text_pts[['latlon']] <- NULL # hides the count of uploaded sites
      disable_buttons[['latlon']] <- TRUE
      ## if not matched, show this message instead
      shiny::validate('No coordinate columns found.')
    }
    
    # ok
    disable_buttons[['latlon']] <- FALSE
  })
  
  #############################################################################  #
  ## reactive: latlon is in file uploaded ####
  
  data_up_latlon <- reactive({
    
    ## wait for file to be uploaded
    req(input$ss_upload_latlon)
    
    
    ## if acceptable file type, read in; if not, send warning text
    input_file_path <- input$ss_upload_latlon$datapath
    # ideally would quickly check file size here before actually trying to read the entire file in case it is > cap.
    
    sitepoints <- as.data.table(read_csv_or_xl(fname= input_file_path))
    
    # DO NOT USE THE UPLOAD IF IT HAS MORE THAN MAX POINTS ALLOWED FOR UPLOAD
    #
    if (NROW(sitepoints) > input$max_pts_upload) {
      cat("ROW COUNT TOO HIGH IN FILE THAT SHOULD provide lat lon: ", NROW(sitepoints), "\n")
      invalid_alert[['latlon']] <- 0 # hides the invalid site warning
      an_map_text_pts[['latlon']] <- NULL # hides the count of uploaded sites
      disable_buttons[['latlon']] <- TRUE
      validate(paste0('Max allowed upload of points is ', as.character(input$max_pts_upload)))
    } else {
      
      cat("ROW COUNT IN FILE THAT SHOULD provide lat lon: ", NROW(sitepoints), "\n")
      ## if column names are found in lat/long alias comparison, process
      if (any(tolower(colnames(sitepoints)) %in% lat_alias) & any(tolower(colnames(sitepoints)) %in% lon_alias)) {
        
        sitepoints[, ejam_uniq_id := .I]
        data.table::setcolorder(sitepoints, 'ejam_uniq_id')
        
        sitepoints <- sitepoints %>%
          latlon_df_clean() #%>%   # This does latlon_infer() and latlon_as.numeric() and latlon_is.valid()
        
        sitepoints$invalid_msg <- NA
        
        sitepoints$invalid_msg[is.na(sitepoints$lon) | is.na(sitepoints$lat)] <- 'bad lat/lon coordinates'
        #data.table::as.data.table()
        cat("ROW COUNT after latlon_df_clean(): ", NROW(sitepoints), "\n")
        disable_buttons[['latlon']] <- FALSE
        sitepoints
      } else {
        invalid_alert[['latlon']] <- 0 # hides the invalid site warning
        an_map_text_pts[['latlon']] <- NULL # hides the count of uploaded sites
        disable_buttons[['latlon']] <- TRUE
        ## if not matched, show this message instead
        shiny::validate('No coordinate columns found.')
      }
    }
  })
  
  #############################################################################  #
  ## reactive: latlon by FRS registry IDs ####
  
  data_up_frs <- reactive({
    ## wait for file to be uploaded
    req(input$ss_upload_frs)
    ##  >this part could be replaced by  latlon_from_anything() *** and each time it is repeated below
    # ext <- latlon_from_anything(input$ss_upload_latlon$datapath)
    
    input_file_path <- input$ss_upload_frs$datapath
    ## if acceptable file type, read in; if not, send warning text
    
    read_frs <- as.data.table(read_csv_or_xl(fname= input_file_path))
    # returns a data.frame
    cat("ROW COUNT IN FILE THAT SHOULD provide FRS REGISTRY_ID: ", NROW(read_frs), "\n")
    #include frs_is_valid verification check function, must have colname REGISTRY_ID
    if (frs_is_valid(read_frs)) {
      if ("regid" %in% colnames(read_frs)) {
        colnames(read_frs) <- gsub("regid", "REGISTRY_ID", colnames(read_frs))
      }
      #converts registry id to character if not already in that class ( frs registry ids are character)
      if (('REGISTRY_ID' %in% colnames(read_frs)) & (class(read_frs$REGISTRY_ID) != "character")) {
        read_frs$REGISTRY_ID = as.character(read_frs$REGISTRY_ID)
      }
      
      sitepoints <- dplyr::left_join(read_frs, frs_from_regid(read_frs$REGISTRY_ID))
      #sitepoints <- frs_from_regid(read_frs$REGISTRY_ID)
      # read_frs_dt <- data.table::as.data.table(read_frs)
      data.table::setDT(sitepoints) # same but less memory/faster?
      
      ## add ejam_uniq_id and valid T/F columns
      sitepoints[, ejam_uniq_id := .I]
      data.table::setcolorder(sitepoints, 'ejam_uniq_id')
      #site_is_invalid(sitepoints, type = 'FRS')
      sitepoints[,valid := !(REGISTRY_ID == 'NA' | is.na(lon) | is.na(lat))]
      sitepoints$invalid_msg <- NA
      
      sitepoints$invalid_msg[sitepoints$REGISTRY_ID == 'NA'] <- 'bad REGISTRY_ID'
      sitepoints$invalid_msg[is.na(sitepoints$lon) | is.na(sitepoints$lat)] <- 'bad lat/lon coordinates'
      sitepoints
    } else {
      invalid_alert[['FRS']] <- 0 # hides the invalid site warning
      an_map_text_pts[['FRS']] <- NULL # hides the count of uploaded sites
      disable_buttons[['FRS']] <- TRUE
      shiny::validate('Records with invalid Registry IDs')
    }
    ## return merged dataset
    cat("SITE COUNT VIA FRS frs_is_valid() looking for REGISTRY_ID: ", NROW(sitepoints), "\n")
    disable_buttons[['FRS']] <- FALSE
    sitepoints
  })
  
  #############################################################################  #
  ## reactive: latlon by NAICS ####
  data_up_naics <- reactive({
    ## check if anything has been selected or entered
    req(isTruthy(input$ss_select_naics))
    
    #define inputs
    naics_user_picked_from_list <- input$ss_select_naics
    add_naics_subcategories <- input$add_naics_subcategories
    # q: IS IT BETTER TO USE THIS IN naics_from_any() OR IN frs_from_naics() BELOW ??
    
    # naics_validation function to check for non empty NAICS inputs
    if (naics_validation(naics_enter = '', naics_select = input$ss_select_naics)) {
      #if (naics_validation(input$ss_enter_naics,input$ss_select_naics)) {
      inputnaics = {}
      
      # if not empty, assume its pulled using naics_from_any() or older naics_find() above
      if (length(inputnaics) == 0 | rlang::is_empty(inputnaics)) {
        #construct regex expression and finds sites that align with user-selected naics codes
        inputnaics <- naics_user_picked_from_list
        inputnaics <- unique(inputnaics[inputnaics != ""])
        
        print(inputnaics)
        
        #merge user-selected NAICS with FRS facility location information
        # sitepoints <- frs_by_naics[NAICS %like% inputnaics ,  ]
        
        #   2. GET FACILITY LAT/LON INFO FROM NAICS CODES
        
        sitepoints <- frs_from_naics(inputnaics, children = add_naics_subcategories)[, .(lat,lon,REGISTRY_ID,PRIMARY_NAME,NAICS)] # xxx
        
        sitepoints[, ejam_uniq_id := .I]
        data.table::setcolorder(sitepoints, 'ejam_uniq_id')
        # print(sitepoints)
        if (rlang::is_empty(sitepoints) | nrow(sitepoints) == 0) {
          ################ Should output something saying no valid results returned ######## #
          invalid_alert[['NAICS']] <- 0 # hides the invalid site warning
          an_map_text_pts[['NAICS']] <- NULL # hides the count of uploaded sites
          disable_buttons[['NAICS']] <- TRUE
          shiny::validate('No valid locations found under this NAICS code.')
        }
      } else{
        sitepoints <- frs_from_naics(inputnaics, children = add_naics_subcategories)[, .(lat,lon,REGISTRY_ID,PRIMARY_NAME,NAICS)] # xxx
        
        sitepoints[, ejam_uniq_id := .I]
        data.table::setcolorder(sitepoints, 'ejam_uniq_id')
        sitepoints$invalid_msg <- NA
        sitepoints$invalid[is.na(sitepoints$NAICS)] <- 'bad NAICS Code'
        sitepoints$invalid_msg[is.na(sitepoints$lon) | is.na(sitepoints$lat)] <- 'bad lat/lon coordinates'
        # print(sitepoints)
        showNotification('Points submitted successfully!', duration = 1)
      }
    } else {
      invalid_alert[['NAICS']] <- 0 # hides the invalid site warning
      an_map_text_pts[['NAICS']] <- NULL # hides the count of uploaded sites
      disable_buttons[['NAICS']] <- TRUE
      ################ Should output something saying no valid results returned ######## #
      shiny::validate('Invalid NAICS Input')
      
    }
    cat("SITE COUNT VIA NAICS from frs_from_naics: ", NROW(sitepoints), "\n")
    ## assign final value to data_up_naics reactive variable
    sitepoints <- sitepoints %>% latlon_df_clean()
    sitepoints$invalid_msg <- NA
    sitepoints$invalid[is.na(sitepoints$NAICS)] <- 'bad NAICS Code'
    sitepoints$invalid_msg[is.na(sitepoints$lon) | is.na(sitepoints$lat)] <- 'bad lat/lon coordinates'
    cat("SITE COUNT VIA NAICS after latlon_df_clean: ", NROW(sitepoints), "\n")
    disable_buttons[['NAICS']] <- FALSE
    sitepoints
  })
  
  
  
  #############################################################################  #
  ## reactive: latlon by EPA Program IDs ####
  
  data_up_epa_program_up <- reactive({
    ## wait for file to be uploaded
    req(isTruthy(input$ss_upload_program))
    #req(input$submit_program)
    
    #if (input$ss_choose_method_upload == 'EPA_PROGRAM') {
    #if (input$program_ul_type == 'upload') {
    req(input$ss_upload_program)
    
    ## check if file extension is appropriate
    input_file_path <- input$ss_upload_program$datapath
    ## if acceptable file type, read in; if not, send warning text
    
    
    read_pgm <- as.data.table(read_csv_or_xl(fname= input_file_path))
    
    # returns a data.frame
    cat("ROW COUNT IN file that should have program, pgm_sys_id: ", NROW(read_pgm), "\n")
    ## error if no columns provided
    if (!any(c('program','pgm_sys_id') %in% tolower(colnames(read_pgm)))) {
      invalid_alert[['EPA_PROGRAM_up']] <- 0 # hides the invalid site warning
      an_map_text_pts[['EPA_PROGRAM_up']] <- NULL# hides the count of uploaded sites
      disable_buttons[['EPA_PROGRAM_up']] <- TRUE
      validate('Please add a file with at least these two columns: program, pgm_sys_id \n and possibly these columns as well: REGISTRY_ID,lat,lon')
    }
    
    ## convert pgm_sys_id and REGISTRY_ID columns to character before joining
    if (('pgm_sys_id' %in% colnames(read_pgm)) & (class(read_pgm$pgm_sys_id) != "character")) {
      read_pgm$pgm_sys_id = as.character(read_pgm$pgm_sys_id)
    }
    if (('REGISTRY_ID' %in% colnames(read_pgm)) & (class(read_pgm$REGISTRY_ID) != "character")) {
      read_pgm$REGISTRY_ID = as.character(read_pgm$REGISTRY_ID)
    }
    
    ## add check for program and pgm_sys_id else validate
    
    ## look for program in list from unique(frs_by_programid$program)
    
    if (!exists("frs_by_programid")) dataload_from_pins("frs_by_programid")
    
    ## if any of these columns already exist, join by all of them
    if (any(c('REGISTRY_ID','lat','lon') %in% colnames(read_pgm))) {
      pgm_out <- dplyr::left_join(
        read_pgm, frs_by_programid#,
        #by = c("program", "pgm_sys_id")
      )
    } else {
      pgm_out <- dplyr::left_join(
        read_pgm, frs_by_programid,
        by = c("program", "pgm_sys_id")
      )
    }
    
    pgm_out[, ejam_uniq_id := .I]
    data.table::setcolorder(pgm_out, 'ejam_uniq_id')
    
    ## clean so that any invalid latlons become NA
    pgm_out <- pgm_out %>%
      latlon_df_clean()
    
    pgm_out$invalid_msg <- NA
    pgm_out$invalid_msg[pgm_out$REGISTRY_ID == 'NA'] <- 'bad REGISTRY_ID'
    pgm_out$invalid_msg[is.na(pgm_out$lon) | is.na(pgm_out$lat)] <- 'bad lat/lon coordinates'
    #} else if (input$program_ul_type == 'dropdown') {
    #}
    ## return output dataset
    cat("SITE COUNT VIA PROGRAM ID: ", NROW(pgm_out), "\n")
    disable_buttons[['EPA_PROGRAM_up']] <- FALSE
    pgm_out
  })
  
  data_up_epa_program_sel <- reactive({
    ## wait for file to be uploaded
    #req(isTruthy(input$ss_select_program))
    #req(input$submit_program)
    
    if (!exists("frs_by_programid")) dataload_from_pins("frs_by_programid")
    
    #if (input$ss_choose_method_drop == 'EPA_PROGRAM') {
    req(isTruthy(input$ss_select_program))
    ## filter frs_by_programid to currently selected program
    pgm_out <- frs_by_programid[ program == input$ss_select_program]
    
    
    if (nrow(pgm_out) > 0) {
      
      
      pgm_out[, ejam_uniq_id := .I]
      data.table::setcolorder(pgm_out, 'ejam_uniq_id')
      
      ## clean so that any invalid latlons become NA
      pgm_out <- pgm_out %>%
        latlon_df_clean()
      
      pgm_out$invalid_msg <- NA
      pgm_out$invalid_msg[pgm_out$REGISTRY_ID == 'NA'] <- 'bad REGISTRY_ID'
      pgm_out$invalid_msg[is.na(pgm_out$lon) | is.na(pgm_out$lat)] <- 'bad lat/lon coordinates'
      
      #}
      ## return output dataset
      cat("SITE COUNT VIA PROGRAM ID: ", NROW(pgm_out), "\n")
      disable_buttons[['EPA_PROGRAM_sel']] <- FALSE
      pgm_out
    } else {
      invalid_alert[['EPA_PROGRAM_sel']] <- 0
      an_map_text_pts[['EPA_PROGRAM_sel']] <- NULL
      disable_buttons[['EPA_PROGRAM_sel']] <- TRUE
      validate('No valid locations found under this EPA program.')
    }
  })
  
  #############################################################################  #
  ## reactive: latlon by SIC ####
  data_up_sic <- reactive({
    ## check if anything has been selected or entered
    req(isTruthy(input$ss_select_sic))
    #req(shiny::isTruthy(input$ss_enter_sic) || shiny::isTruthy(input$ss_select_sic))
    
    #define inputs
    add_sic_subcategories <- FALSE #input$add_naics_subcategories
    # q: IS IT BETTER TO USE THIS IN naics_from_any() OR IN frs_from_naics() BELOW ?? ***
    
    # naics_validation function to check for non empty SIC inputs
    if (naics_validation('', input$ss_select_sic)) {
      inputsic = {}
      # if not empty, assume its pulled using naics_from_any() or older naics_find() above
      if (length(inputsic) == 0 | rlang::is_empty(inputsic)) {
        #construct regex expression and finds sites that align with user-selected SIC codes
        inputsic <- input$ss_select_sic #c(sic_wib_split, input$ss_select_sic)
        inputsic <- unique(inputsic[inputsic != ""])
        cat("selected SIC:  ")
        print(inputsic)
        #merge user-selected NAICS with FRS facility location information
        #sitepoints <- frs_by_sic[SIC %like% inputsic ,  ]
        
        #   2. GET FACILITY LAT/LON INFO FROM SIC CODES
        
        # print('testb')
        sitepoints <- frs_from_sic(inputsic, children = add_sic_subcategories)[, .(lat,lon,REGISTRY_ID,PRIMARY_NAME,SIC)] # xxx
        
        sitepoints[, `:=`(ejam_uniq_id = .I,
                          valid = !is.na(lon) & !is.na(lat))]
        data.table::setcolorder(sitepoints, 'ejam_uniq_id')
        sitepoints$invalid_msg <- NA
        sitepoints$invalid[is.na(sitepoints$SIC)] <- 'bad SIC Code'
        sitepoints$invalid_msg[is.na(sitepoints$lon) | is.na(sitepoints$lat)] <- 'bad lat/lon coordinates'
        # print(sitepoints)
        if (rlang::is_empty(sitepoints) | nrow(sitepoints) == 0) {
          ################ Should output something saying no valid results returned ######## #
          
          invalid_alert[['SIC']] <- 0 # hides the invalid site warning
          an_map_text_pts[['SIC']] <- NULL # hides the count of uploaded sites
          disable_buttons[['SIC']] <- TRUE
          shiny::validate('No valid locations found under this SIC code.')
        }
      } else {
        sitepoints <- frs_from_sic(inputsic, children = add_sic_subcategories)[, .(lat,lon,REGISTRY_ID,PRIMARY_NAME,SIC)] # xxx
        sitepoints[, `:=`(ejam_uniq_id = .I,
                          valid = !is.na(lon) & !is.na(lat))]
        data.table::setcolorder(sitepoints, 'ejam_uniq_id')
        sitepoints$invalid_msg <- NA
        sitepoints$invalid[is.na(sitepoints$SIC)] <- 'bad SIC Code'
        sitepoints$invalid_msg[is.na(sitepoints$lon) | is.na(sitepoints$lat)] <- 'bad lat/lon coordinates'
        showNotification('Points submitted successfully!', duration = 1)
      }
    } else {
      invalid_alert[['SIC']] <- 0 # hides the invalid site warning
      an_map_text_pts[['SIC']] <- NULL # hides the count of uploaded sites
      ################ Should output something saying no valid results returned ######## #
      disable_buttons[['SIC']] <- TRUE
      shiny::validate('Invalid SIC Input')
    }
    cat("SITE COUNT VIA SIC: ", NROW(sitepoints), "\n")
    ## assign final value to data_up_naics reactive variable
    disable_buttons[['SIC']] <- FALSE
    return(sitepoints)
  })
  
  ## reactive: places by FIPS ####
  
  data_up_fips <- reactive({
    req(input$ss_upload_fips)
    
    
    
    input_file_path <- input$ss_upload_fips$datapath
    ## if acceptable file type, read in; if not, send warning text
    fips_dt <- as.data.table(read_csv_or_xl(fname= input_file_path))
    cat("COUNT OF ROWS IN FIPS FILE: ", NROW(fips_dt),"\n")
    
    ################################################################################### #
    
    fips_vec <- fips_from_table(fips_table = fips_dt, addleadzeroes = TRUE, inshiny = TRUE)
    #fips_vec <- fips_out$vec
    
    if (is.null(fips_vec)) {
      disable_buttons[['FIPS']] <- TRUE
      invalid_alert[['FIPS']] <- 0  # hides the invalid site warning
      an_map_text_fips(HTML(NULL)) # hides the count of uploaded sites
      fips_alias <- c('FIPS','fips','fips_code','fipscode','Fips','statefips','countyfips', 'ST_FIPS','st_fips','ST_FIPS','st_fips', 'FIPS.ST', 'FIPS.COUNTY', 'FIPS.TRACT')
      
      validate(paste0('No FIPS column found. Please use one of the following names: ', paste0(fips_alias, collapse = ', ')))
    } else{
      disable_buttons[['FIPS']] <- FALSE
      cat("COUNT OF FIPS via fips_from_table(): ", length(fips_vec), '\n')
      # now let ejamit() do the rest for the FIPS case
      fips_vec
    }
    fips_vec <- fips_lead_zero(fips_vec)
    fips_is_valid <- fips_valid(fips_vec)
    
    if (sum(fips_is_valid) > 0) {
      
      numna <- sum(!fips_is_valid)
      num_notna <- length(fips_vec) - sum(!fips_is_valid)
      num_valid_pts_uploaded[['FIPS']] <- num_notna
      invalid_alert[['FIPS']] <- numna # this updates the value of the reactive invalid_alert()
      cat("Number of FIPS codes:  "); cat(length(fips_vec), 'total,', num_notna, 'valid,', numna, ' invalid \n')
      
      fips_vec
    } else {
      invalid_alert[['FIPS']] <- 0 # hides the invalid site warning
      an_map_text_fips(HTML(NULL)) # hides the count of uploaded sites/shapes
      disable_buttons[['FIPS']] <- TRUE
      ## if not matched, return this message
      shiny::validate('No valid FIPS codes found in this file.')
    }
    
  }) # END OF FIPS UPLOAD
  ################################################################################### #
  
  
  ## reactive: latlon by MACT subpart ####
  
  data_up_mact <- reactive({
    
    req(isTruthy(input$ss_select_mact))
    
    if (!exists("frs_by_mact")) dataload_from_pins("frs_by_mact")
    
    ## filter frs_by_mact to currently selected subpart
    mact_out <- frs_by_mact[subpart == input$ss_select_mact]
    cat("COUNT OF FACILITIES BY MACT: ", NROW(mact_out), "\n")
    ## remove any facilities with invalid latlons before returning
    #mact_out <- mact_out[!is.na(lat) & !is.na(lon),]
    cat("COUNT OF FACILITIES BY MACT with lat lon values: ", NROW(mact_out), "\n")
    if (all(is.na(mact_out$lat)) & all(is.na(mact_out$lon))) {
      invalid_alert[['MACT']] <- nrow(mact_out)
      an_map_text_pts[['MACT']] <- NULL
      disable_buttons[['MACT']] <- TRUE
      validate('No valid locations found under this MACT subpart')
    } else {
      disable_buttons[['MACT']] <- FALSE
      
      # mact_out[, `:=`(ejam_uniq_id = .I,
      #                 valid = !is.na(lon) & !is.na(lat))]
      mact_out$ejam_uniq_id <- 1:nrow(mact_out)
      mact_out$valid <- !is.na(mact_out$lon) & !is.na(mact_out$lat)
      data.table::setcolorder(mact_out, 'ejam_uniq_id')
      
      mact_out$invalid_msg <- NA
      mact_out$invalid_msg[is.na(mact_out$lon) | is.na(mact_out$lat)] <- 'bad lat/lon coordinates'
      ## return output dataset
      mact_out
    }
  })
  ##################################################### #
  
  # PLACES ARE READY TO ANALYZE ####
  
  ## data_uploaded() reactive holds the points or shapefiles ####
  
  data_uploaded <- reactive({
    
    ## if >1 upload method used, use the one currently indicated by radio button ss_choose_method
    
    if        (current_upload_method() == 'latlon'        ) {data_up_latlon()
      #} else if (current_upload_method() == 'latlontypedin' ) {data_typedin_latlon()
    } else if (current_upload_method() == 'NAICS'         ) {data_up_naics()
    } else if (current_upload_method() == 'FRS'           ) {data_up_frs()
      #} else if (current_upload_method() == 'ECHO'          ) {data_up_echo()
    } else if (current_upload_method() == 'EPA_PROGRAM_sel'   ) {data_up_epa_program_sel()
    } else if (current_upload_method() == 'EPA_PROGRAM_up'   ) {data_up_epa_program_up()
    } else if (current_upload_method() == 'SIC'           ) {data_up_sic()
    } else if (current_upload_method() == 'FIPS'          ) {data_up_fips() # blocks nearby, not lat/lon values
    } else if (current_upload_method() == 'MACT'          ) {data_up_mact()
    } else if (current_upload_method() == 'SHP'           ) {data_up_shp()  # shapefiles, not lat/lon values
    }
    
  })
  #############################################################################  #
  
  disable_buttons <- reactiveValues('FIPS' = TRUE, 'SHP' = TRUE,
                                    'latlon' = TRUE, 'FRS' = TRUE,
                                    'EPA_PROGRAM_up' = TRUE, 'EPA_PROGRAM_sel' = TRUE,
                                    'NAICS' = TRUE, 'SIC' = TRUE,
                                    'MACT' = TRUE)
  
  ## disable run, hide preview button, until an upload. input$ss_upload_latlon or whatever and current_upload_method() say there is data uploaded; then enable and show
  observe({
    
    if (current_upload_method() == 'latlon') {
      #if (!isTruthy(input$ss_upload_latlon)) {
      if (disable_buttons[['latlon']]) {
        shinyjs::disable(id = 'bt_get_results'); shinyjs::hide(id = 'show_data_preview')
        invalid_alert[[current_upload_method()]] <- 0
        an_map_text_pts[[current_upload_method()]] <- NULL
      } else {
        shinyjs::enable( id = 'bt_get_results'); shinyjs::show(id = 'show_data_preview')
      }
      
      # } else if (current_upload_method() == 'latlontypedin') {
      #   if (!isTruthy(input$ss_typedin_latlon)) {              #
      #     shinyjs::disable(id = 'bt_get_results'); shinyjs::hide(id = 'show_data_preview')
      #   } else {
      #     shinyjs::enable( id = 'bt_get_results'); shinyjs::show(id = 'show_data_preview')
      #   }
      
    } else if (current_upload_method() == 'FRS') {
      #if (!isTruthy(input$ss_upload_frs)) {
      
      if (disable_buttons[['FRS']]) {
        shinyjs::disable(id = 'bt_get_results'); shinyjs::hide(id = 'show_data_preview')
        invalid_alert[[current_upload_method()]] <- 0
        an_map_text_pts[[current_upload_method()]] <- NULL
      } else {
        shinyjs::enable( id = 'bt_get_results'); shinyjs::show(id = 'show_data_preview')
      }
      
    } else if (current_upload_method() == 'NAICS') {
      #if (!isTruthy(input$ss_select_naics)) {
      if (disable_buttons[['NAICS']]) {
        shinyjs::disable(id = 'bt_get_results'); shinyjs::hide(id = 'show_data_preview')
        invalid_alert[[current_upload_method()]] <- 0
        an_map_text_pts[[current_upload_method()]] <- NULL
      } else {
        shinyjs::enable( id = 'bt_get_results'); shinyjs::show(id = 'show_data_preview')
      }
      
    } else if (current_upload_method() == 'EPA_PROGRAM_up') {
      if (disable_buttons[['EPA_PROGRAM_up']]) {
        #if ((input$ss_choose_method == 'upload' & !isTruthy(input$ss_upload_program)) |
        #    (input$ss_choose_method == 'dropdown' & !isTruthy(input$ss_select_program))) {
        shinyjs::disable(id = 'bt_get_results'); shinyjs::hide(id = 'show_data_preview')
        invalid_alert[[current_upload_method()]] <- 0
        an_map_text_pts[[current_upload_method()]] <- NULL
      } else {
        shinyjs::enable( id = 'bt_get_results'); shinyjs::show(id = 'show_data_preview')
      }
      
    } else if (current_upload_method() == 'EPA_PROGRAM_sel') {
      if (disable_buttons[['EPA_PROGRAM_sel']]) {
        #if ((input$ss_choose_method == 'upload' & !isTruthy(input$ss_upload_program)) |
        #    (input$ss_choose_method == 'dropdown' & !isTruthy(input$ss_select_program))) {
        shinyjs::disable(id = 'bt_get_results'); shinyjs::hide(id = 'show_data_preview')
        invalid_alert[[current_upload_method()]] <- 0
        an_map_text_pts[[current_upload_method()]] <- NULL
      } else {
        shinyjs::enable( id = 'bt_get_results'); shinyjs::show(id = 'show_data_preview')
      }
    } else if (current_upload_method() == 'SIC') {
      if (disable_buttons[['SIC']]) {
        #if (!isTruthy(input$ss_select_sic)) {
        shinyjs::disable(id = 'bt_get_results'); shinyjs::hide(id = 'show_data_preview')
        invalid_alert[[current_upload_method()]] <- 0
        an_map_text_pts[[current_upload_method()]] <- NULL
      } else {
        shinyjs::enable( id = 'bt_get_results'); shinyjs::show(id = 'show_data_preview')
      }
      
    } else if (current_upload_method() == 'FIPS') {
      #if (!isTruthy(input$ss_upload_fips)) {
      if (disable_buttons[['FIPS']]) {
        #if (isTruthy(data_up_fips())) {
        shinyjs::disable(id = 'bt_get_results'); shinyjs::hide(id = 'show_data_preview')
        invalid_alert[[current_upload_method()]] <- 0
        an_map_text_fips(HTML(NULL))
      } else {
        shinyjs::enable( id = 'bt_get_results'); shinyjs::show(id = 'show_data_preview')
      }
      
    } else if (current_upload_method() == 'MACT') {
      if (disable_buttons[['MACT']]) {
        #if (!isTruthy(input$ss_select_mact)) {
        shinyjs::disable(id = 'bt_get_results'); shinyjs::hide(id = 'show_data_preview')
        invalid_alert[[current_upload_method()]] <- 0
        an_map_text_pts[[current_upload_method()]] <- NULL
      } else {
        shinyjs::enable( id = 'bt_get_results'); shinyjs::show(id = 'show_data_preview')
      }
      
    } else if (current_upload_method() == 'SHP') {
      if (disable_buttons[['SHP']]) {
        #if (!isTruthy(input$ss_upload_shp)) {
        shinyjs::disable(id = 'bt_get_results'); shinyjs::hide(id = 'show_data_preview')
        invalid_alert[[current_upload_method()]] <- 0
        an_map_text_shp <- NULL
      } else {
        shinyjs::enable(id = 'bt_get_results'); shinyjs::show(id = 'show_data_preview')
      }
    }
    
    ## add warning and disable button if radius is set to 0 for points
    if (!(current_upload_method() %in% c('FIPS','SHP'))) {
      if (current_slider_val[[current_upload_method()]] == 0) {
        shinyjs::disable(id = 'bt_get_results')
        showNotification(id = 'radius_warning', session = session,
                         duration = NULL, type = 'error', closeButton = F,
                         'Please use a numeric radius greater than 0 for analyzing points.')
        
      } else if (current_slider_val[[current_upload_method()]] > 0 &
                 disable_buttons[[current_upload_method()]] == FALSE) {
        shinyjs::enable(id = 'bt_get_results')
        removeNotification(id = 'radius_warning', session = session)
      } else {
        removeNotification(id = 'radius_warning', session = session)
      }
    } else {#else if(disable_buttons[[current_upload_method()]] == FALSE) {
      
      removeNotification(id = 'radius_warning', session = session)
    }
    if (input$testing) {
      cat("Enabled/disabled button to get results, and showed/hid data preview based on current_upload_method() ==  ", current_upload_method(), "\n\n")
    }
  })
  
  #############################################################################  #
  #. ####
  # ______ VIEW SITES (uploaded; not yet processed) ####
  #. ####
  
  output$invalid_sites_alert2 <- renderUI({
    req(invalid_alert[[current_upload_method()]])
    if (invalid_alert[[current_upload_method()]] > 0) {
      if ( input$ss_choose_method == 'dropdown') {
        HTML(paste0(
          '<section
  class="usa-site-alert usa-site-alert--emergency usa-site-alert--slim"
  aria-label="Site alert,,,,,,"
>
  <div class="usa-alert">
    <div class="usa-alert__body">
      <p class="usa-alert__text">
        <strong>', ' Warning! ','</strong>', 'There are ', prettyNum(invalid_alert[[current_upload_method()]],big.mark = ","), ' selected sites without associated lat/lon information.',
          '</p>
    </div>
  </div>
</section>'))
      } else if ( input$ss_choose_method == 'upload') {
        
        
        HTML(paste0(
          '<section
  class="usa-site-alert usa-site-alert--emergency usa-site-alert--slim"
  aria-label="Site alert,,,,,,"
>
  <div class="usa-alert">
    <div class="usa-alert__body">
      <p class="usa-alert__text">
        <strong>', 'Warning! ','</strong>', 'There are ', invalid_alert[[current_upload_method()]], ' invalid location(s) in your dataset.',
          '</p>
    </div>
  </div>
</section>'))
      }
    } else {
      HTML(NULL)
    }
    
  })
  
  ## initialize reactive for count of uploaded shapefiles
  an_map_text_shp <- reactiveVal(HTML(NULL))
  
  ## update count of uploaded shapefiles
  observe({
    req(data_uploaded())
    if (current_upload_method() == "SHP") {
      
      shp <- data_uploaded()#[['shape']]  ### WHAT DOES THIS DO? IT SEEMS TO NOT DO ANYTHING... ***
      num_na <- 0 # we do not keep track of invalid shapefile polygons uploaded
      num_notna <- NROW(data_uploaded()) #[['shape']])
      
      
      msg <- HTML(paste0(
        "<span style='border: 1px solid #005ea2; padding: 10px;'>Total shape(s) uploaded: <strong>",
        prettyNum(num_na + num_notna, big.mark = ","),"</strong></span>"
      ))
      an_map_text_shp(msg)
      
    }
  })
  
  ## initialize reactive for count of uploaded FIPS
  an_map_text_fips <- reactiveVal(HTML(NULL))
  
  observe({
    req(data_uploaded())
    
    if (current_upload_method() == "FIPS") {
      
      num_na <- 0 # we do not keep track of invalid FIPS uploaded
      num_locs <- NROW(data_uploaded())
      
      msg <- HTML(paste0(
        "<span style='border: 1px solid #005ea2; padding: 10px;'>Total location(s) uploaded by FIPS: <strong>",
        prettyNum(num_locs, big.mark = ","),"</strong></span>"
      ))
      an_map_text_fips(msg)
    }
  })
  
  ## initialize reactive for count of uploaded places
  #an_map_text_pts <- reactiveVal(NULL)
  
  an_map_text_pts <-  reactiveValues('latlon' = NULL,
                                     'NAICS' = NULL,
                                     'SIC' = NULL,
                                     'FRS' = NULL,
                                     'EPA_PROGRAM_up' = NULL,
                                     'EPA_PROGRAM_sel' = NULL,
                                     'MACT' = NULL)
  
  observe({
    req(data_uploaded())
    if (!current_upload_method() %in% c('FIPS','SHP')) {
      
      lat_or_lon.na <- (is.na(data_uploaded()$lat) | is.na(data_uploaded()$lon))
      if (nrow(data_uploaded()) > 1) {
        num_na <- NROW(data_uploaded()[lat_or_lon.na,  ]) # if uploaded multiple rows (points)
      } else {
        num_na <- NROW(data_uploaded()[lat_or_lon.na])   # if uploaded only one row (point) (or does that already have invalid ones removed?)
      }
      totalcount <- NROW(data_uploaded())
      num_notna <- totalcount - num_na
      
      ## if invalid data found, set invalid_alert() otherwise closeAlert()
      cat("Number of points:  "); cat(totalcount, 'total,', num_notna, 'valid,', num_na, ' invalid \n')
      if (num_na > 0) {
        #invalid_alert(num_na)
        invalid_alert[[current_upload_method()]] <- num_na
        
        #if all counts are invalid, disable analysis button
        if(num_na == totalcount){
          disable_buttons[[current_upload_method()]] <- 'TRUE'
          showNotification('Warning: no valid site uploads exist. Please select a file with at least one valid upload', type = 'message', closeButton = TRUE)
        }
      } else {
        #invalid_alert(NULL)
        invalid_alert[[current_upload_method()]] <- 0
      }
      
      msg <- HTML(paste0(
        "<span style='border: 1px solid #005ea2; padding: 10px;'>Total location(s) uploaded: <strong>", prettyNum(num_na + num_notna, big.mark = ","),"</strong></span>"
        #"<br>","Site(s) with invalid lat/lon values: <strong>", prettyNum(num_na, big.mark = ","), "</strong>","</span>"
      ))
      an_map_text_pts[[current_upload_method()]] <- msg
    }
  })
  
  ## Which points are valid?(and  how many; warn if 0) ####
  output$an_map_text <- renderUI({
    
    req(data_uploaded())
    if (current_upload_method() == 'SHP') {
      
      an_map_text_shp()
    } else if (current_upload_method() == 'FIPS' ) {
      an_map_text_fips()
    } else if (current_upload_method() %in% c('MACT','latlon','FRS','NAICS','SIC',
                                              'EPA_PROGRAM_up','EPA_PROGRAM_sel')) {
      an_map_text_pts[[current_upload_method()]]
    } else {
      HTML(NULL)
    }
    
  })
  
  ## Which points are clustered? (may double-count people) ####
  
  # note this had been done in addlinks_clusters_and_sort_cols()
  
  # is_clustered <- shiny::reactive({
  #   req(data_uploaded())
  #
  #   # which sites have residents that might also be near others sites?
  #   # circles overlap if 2 facilities are twice the radius apart  # in miles
  #       distance_near_eachother(
  #     lon = data_uploaded()$lon,
  #     lat = data_uploaded()$lat,
  #     distance = 2 * sanitized_bt_rad_buff()
  #     ## if switching units between miles and km - not currently used
  #     # distance = ifelse(input$radius_units == 'miles',
  #     #                   2 * sanitized_bt_rad_buff(),
  #     #                   2 * sanitized_bt_rad_buff() * 0.62137119
  #     #)
  #   )
  # })
  ######################################  #
  
  # *TABLE of uploaded points ####
  
  ## reactive for data shown in preview data table
  ## can be adjusted from data_uploaded(), like for FIPS
  data_preview <- reactive({
    req(data_uploaded())
    
    if (current_upload_method() == "SHP") {
      dt <- data_uploaded() %>%
        sf::st_drop_geometry()
    } else if (current_upload_method() == 'FIPS') {
      dt <- data.frame(FIPS = data_uploaded()) %>%
        dplyr::mutate(type = fipstype(FIPS), name = fips2name(FIPS))
    } else {
      dt <- data_uploaded()
    }
    dt
  })
  
  output$print_test2_dt <- DT::renderDT(
    ## server = FALSE forces download to include all rows, not just visible ones
    server = TRUE, {
      
      DT::datatable(data_preview(),
                    
                    ## to add download buttons
                    #extensions = 'Buttons',
                    
                    ##                                   keep rownames in display table
                    rownames = TRUE,                     ### ??? really ?
                    
                    options = list(pageLength = 100,
                                   scrollX = TRUE,
                                   scrollY = '500px',
                                   
                                   columnDefs = list(
                                     ##                label rownames to remove from download  ??? really ?
                                     list(
                                       targets = 0, className = "rownames"
                                     )
                                   )#,
                                   
                                   ## to specify button placement - "B" = buttons, see https://datatables.net/reference/option/dom
                                   #dom = 'Brtip',
                                   # buttons = list(
                                   #   ## customize CSV button
                                   #   list(extend = 'csv',
                                   #        ## name of downloaded file
                                   #        filename = 'ejam_raw_data_download',
                                   #        ## drop rownames for download
                                   #        exportOptions = list(columns = ":not(.rownames)")
                                   #   ),
                                   #   ## customize Excel button
                                   #   list(extend = 'excel',
                                   #        ## name of downloaded file
                                   #        filename = 'ejam_raw_data_download',
                                   #        ## drop title row from download
                                   #        title = NULL,
                                   #        ## drop rownames for download
                                   #        exportOptions = list(columns = ":not(.rownames)")
                                   #    )
                                   #  )
                                   
                    ), # end options
                    
                    escape = FALSE) # escape = FALSE may add security issue but makes links clickable in table
    })
  ######################################  #
  
  ## use external download buttons for preview data
  ## this allows loading the table on the server-side which improves speed and avoids
  ## crashes with larger datasets
  output$download_preview_data_xl <- downloadHandler(filename = 'epa_raw_data_download.xlsx',
                                                     content = function(file) {
                                                       
                                                       writexl::write_xlsx(data_preview(), file)})
  output$download_preview_data_csv <- downloadHandler(filename = 'epa_raw_data_download.csv',
                                                      content = function(file) {
                                                        ## shapefile is not of type data.table
                                                        if (current_upload_method() == 'SHP') {
                                                          readr::write_csv(data_preview(), file, append = F)
                                                          
                                                        } else {
                                                          data.table::fwrite(data_preview(), file, append = F)
                                                          
                                                        }
                                                      })
  
  #############################################################################  #
  
  # RADIUS SLIDER updates/rules ####
  
  output$radius_slider_ui <- renderUI({
    valid_default_miles <- is.numeric(input$default_miles) && input$default_miles >= 0.5
    valid_max_miles <- is.numeric(input$max_miles) && input$max_miles > 0
    
    if (valid_default_miles && valid_max_miles) {
      shiny::sliderInput(
        inputId = 'bt_rad_buff',
        label = "",
        min = current_slider_min[[current_upload_method()]],
        max = input$max_miles,
        value = input$default_miles,
        step = stepradius,
        post = ' miles'
      )
    } else {
      error_message <- if (!valid_default_miles && !valid_max_miles) {
        "The radius input is disabled because both the default and max miles values are invalid. Please set valid distances in the Advanced Settings tab."
      } else if (!valid_default_miles) {
        "The radius input is disabled because the default miles value is invalid. Please set a valid distance in the Advanced Settings tab."
      } else {
        "The radius input is disabled because the max miles value is invalid. Please set a valid maximum distance in the Advanced Settings tab."
      }
      
      tags$p(error_message, style = "color: red; font-weight: bold;")
    }
  })
  
  
  
  ## disable radius slider when FIPS is selected
  observe({
    if (current_upload_method() == 'FIPS') {
      shinyjs::disable(id = 'bt_rad_buff')
    } else {
      shinyjs::enable(id = 'bt_rad_buff')
    }
  })
  
  ## create different initial (and minimum?) radius values for each site selection type
  ### input$default_miles is set in advanced tab by global.R and then based on user input if any
  ### or via e.g., radius=3.1 or radius_shapefile=1 param that can be provided to run_app()
  
  current_slider_min <- list(
    # constants defined in global.R
    'latlon' =  minradius, 'NAICS' =  minradius, 'SIC' =  minradius, 
    'FRS' =  minradius, 'MACT' =  minradius,
    'EPA_PROGRAM_up' =  minradius, 'EPA_PROGRAM_sel' =  minradius, 
    
    'FIPS' = minradius_shapefile, 'SHP' = minradius_shapefile # but disabled for FIPS
  )
  current_slider_val <- reactiveValues(
    # these are just placeholders that should get updated at startup, though.
    'latlon' = 1, 'NAICS' = 1, 'SIC' = 1, 
    'FRS' = 1, 'MACT' = 1,
    'EPA_PROGRAM_up' = 1, 'EPA_PROGRAM_sel' = 1, 
    
    'FIPS' = minradius_shapefile, 'SHP' = minradius_shapefile  # but disabled for FIPS  
  )
  
  ## record radius at time of analysis
  submitted_radius_val <- reactiveVal(NULL)
  
  # set/update based on advanced tab set by global.R and then might be changed by a user
  observeEvent(
    input$default_miles,
    {
      # Sanitize the input: Convert to numeric or set a default value
      sanitized_miles <- as.numeric(input$default_miles)
      
      # Handle cases where the input cannot be converted to a numeric value
      if (is.na(sanitized_miles)) {
        sanitized_miles <- 0
      }
      
      these <- c(
        'latlon', 'NAICS', 'SIC', 
        'FRS', 'MACT',
        'EPA_PROGRAM_up', 'EPA_PROGRAM_sel'
        # note   FIPS and SHP types handled separately
      )
      for (this in these) {
        current_slider_val[[this]] <- sanitized_miles
      }   
    }
  )
  # set/update based on advanced tab set by global.R and then might be changed by a user
  observeEvent(
    input$default_miles_shapefile,
    {
      these <- c("FIPS", "SHP") # but disabled for FIPS 
      for (this in these) {current_slider_val[[this]] <- input$default_miles_shapefile}
    }
  )
  
  ## update/restore previous radius (and reset the min value) when site selection type changes/changes back
  observeEvent(
    eventExpr = {current_upload_method()}, 
    {
      updateSliderInput(session, inputId = 'bt_rad_buff',
                        value = current_slider_val[[current_upload_method()]])
      # updateSliderInput(session, inputId = 'bt_rad_buff',
      #                   min   = current_slider_min[[current_upload_method()]])  # now done in renderUI()
    }
  )
  ## update stored radius when slider changes
  # except this would initially save the one current radius (default) as the current value for all methods including shapefiles which we dont want to do
  observeEvent(
    sanitized_bt_rad_buff(),
    {
      current_slider_val[[current_upload_method()]] <- sanitized_bt_rad_buff()
    }
  )
  
  ## Create separate radius label to allow line break
  output$radius_label <- renderUI({
    val <- sanitized_bt_rad_buff()
    lab <- paste0('<b>Distance from Site: <br/>', val, ' miles ','(',round(val / 0.62137119, 2), ' km)</b>')
    HTML(lab)
  })
  ###################################################################################### #
  
  # *MAP of uploaded/selected places ####
  
  orig_leaf_map <- reactive({
    
    # ***
    ## or...
    # mapfast(data_uploaded(), radius = sanitized_bt_rad_buff(), column_names = "ej")
    ## or...
    # map_facilities(mypoints = data_uploaded(), #as.data.frame(data_uploaded()),
    #                rad = sanitized_bt_rad_buff(),
    #                highlight = input$an_map_clusters,
    #                clustered = is_clustered())
    
    if (current_upload_method() == "SHP") {
      ## ---------------------------------------------- __MAP SHAPES uploaded ####
      
      canmap <- TRUE
      max_pts <- input$max_shapes_map
      
      if (num_valid_pts_uploaded[['SHP']] > max_pts) {
        #if (nrow(data_uploaded()) > max_pts) {
        warning(paste0('Too many uploaded polygons (> ', prettyNum(max_pts, big.mark = ','),') for map to show'))
        validate(paste0('Too many uploaded polygons (> ', prettyNum(max_pts, big.mark = ','),') for map to show'))
        
        # add code here to show just a subset up to max allowed ***
        
        canmap <- FALSE
      } else {
        req(data_uploaded())
        
        bbox <- sf::st_bbox(data_uploaded())
        leaflet() %>%
          addTiles() %>%
          fitBounds(
            lng1 = as.numeric(bbox[1]), lng2 = as.numeric(bbox[3]),
            lat1 = as.numeric(bbox[2]), lat2 = as.numeric(bbox[4])
          )
        #try(map_shapes_leaflet(shapes = data_uploaded() %>% st_zm())) # popups?
      }
      
    } else if (current_upload_method() == 'FIPS') {
      ## ---------------------------------------------- __MAP FIPS CENSUS UNITS uploaded ####
      req(data_uploaded())
      
      canmap <- TRUE
      max_pts <- input$max_shapes_map
      if (NROW(data_uploaded()) > max_pts) {
        warning(paste0('Too many FIPS polygons (> ', prettyNum(max_pts, big.mark = ','),') for map to show'))
        validate(paste0('Too many FIPS polygons (> ', prettyNum(max_pts, big.mark = ','),') for map to show'))
        
        # add code here to show just a subset up to max allowed ***
        
        canmap <- FALSE
      } else {
        
        ### **Download FIPS Boundaries via API ---------------------------- ####
        
        FTYPES <- fipstype(data_uploaded())
        if (all(FTYPES %in% "blockgroup")) {
          shps <- try(shapes_blockgroups_from_bgfips(data_uploaded()))
          if (inherits(shps, "try-error")) {
            warning("could not obtain boundaries to map blockgroups")
            validate("could not obtain boundaries to map blockgroups")
            canmap <- FALSE
          }
        } else {
          if (all(FTYPES %in% "county")) {
            shps <- try(shapes_counties_from_countyfips(data_uploaded()))
            if (inherits(shps, "try-error")) {
              warning("could not obtain boundaries to map counties")
              validate("could not obtain boundaries to map counties")
              canmap <- FALSE
            }
          } else {
            warning('cannot map FIPS types other than counties or blockgroups currently')
            validate("cannot map FIPS types other than counties or blockgroups currently")
            canmap <- FALSE
          }
        }
        if (canmap) {
          
          try(map_shapes_leaflet(shps))  # popups?
        }
      }
      
    } else {
      ## ---------------------------------------------- __MAP LAT LON points uploaded ####
      d_upload <- data_uploaded()
      max_pts <- input$max_pts_map # was the fixed max_pts_map
      
      if (nrow(data_uploaded()) > max_pts) {
        ## Max allowed points was exceeded! see code in ejscreenapi that handled that case using  input$max_pts_map
        validate(paste0('Too many points (> ', prettyNum(max_pts, big.mark = ','),') uploaded for map to be displayed'))
        
        # add code here to show just a subset up to max allowed ***
        
      } else {
        
        ## If more than one valid point...
        if (sum((
          !is.na(data_uploaded()$lat) &
          !is.na(data_uploaded()$lon)
        )) > 1) {
          leaflet() %>%
            addTiles() %>%
            fitBounds(lng1 = min(data_uploaded()$lon, na.rm = T),
                      lng2 = max(data_uploaded()$lon, na.rm = T),
                      lat1 = min(data_uploaded()$lat, na.rm = T),
                      lat2 = max(data_uploaded()$lat, na.rm = T))
          ## if only one valid point
        } else {
          leaflet() %>%
            addTiles() %>%
            setView(lat = data_uploaded()$lat, lng = data_uploaded()$lon, zoom = 10)
        }
      }
    }
    
  })
  ######################################  #######################################  #
  
  ## output: draw map of uploaded places
  
  output$an_leaf_map <- leaflet::renderLeaflet({
    
    ## check if data has been uploaded yet
    ## make errors silent by default; print below
    m <- try(data_uploaded(), silent = T)
    
    ## if not, show empty map
    if (inherits(m, 'try-error')) {
      ## only print non-empty error messages
      error_msg <- attr(m,'condition')$message
      if (error_msg != "") {
        print(paste0("Error: ", error_msg))
      }
      leaflet() %>% addTiles() %>% setView(lat = 39.8283, lng = -98.5795, zoom = 4)
    } else {
      tryCatch({  ## if so, try to load map
        orig_leaf_map()},
        error = function(e) {  ## else, show validate error
          validate(conditionMessage(e))
        }
      )
    }
  })
  
  #############################################################################  #
  # . --------------------------------------------------------------- # ###
  
  #. ####
  # ______ RUN ANALYSIS  (when button is pressed) ________####
  #. ####
  
  ## data_processed()  reactive holds results of doaggregate()
  ## data_summarized() reactive holds results of batch.summarize()
  
  # THIS IS VERY VERY SIMILAR TO THE CODE IN ejamit() and perhaps could just rely on one set of code for both. ***xxx
  # >this part could be replaced by ejamit() or something like that ####
  
  observeEvent(input$bt_get_results, {  # (button is pressed)
    submitted_upload_method(current_upload_method())
    submitted_radius_val(current_slider_val[[submitted_upload_method()]])
    
    showNotification('Processing sites now!', type = 'message', duration = 1)
    
    ## progress bar setup overall for 3 operations  (getblocksnearby, doaggregate, batch.summarize)
    progress_all <- shiny::Progress$new(min = 0, max = 1)
    progress_all$set(value = 0, message = 'Step 1 of 3', detail = 'Getting nearby census blocks')
    
    #############################################################################  #
    # 1) **EJAM::getblocksnearby()** ####
    
    ################################################# #
    
    ## get blocks in FIPS   ####
    
    if (submitted_upload_method() == 'FIPS') {  # if FIPS, do everything in 1 step right here.
      
      out <- ejamit(fips = data_uploaded(),              # unlike for SHP or latlon cases, this could include invalid FIPS!
                    radius = 999, # because FIPS analysis
                    maxradius = input$maxradius,
                    avoidorphans = input$avoidorphans,
                    quadtree = localtree,
                    # countcols = NULL,
                    # popmeancols = NULL,
                    # calculatedcols = NULL,
                    subgroups_type = input$subgroups_type,
                    include_ejindexes   = (input$include_ejindexes == "TRUE" || input$include_ejindexes == TRUE), # it was character not logical because of how input UI done?
                    calculate_ratios = input$calculate_ratios,
                    extra_demog = input$extra_demog,
                    need_proximityscore = FALSE, #input$need_proximityscore, # not relevant for FIPS
                    # infer_sitepoints = FALSE,
                    # need_blockwt = TRUE,
                    # updateProgress = ??? , # not sure this is needed or works here
                    in_shiny = TRUE, # not sure this is needed or works here
                    # quiet = TRUE,
                    # parallel = FALSE,
                    silentinteractive = TRUE,
                    # called_by_ejamit = TRUE, # not sure this is needed or works here
                    testing = input$testing,
                    
                    thresholds   = list(input$an_thresh_comp1, input$an_thresh_comp2), # thresholds = list(90, 90), # or 80,80
                    threshnames  = list(input$an_threshnames1, input$an_threshnames2), # list(c(names_ej_pctile, names_ej_state_pctile), c(names_ej_supp_pctile, names_ej_supp_state_pctile)),
                    threshgroups = list(sanitized_an_threshgroup1(), sanitized_an_threshgroup2()) # list("EJ-US-or-ST", "Supp-US-or-ST")
      )
      
      ## note which FIPS dropped by ejamit() (i.e., in getting step or doagg step) as invalid,
      ## and add those back into results as mostly empty rows (which non-FIPS cases do later)
      if (is.null(out)) {
        validate('No valid blockgroups found matching these FIPS codes.')
      } else {
        # out$results_bysite <- merge(d_upload[, .(ejam_uniq_id, valid, invalid_msg)],
        #                             out$results_bysite,
        #                             by = 'ejam_uniq_id', all = T)
        #incorporate new longnames into FIPS data
        newcolnames <- c(
          "valid",
          "invalid_msg"
        )
        # put those up front as first columns
        data.table::setcolorder(out$results_bysite, neworder = c('ejam_uniq_id', newcolnames))
        data.table::setcolorder(out$results_overall, neworder = c('ejam_uniq_id'))
        #setcolorder(out$results_bysite, neworder = newcolnames)
        # move ejam_uniq_id to front of longnames vector
        out$longnames <- c('ejam_uniq_id', newcolnames, out$longnames[out$longnames != 'ejam_uniq_id'])
      }
      
      ################################################# #
    } else { #  everything other than FIPS code analysis
      #############################################################################  #
      
      ## get blocks in POLYGONS / SHAPEFILES ####
      
      if (submitted_upload_method() == "SHP") {
        
        shp_valid <- data_uploaded()[data_uploaded()$valid == T, ] # *** remove this if shapefile_clean() will do it?
        rad_buff <- sanitized_bt_rad_buff()

        if (!is.na(rad_buff) && rad_buff > 0) {
          #if (!silentinteractive) {
          cat('Adding buffer around each polygon.\n')
          #}
          shp <- shape_buffered_from_shapefile(
            shapefile = shp_valid,
            radius.miles =  rad_buff
          ) # default crs
        } else {
          shp <- shp_valid
        }
        ## progress bar to show getblocksnearby status
        progress_getblocks_shp <- shiny::Progress$new(min = 0, max = 1)
        progress_getblocks_shp$set(value = 0, message = '0% done')
        updateProgress_getblocks_shp <- function(value = NULL, message_detail=NULL, message_main = '0% done') {
          if (is.null(value)) { # - If value is NULL, it will move the progress bar 1/20 of the remaining distance.
            value <- progress_getblocks_shp$getValue()
            value <- value + (progress_getblocks_shp$getMax() - value) / 10
            message_main = paste0(value*100, '% done')
          }
          progress_getblocks_shp$set(value = value, message = message_main, detail = message_detail)
        }
        
        sites2blocks <- get_blockpoints_in_shape(
          shp,                                              ## already removed invalid ones
          updateProgress = updateProgress_getblocks_shp
        )$pts
        d_upload <- sites2blocks
        
        ## note which places dropped by get_blockpoints_in_shape() as invalid? none?
        
        ## close getblocks progress bar
        progress_getblocks_shp$close()
      }
      ################################################# #
      
      ## get blocks near LAT/LON  POINTS  facilities/latlon # ####
      
      if (!(submitted_upload_method() %in% c('SHP', 'FIPS'))) {  # if LATITUDE AND LONGITUDE (POINTS), find blocks nearby
        
        d_upload <- data_uploaded()[!is.na(lat) & !is.na(lon),]
        
        ## progress bar to show getblocksnearby status
        progress_getblocks <- shiny::Progress$new(min = 0, max = 1)
        progress_getblocks$set(value = 0, message = '0% done')
        updateProgress_getblocks <- function(value = NULL, message_detail = NULL, message_main = '0% done') {
          if (is.null(value)) { # - If value is NULL, it will move the progress bar 1/20 of the remaining distance.
            value <- progress_getblocks$getValue()
            value <- value + (progress_getblocks$getMax() - value) / 20
            message_main = paste0(value*100, '% done')
          }
          progress_getblocks$set(value = value, message = message_main, detail = message_detail)
        }
        
        sites2blocks <- getblocksnearby(
          
          sitepoints = d_upload,                         ## already removed invalid latlons from d_upload
          radius = sanitized_bt_rad_buff(),
          quadtree = localtree,
          avoidorphans = input$avoidorphans,
          maxradius = input$maxradius,
          quiet = TRUE,
          updateProgress = updateProgress_getblocks
        )
        
        ## note which places dropped by getblocksnearby() as invalid
        dup <- data_uploaded() # includes invalid ones too
        dup$valid <- dup$ejam_uniq_id %in% sites2blocks$ejam_uniq_id
        dup$invalid_msg[!(dup$ejam_uniq_id %in% sites2blocks$ejam_uniq_id)] <- 'no blocks found nearby'
        data_uploaded <- dup
        
        ## close doaggregate progress bar
        progress_getblocks$close()
        
      } # end LAT LON finding blocks nearby, now ready for latlon and shapefiles to do aggregation
      #############################################################################  #
      
      ## progress bar update overall
      progress_all$inc(1/3, message = 'Step 2 of 3', detail = 'Aggregating')
      ## progress bar to show doaggregate status
      progress_doagg <- shiny::Progress$new(min = 0, max = 1)
      progress_doagg$set(value = 0, message = 'Initiating aggregation')
      ## function for updating progress bar, to pass in to doaggregate function
      updateProgress_doagg <- function(value = NULL, message_detail = NULL, message_main = NULL) {
        # Create a callback function - When called, it sets progress bar to value.
        if (is.null(value)) { # - If value is NULL, it will move the progress bar 1/5 of the remaining distance.
          value <- progress_doagg$getValue()
          value <- value + (progress_doagg$getMax() - value) / 5
        }
        progress_doagg$set(value = value, message = message_main, detail = message_detail)
      }
      #############################################################################  #
      
      if (submitted_upload_method() != "FIPS") {  # if LAT LON or SHAPEFILE, now have blocks nearby and ready to aggregate
        
        #############################################################################  #
        # 2) **EJAM::doaggregate()** ####
        
        out <- suppressWarnings(
          
          doaggregate(
            sites2blocks = sites2blocks,
            sites2states_or_latlon = d_upload,                        # already removed invalids
            radius = sanitized_bt_rad_buff(),
            #countcols = 0, popmeancols = 0, calculatedcols = 0, # *** if using defaults of doaggregate()
            subgroups_type = input$subgroups_type, # nh, alone, or both # or use default of doaggregate() based on whatever subgroups_d etc are now ***
            include_ejindexes = (input$include_ejindexes == "TRUE"), # it was character not logical because of how input UI done
            calculate_ratios = input$calculate_ratios,
            extra_demog = input$extra_demog,
            need_proximityscore = input$need_proximityscore,
            infer_sitepoints = FALSE,
            called_by_ejamit = FALSE,
            updateProgress = updateProgress_doagg, ## pass progress bar function as argument
            testing = input$testing
          )
        )
        
        ## >>add "invalid_msg" for any missing after doaggregate() as invalid ####
        dup <- data_uploaded() # still includes invalid ones too!
        dup$valid <- dup$ejam_uniq_id %in% out$results_bysite$ejam_uniq_id

        dup$invalid_msg[!(dup$ejam_uniq_id %in% out$results_bysite$ejam_uniq_id)] <- 'dropped from doaggregate'
        data_uploaded <- dup  # odd syntax, but this replaced the reactive with the modified version of the reactive?
        
        ## stop and return NULL if no valid sites left after doaggregate
        if(all(dup$valid == FALSE)){
          message('No valid sites remaining. Quitting analysis')
          data_processed(NULL)
          
          progress_doagg$close()
          progress_all$close()
          shiny::showNotification('No valid site remaining, so the analysis was stopped. Please try a larger buffer radius or different dataset.',type = 'error',duration=5)
          validate('No valid sites remaining - analysis stopped')
        }
        # provide sitepoints table provided by user aka data_uploaded(), (or could pass only lat,lon and ST -if avail- not all cols?)
        # and doaggregate() decides where to pull ST info from -
        # ideally from ST column,
        # second from fips of block with smallest distance to site,
        # third from lat,lon of sitepoints intersected with shapefile of state bounds

        
        ## close doaggregate progress bar
        progress_doagg$close()
        
        ################################################################ #
        

        ## >>add "invalid_msg" for any dropped during getblocksnearby but not doaggregate  ####
        dup <- data_uploaded()
        dup$invalid_msg[is.na(dup$invalid_msg) & !(dup$ejam_uniq_id %in% sites2blocks$ejam_uniq_id)] <- 'no blocks found nearby'
        dup$valid <- dup$ejam_uniq_id %in% sites2blocks$ejam_uniq_id
        dup$invalid_msg[is.na(dup$invalid_msg) & !(dup$ejam_uniq_id %in% out$results_bysite$ejam_uniq_id)] <- 'unable to aggregate'
        dup$valid <- dup$ejam_uniq_id %in% out$results_bysite$ejam_uniq_id
        

        ## Handle sites dropped during getblocksnearby or doaggregate steps
        # dup <- data_uploaded()
        # dup$invalid_msg[is.na(dup$invalid_msg) & !(dup$ejam_uniq_id %in% sites2blocks$ejam_uniq_id)] <- 'no blocks found nearby'
        # dup$valid <- dup$ejam_uniq_id %in% sites2blocks$ejam_uniq_id
        # dup$invalid_msg[is.na(dup$invalid_msg) & !(dup$ejam_uniq_id %in% out$results_bysite$ejam_uniq_id)] <- 'unable to aggregate'
        # dup$valid <- dup$ejam_uniq_id %in% out$results_bysite$ejam_uniq_id
        # dup$invalid_msg[dup$valid & is.na(dup$ST)] <- 'Invalid State - no state %iles'

        if (submitted_upload_method() %in% c('MACT','FRS','latlon','EPA_PROGRAM_up',
                                             'EPA_PROGRAM_sel','NAICS','SIC')) {
          
          ## >>merge "valid" + invalid sites  ####
          # (to add those back into results, as mostly empty rows; but already done above for FIPS case)
          
          out$results_bysite <- merge(dup[, .(ejam_uniq_id, valid, invalid_msg)],
                                      out$results_bysite,
                                      by = 'ejam_uniq_id', all = T)
          
        } else if (submitted_upload_method() == 'SHP') {
          
          out$results_bysite <- merge(dup[, c('ejam_uniq_id','valid','invalid_msg')],
                                      out$results_bysite,
                                      by = 'ejam_uniq_id', all = T) %>%
            sf::st_drop_geometry()
        }
        ################################################################ #
        
        # add URLs >>(should be a function)  ####
        #
        #  >this should be a function, and is used by both server and ejamit() ###  #
        # duplicated almost exactly in ejamit() but reactives are not reactives there
        # maybe use url_4table() - see ejamit() code
        #
        #if ("REGISTRY_ID" %in% names(out$results_bysite)) {
        # echolink = url_echo_facility_webpage(out$results_bysite$REGISTRY_ID, as_html = FALSE)
        #} else {
        # echolink = url_echo_facility_webpage(out$results_bysite$REGISTRY_ID, as_html = FALSE)
        #}
        ## the registry ID column is only found in uploaded ECHO/FRS/NAICS data -
        ## it is not passed to doaggregate output at this point, so pull the column from upload to create URLS
        
        if ("REGISTRY_ID" %in% names(data_uploaded())) {
          escaped_registry_id = escape_html(data_uploaded()$REGISTRY_ID)
          echolink = url_echo_facility_webpage(escaped_registry_id, as_html = TRUE, linktext = 'ECHO Report')
        } else if ("RegistryID" %in% names(data_uploaded())) {
          escaped_registry_id = escape_html(data_uploaded()$RegistryID)
          echolink = url_echo_facility_webpage(escaped_registry_id, as_html = TRUE, linktext = 'ECHO Report')
        } else {
          echolink = rep('N/A', nrow(out$results_bysite))
        }
        
        if (submitted_upload_method() != 'SHP') {
          out$results_bysite[ , `:=`(
            `EJScreen Report` = ifelse(valid == T, url_ejscreen_report(    lat = d_upload$lat, lon =  d_upload$lon, radius = sanitized_bt_rad_buff(), as_html = TRUE), 'N/A'),
            `EJScreen Map`    = ifelse(valid == T, url_ejscreenmap(        lat = d_upload$lat, lon =  d_upload$lon,                             as_html = TRUE),  'N/A'),
            # `ACS Report`      = url_ejscreen_acs_report(lat = d_upload$lat, lon =  d_upload$lon, radius = sanitized_bt_rad_buff(), as_html = TRUE),
            `ECHO report` = ifelse(valid == T, echolink, 'N/A')
          )]
          out$results_overall[, `:=`(
            `EJScreen Report` = NA,
            `EJScreen Map` = NA,
            `ECHO report` = NA
          )]
        } else {
          ## setting shapefile URLs to NA for now
          out$results_bysite <- out$results_bysite %>%
            dplyr::mutate(
              `EJScreen Report` = 'N/A',#ifelse(valid == T, url_ejscreen_report(    lat = d_upload$lat, lon =  d_upload$lon, radius = sanitized_bt_rad_buff(), as_html = TRUE), 'N/A'),
              `EJScreen Map`    = 'N/A',#ifelse(valid == T, url_ejscreenmap(        lat = d_upload$lat, lon =  d_upload$lon,                             as_html = TRUE),  'N/A'),
              # `ACS Report`      = url_ejscreen_acs_report(lat = d_upload$lat, lon =  d_upload$lon, radius = sanitized_bt_rad_buff(), as_html = TRUE),
              `ECHO report` = 'N/A'#ifelse(valid == T, echolink, 'N/A')
            )
          out$results_overall <- out$results_overall %>%
            dplyr::mutate(
              `EJScreen Report` = NA,
              `EJScreen Map` = NA,
              `ECHO report` = NA
            )
        }
        newcolnames <- c(
          'valid','invalid_msg',
          "EJScreen Report",
          "EJScreen Map",
          "ECHO report"
        )
        newcolnames_overall <- c(
          "EJScreen Report",
          "EJScreen Map",
          "ECHO report"
        )
        # put those up front as first columns
        out$results_bysite <- dplyr::relocate(out$results_bysite, c('ejam_uniq_id', newcolnames), .before = 1)
        out$results_overall <- dplyr::relocate(out$results_overall, newcolnames_overall, .before = 2)
        
        # move ejam_uniq_id to front of longnames vector
        out$longnames <- c('ejam_uniq_id',newcolnames, out$longnames[out$longnames != 'ejam_uniq_id'])
        #############################################################################  #
        
        # add radius to results tables (in server and in ejamit() ####
        # out$results_bysite[      , radius.miles := sanitized_bt_rad_buff()]   # *** why not use data.table modify by reference, like this?
        # out$results_overall[     , radius.miles := sanitized_bt_rad_buff()]
        # out$results_bybg_people[ , radius.miles := sanitized_bt_rad_buff()]
        #
        out$results_bysite$radius.miles <- sanitized_bt_rad_buff()
        out$results_overall$radius.miles <- sanitized_bt_rad_buff()
        out$results_bybg_people$radius.miles <- sanitized_bt_rad_buff()
        
        # out$longnames <- NA # see ejamit()
        # out$formatted <- table_tall_from_overall(out$results_overall, out$longnames) # see ejamit()
        
      } # end of non fips, ie all latlon or shapefile aggregation
      
    } # done with all ways of analyzing ...  latlon, Shapefiles, and FIPS codes
    
    ## assign doaggregate output to data_processed reactive
    data_processed(out)
    
    ## update overall progress bar
    progress_all$inc(1/3, message = 'Step 3 of 3', detail = 'Summarizing')
    
    #############################################################################  #
    
    # 3) **batch.summarize()** on already processed data ####
    
    if (submitted_upload_method() == 'SHP') {
      outsum <- batch.summarize(
        sitestats = data.frame(data_processed()$results_bysite %>%
                                 sf::st_drop_geometry()),
        
        # popstats =  data.frame(data_processed()$results_bysite %>%  # batch.summarize no longer needs it passed twice
        #                          sf::st_drop_geometry()),
        ## user-selected quantiles to use
        #probs = as.numeric(input$an_list_pctiles),
        quiet = TRUE,
        thresholds   = list(input$an_thresh_comp1, input$an_thresh_comp2), # thresholds = list(90, 90),
        threshnames  = list(input$an_threshnames1, input$an_threshnames2), # list(c(names_ej_pctile, names_ej_state_pctile), c(names_ej_supp_pctile, names_ej_supp_state_pctile)),
        threshgroups = list(sanitized_an_threshgroup1(), sanitized_an_threshgroup2()) # list("EJ-US-or-ST", "Supp-US-or-ST")
      )
    } else {
      outsum <- batch.summarize(
        sitestats = data.frame(data_processed()$results_bysite),
        
        # popstats =  data.frame(data_processed()$results_bysite), # batch.summarize no longer needs it passed
        ## user-selected quantiles to use
        #probs = as.numeric(input$an_list_pctiles),
        quiet = TRUE,
        thresholds   = list(input$an_thresh_comp1, input$an_thresh_comp2), # thresholds = list(90, 90),
        threshnames  = list(input$an_threshnames1, input$an_threshnames2), # list(c(names_ej_pctile, names_ej_state_pctile), c(names_ej_supp_pctile, names_ej_supp_state_pctile)),
        threshgroups = list(sanitized_an_threshgroup1(), sanitized_an_threshgroup2()) # list("EJ-US-or-ST", "Supp-US-or-ST")
      )
    }
    ## update overall progress bar
    progress_all$inc(1/3, message = 'Done processing! Loading results now', detail = NULL)
    num_invalid_sites <- NROW(data_processed()$results_bysite[data_processed()$results_bysite$valid == F,])
    
    if (num_invalid_sites > 0) {
      showNotification(paste(num_invalid_sites, " sites were excluded from the Community Report results. Refer to the 'Invalid Reason' column in the details panel for more information."), 
                       type = "warning", duration = 7)
    }
    
    # assign batch.summarize output to data_summarized reactive ### #
    data_summarized(outsum)
    
    ## close overall progress bar
    progress_all$close()
    
    ## switch tabs and jump to top of screen
    shinyjs::js$toTop();
    showTab(session = session, inputId = 'all_tabs', target = 'See Results') # in case was hidden because app had just launched
    updateTabsetPanel(session, inputId = "all_tabs",     selected = "See Results")
    updateTabsetPanel(session, inputId = 'results_tabs', selected = 'Community Report')
  })  # end of observeEvent based on Start analysis button called input$bt_get_results
  
  #############################################################################  #
  # if (input$calculate_ratios) {  ## ratios can be dropped from output table of results but are used by summary report, plots, etc. so simplest is to still calculate them
  #############################################################################  #
  # . 4) ratios ####
  # ______ AVERAGES and RATIOS TO AVG
  
  # *** Ratios were already calculated by doaggregate() 
  # *** UNLESS somehow the user has changed the defaults to doaggregate(  calculate_ratios = FALSE  )
  
  ############################# #
  ##   demog RATIOS of overall scores to US or state D AVG ####
  
  ratio.to.us.d    <- reactive({unlist(
    data_processed()$results_overall[ , c(..names_d_ratio_to_avg,       ..names_d_subgroups_ratio_to_avg      )]
  ) }) # ???
  ratio.to.state.d <- reactive({unlist(
    data_processed()$results_overall[ , c(..names_d_ratio_to_state_avg, ..names_d_subgroups_ratio_to_state_avg)]
  ) }) # ???
  
  ############################# #
  ##   enviro RATIOS of overall scores to US or state E AVG  ####
  
  ratio.to.us.e    <- reactive({unlist(data_processed()$results_overall[ , ..names_e_ratio_to_avg]) })                     # ???
  # ratio.to.us.e_TEST <- reactive({ unlist(data_summarized()$rows['Average person', names_e]) /
  #     avg.in.us[, names_e]
  # doaggregate results_overall output, if needed: unlist(data_processed()$results_overall[1, ..names_e])
  # })
  ratio.to.state.e <- reactive({unlist(data_processed()$results_overall[ , ..names_e_ratio_to_state_avg]) })                     # ???
  
  #############################################################################  #
  # }
  #############################################################################  #
  # . --------------------------------------------------------------- ####
  #. ## ##
  # ______ SEE RESULTS _________ ####
  #. ####
  
  # #############################################################################  #
  
  # ________ SUMMARY REPORT  __________ ####
  
  ## Header ####
  
  ### ( Total Population count ) ####
  total_pop <- reactive({
    req(data_processed())
    ## format and return total population  ## some of these numbers seem very large! possible double-counting??
    round(data_processed()$results_overall$pop, table_rounding_info("pop") )
  })
  
  ### ( Title of analysis ) ####
  # Unless user changes it here, use a standard title that has been determined by global.R but then optionally modified by advanced settings tab
  
  output$analysis_title_ui <- renderUI({
    shiny::textInput('analysis_title',
                     label = 'Name of Your Analysis',
                     value = sanitized_standard_analysis_title())
  })
  
  ### summary header is stored in a reactive
  summary_title <- reactiveVal(NULL)
  
  ### summary header is updated when 'Start Analysis' button clicked
  ### or when Analysis Title is updated after a run
  observeEvent(
    ## allow title to update when either of these inputs change
    eventExpr = {
      input$bt_get_results
      sanitized_analysis_title()
    }, handlerExpr = {
      req(data_processed())
      ## paste header information together
      title_text <- paste0('<div style="font-weight: bold; font-size: 11pt; text-align: center;">',
                           sanitized_analysis_title(), '<br>')
      
      ## exclude radius info from header text when using FIPS
      if (current_upload_method() != 'FIPS') {
        title_text <- paste0(title_text,
                             'Residents within ',
                             #sanitized_bt_rad_buff(), ' ', input$radius_units, ' of any of the ',
                             sanitized_bt_rad_buff(), ' miles of any of the '
        )
      }
      title_text <- paste0(title_text,
                           prettyNum( NROW(data_processed()$results_bysite), big.mark = ","),
                           ' sites analyzed<br>',
                           #    "in the xxx source category or sector<br>",
                           'Population: ', prettyNum( total_pop(), big.mark = ","), '</div>'
      )
      ### update summary header reactive variable
      summary_title(title_text)
    })
  
  ### summary header is output as html
  output$view1_total_pop <- renderUI({
    HTML(summary_title())
  })
  
  #############################################################################  #
  ## *TABLE DEMOG (for summary report) ####
  
  v1_demog_table <- reactive({
    
    req(data_processed())
    # should it check if (input$calculate_ratios) or is it ok to show NA values instead of hiding those columns *** ?
    
    table_out_d <- table_gt_from_ejamit_overall(data_processed()$results_overall,
                                                type = 'demog')
    table_out_d
  })
  #
  # ## output:  gt  view1_demog_table()
  # output$view1_demog_table <- gt::render_gt({
  #   v1_demog_table()
  # })
  #############################################################################  #
  ## *TABLE ENVT. (for summary report) ####
  
  v1_envt_table <- reactive({
    
    req(data_processed())
    # should it check if (input$calculate_ratios) # *** ?
    
    tab_out_e <- table_gt_from_ejamit_overall(data_processed()$results_overall,
                                              type = "envt")
    tab_out_e
  })
  #
  # ## output: show environmental indicator table
  #
  # output$view1_envt_table <- gt::render_gt({
  #   v1_envt_table()
  # })
  
  #############################################################################  #
  
  ## *MAP (for summary report) ####
  
  report_map <- reactive({
    
    #req(data_processed())
    validate(need(data_processed(), 'Please run an analysis to see results.'))
    circle_color <- '#000080'
    
    #if shapefile, merge geometry and create buffer if nonzero buffer is set
    if (submitted_upload_method() == "SHP") {
      
      shp_valid <- data_uploaded()[data_uploaded()$valid == T, ]
      d_up <- shp_valid
      d_up_geo <- d_up[,c("ejam_uniq_id","geometry")]
      d_merge = merge(d_up_geo,data_processed()$results_bysite, by = "ejam_uniq_id", all.x = FALSE, all.y = TRUE)
      
      popup_labels <- fixcolnames(namesnow = setdiff(names(d_merge),c('geometry', 'valid', 'invalid_msg')), oldtype = 'r', newtype = 'shortlabel')
      
      rad_buff <- sanitized_bt_rad_buff()
      
      if (!is.na(rad_buff) && rad_buff > 0) {
        d_uploads <- sf::st_buffer(d_merge[d_merge$valid == T, ] , # was "ESRI:102005" but want 4269
                                   dist = units::set_units(rad_buff, "mi"))
        leaflet(d_uploads) %>%  addTiles()  %>%
          addPolygons(data = d_uploads, color = circle_color,
                      popup = popup_from_df(d_uploads %>% sf::st_drop_geometry() %>% dplyr::select(-valid, -invalid_msg), labels = popup_labels),
                      popupOptions = popupOptions(maxHeight = 200)) #%>%
      } else {
        data_spatial_convert <- d_merge[d_merge$valid == T, ] %>%
          dplyr::select(-valid, -invalid_msg) %>%
          sf::st_zm() %>% as('Spatial')
        leaflet(data_spatial_convert) %>% addTiles()  %>%
          addPolygons(color = circle_color,
                      popup = popup_from_df(data_spatial_convert %>% sf::st_drop_geometry(),
                                            labels = popup_labels),
                      popupOptions = popupOptions(maxHeight = 200))
      }
      
    } else { #  not shapefile
      
      if (submitted_upload_method() != "FIPS") {
        
        popup_labels <- fixcolnames(namesnow = names(data_processed()$results_bysite), oldtype = 'r', newtype = 'shortlabel')
        popup_labels[is.na(popup_labels)] <- names(data_processed()$results_bysite)[is.na(popup_labels)]
        ## similar to previous map but remove controls and only add circles, not circleMarkers
        
        ## switch this to data analyzed in report, not what was uploaded,   in case there are invalid
        leaflet(data_processed()$results_bysite) %>% #,
          #options = leafletOptions(zoomControl = FALSE, minZoom = 4)) %>%
          addTiles()  %>%
          addCircles(
            radius = submitted_radius_val() * meters_per_mile,
            color = circle_color, fillColor = circle_color,
            fill = TRUE, weight = input$circleweight_in,
            popup = popup_from_df(
              data_processed()$results_bysite %>%
                dplyr::mutate(dplyr::across(
                  dplyr::where(is.numeric), \(x) round(x, digits = 3))),
              labels = popup_labels),
            popupOptions = popupOptions(maxHeight = 200)
          )} else {
            
            req(data_uploaded())
            FTYPES <- fipstype(data_uploaded())
            if (all(FTYPES %in% "blockgroup") | (all(FTYPES %in% "county"))) {
              # FIPS   
              popup_labels <- fixcolnames(namesnow = names(data_processed()$results_bysite), oldtype = 'r', newtype = 'shortlabel')
              popup_labels[is.na(popup_labels)] <- names(data_processed()$results_bysite)[is.na(popup_labels)]
              
              fips_shapes <- shapes_counties_from_countyfips(countyfips = data_processed()$results_bysite$ejam_uniq_id)
              
              if (!is.null(fips_shapes) && nrow(fips_shapes) > 0) {
                
                leaflet(fips_shapes) %>%
                  addTiles() %>%
                  addPolygons(
                    data = fips_shapes,
                    color = "green",
                    popup = popup_from_df(
                      data_processed()$results_bysite %>%
                      dplyr::mutate(dplyr::across(
                        dplyr::where(is.numeric), \(x) round(x, digits = 3))),
                      labels = popup_labels
                    ),
                    popupOptions = popupOptions(maxHeight = 200)
                  )
              } else {
                #Possible failsafe needed if fips is invalid? Will it get to this stage? Blank map returned
                leaflet() %>% addTiles() %>% fitBounds(-115, 37, -65, 48)
              }
            } else{
              #Not counties or blockgroups, return empty map and warning
              
              warning('Cannot map FIPS types other than counties or blockgroups currently')
              leaflet() %>% addTiles() %>% fitBounds(-115, 37, -65, 48)
            
            }
            
          
          }
    }
    
  }) # end of report_map
  
  ## output: summary report map
  output$quick_view_map <- leaflet::renderLeaflet({
    
    ## use separate report map
    report_map()
    
    ## or can keep same map as on Site Selection tab
    # orig_leaf_map()
  })
  
  ## update leaflet map when inputs change
  ##   this is currently resetting map too often in response to checkbox  ***
  # observeEvent(eventExpr = {
  #   sanitized_bt_rad_buff()
  #   input$an_map_clusters
  #   is_clustered()
  #   #input$radius_units
  # }, {

  observe({
    # req(data_uploaded())
    ## This statement needed to ensure map stops if too many points uploaded
    req(isTruthy(orig_leaf_map()))
    
    #clear shapes from map so buffers don't show twice
    leafletProxy(mapId = 'an_leaf_map', session) %>% clearShapes()
    rad_buff <- sanitized_bt_rad_buff()
    if (current_upload_method() == "SHP") {
      if (!is.na(rad_buff) && rad_buff > 0) {
        shp_valid <- data_uploaded()[data_uploaded()$valid == T, ] # *** remove this if shapefile_clean() will do it
        d_uploads <- sf::st_buffer(shp_valid, # was "ESRI:102005" but want 4269
                                   dist = units::set_units(rad_buff, "mi"))
        leafletProxy(mapId = 'an_leaf_map', session) %>%
          addPolygons(data = d_uploads, color = "red")
      }
      d_uploads <- data_uploaded() %>%
        dplyr::select(-valid, -invalid_msg) %>%
        sf::st_zm() %>% as('Spatial') # st_zm() was already done?
      leafletProxy(mapId = 'an_leaf_map', session) %>%
        addPolygons(data = d_uploads,
                    popup = popup_from_df(d_uploads %>% sf::st_drop_geometry()),
                    popupOptions = popupOptions(maxHeight = 200))
    } else 
      
      if (current_upload_method() == 'FIPS') {
        
        leafletProxy(mapId = 'an_leaf_map', session) %>%
          map_shapes_leaflet_proxy(shapes = shapes_counties_from_countyfips(countyfips = data_uploaded()))
        
      } else {
        
        d_upload <- data_uploaded()
        base_color      <- '#000080'
        circle_color <- base_color
        popup_vec = popup_from_df(d_upload %>%
                                    dplyr::select(-valid, -invalid_msg))
        suppressMessages(
          leafletProxy(mapId = 'an_leaf_map', session, data = d_upload) %>%
            map_facilities_proxy(rad = sanitized_bt_rad_buff(),
                                 highlight = TRUE, #input$an_map_clusters,
                                 popup_vec = popup_vec,
                                 use_marker_clusters = nrow(d_upload) > marker_cluster_cutoff,
                                 clustered = FALSE)#is_clustered())
        )
      }
  })
  
  #############################################################################  #
  ## *BARPLOT for short and long reports (avg person D ratios vs US avg) ####
  
  # compare / merge with  EJAM/R/plot_barplot_ratios.R ***
  # https://exts.ggplot2.tidyverse.org/gallery/
  
  v1_summary_plot_state <- reactive({
    req(data_processed())
    if (!is.null(cur_button())) {
      # Extract the selected row number from cur_button
      selected_row <- as.numeric(gsub('button_', '', isolate(cur_button())))
      
      ejam2barplot(
        data_processed(),
        sitenumber = selected_row,
        varnames = c(names_d_ratio_to_state_avg, names_d_subgroups_ratio_to_state_avg),
        main = "Demographics at the Analyzed Location \n Compared to State Averages"
      )
    } else {
      # No specific location selected, use a default plot setup
      if (input$Custom_title_for_bar_plot_of_indicators == '') {
        # Default plot title
        ejam2barplot(
          data_processed(),
          varnames = c(names_d_ratio_to_state_avg, names_d_subgroups_ratio_to_state_avg),
          main = "Demographics at the Analyzed Location \n Compared to State Averages"
        )
      } else {
        # Custom title provided by user
        # Note: This piece may not be needed for state plot
        ejam2barplot(
          data_processed(),
          varnames = c(names_d_ratio_to_state_avg, names_d_subgroups_ratio_to_state_avg),
          main = input$Custom_title_for_bar_plot_of_indicators
        )
      }
    }
  })
  
  ###################  #
  
  v1_summary_plot <- reactive({
    # req(data_summarized()) # it used to say this is required here but I dont think it actually is used
    req(data_processed())
    # data_processed() needed for ridgeline or boxplot, and ratio.to.us.d() which is made from data_processed() is needed for boxplots,
    
    if (input$plotkind_1pager == 'bar') {
      if (!is.null(cur_button())) {
        selected_row <- as.numeric(gsub('button_', '', isolate(cur_button())))
        plot_barplot_ratios_ez(
          out = data_processed(),
          varnames = c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg),
          main = "Demographics at the Analyzed Location \n Compared to US Overall",
          single_location = TRUE,
          row_index = selected_row
        )
      } else {
        
        if (input$Custom_title_for_bar_plot_of_indicators == '') {
          
          #Default way
          plot_barplot_ratios_ez(
            out = data_processed(),
            varnames = c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg), 
          )
          
        } else {
          #If there is a new title in advanced settings
          plot_barplot_ratios_ez(
            out = data_processed(),
            varnames = c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg), 
            main = input$Custom_title_for_bar_plot_of_indicators
          )
        }
        
      }
    }
    else if (input$plotkind_1pager == 'ridgeline') {
      
      ## ratios by site  (demog each site / demog avg in US)
      ratio.to.us.d.bysite <- data_processed()$results_bysite[ ,  c(
        ..names_d_ratio_to_avg,
        ..names_d_subgroups_ratio_to_avg
      )]
      plot_ridgeline_ratios(ratio.to.us.d.bysite)
      
    } else {if (input$plotkind_1pager == "box") {
      
      ## *BOXPLOTS for short report (all sites D ratios vs US avg) ####
      
      # compare / merge with  boxplots_ratios()  ***
      
      # ****************************************************************************
      
      ## ratios by site  (demog each site / demog avg in US)
      ratio.to.us.d.bysite <- data_processed()$results_bysite[ ,  c(
        ..names_d_ratio_to_avg,
        ..names_d_subgroups_ratio_to_avg
      )]
      ## assign column names (could use left_join like elsewhere)
      names(ratio.to.us.d.bysite) <-  fixcolnames(c(names_d, names_d_subgroups), 'r', 'short') # is this right?
      ## pivot data from wide to long - now one row per indicator
      ratio.to.us.d.bysite <- ratio.to.us.d.bysite %>%
        tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'indicator') %>%
        ## replace Infs with NAs - these happen when indicator at a site is equal to zero
        dplyr::mutate(value = dplyr::na_if(value, Inf)) #%>%
      # NOTE NOW ratio.to.us.d.bysite IS A tibble, not data.frame, and is in LONG format now. !!!
      
      ## find max of ratios
      max.ratio.d.bysite <- max(ratio.to.us.d.bysite$value, na.rm = TRUE)
      max.name.d.bysite <- ratio.to.us.d.bysite$indicator[which.max(ratio.to.us.d.bysite$value)]
      ## specify  upper bound for ratios (will drop values above this from graphic)
      q75.maxof75s <- max(quantile(ratio.to.us.d.bysite$value, 0.75, na.rm = TRUE),na.rm = TRUE)
      ylimit <- ceiling(q75.maxof75s) # max of 75th pctiles rounded up to nearest 1.0x?
      max_limit <- max(3, ylimit, na.rm = TRUE) #
      # perhaps want a consistent y limits to ease comparisons across multiple reports the user might run.
      #  If the max value of any ratio is say 2.6, we might want ylim to be up to 3.0,
      #  if the max ratio is 1.01, do we still want ylim to be up to 3.0??
      #  if the max ratio or even max of 95th pctiles is >10, don't show it, but
      #  what if the 75th pctile value of some indicator is >10? expand the scale to always include all 75ths.
      
      ## find 75th %ile of ratios for the indicator with the max ratio
      q75.ratio.d.bysite <- quantile(ratio.to.us.d.bysite$value[ratio.to.us.d.bysite$indicator == max.name.d.bysite], 0.75, na.rm = TRUE)
      
      # to use for dot showing the mean ratio of each indicator *** NOT USED?
      meanratios <- data.frame(
        indicator = fixcolnames(c(names_d, names_d_subgroups), 'r', 'short'),      # is this right?
        value = unlist(ratio.to.us.d()[c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg)])
      )
      ## paste subtitle for boxplot
      subtitle <- paste0('Within ', sanitized_bt_rad_buff(),' miles of one site, ',
                         max.name.d.bysite, ' is ', round(max.ratio.d.bysite,1), 'x the US average\n' #,
                         # 'and 1 in 4 sites is at least ',round(q75.ratio.d.bysite,2), 'x the US average'
      )
      ## specify # of characters to wrap indicator labels
      n_chars_wrap <- 13
      towhat_nicename <- "US Average"
      mymaintext <- paste0("Ratios to ", towhat_nicename, ", as distributed across these sites")
      
      ##################################################################################### #
      
      ## much of this is plotting code is based on boxplots_ratios() - should consolidate
      
      ggplot2::ggplot(
        ratio.to.us.d.bysite  ,
        # mydata,
        aes(x = indicator, y = value )
      ) + #, fill = indicator)) +
        ## draw boxplots
        geom_boxplot() +
        
        #  show average persons ratio to US,  for each boxplot column
        # xxx   Try to fix / use this:
        # geom_point(
        #   data =  meanratios,
        #   aes(x = reorder(indicator, meanratios), y = value), colour = "orange", size = 2
        # ) +
        
        ## wrap indicator labels on x axis
        scale_x_discrete(labels = function(x) stringr::str_wrap(x, n_chars_wrap)) +
        ## set limits for ratio on y axis - use hard limit at 0, make upper limit 5% higher than max limit
        scale_y_continuous(limits = c(0,max_limit), expand = expansion(mult = c(0, 0.05))) +
        ## alternate version that clips top and bottom axes exactly at (0, max_limit)
        # scale_y_continuous(limits = c(0,max_limit), expand = c(0,0)) +
        
        ## add horizontal line at 1
        geom_hline(aes(yintercept = 1)) +
        ## set plot axis labels and titles
        labs(x = "",
             y = "Ratio of Indicator values in selected locations\n vs. US average value",
             subtitle = subtitle,
             title = mymaintext ) +
        # title = 'Ratio vs. US Average for Demographic Indicators') +
        
        ## draw individual dot per site? at least for small datasets?/few facilities - removed as they cover up boxplots with large datasets
        #geom_jitter(color = 'black', size = 0.4, alpha = 0.9, ) +
        
        ## set color scheme ?
        # actually do not need each a color, for boxplot.
        # scale_fill_brewer(palette = 'Dark2') +
        ## alternate color scheme
        # viridis::scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
        
        ggplot2::theme_bw() +
        ggplot2::theme(
          ## set font size of text
          text = ggplot2::element_text(size = 14),
          #axis.text  = ggplot2::element_text(size = 16),
          ## set font size of axis titles
          axis.title = ggplot2::element_text(size = 16),
          ## center and resize plot title
          plot.title = ggplot2::element_text(size = 22, hjust = 0.5),
          ## center subtitle
          plot.subtitle = ggplot2::element_text(hjust = 0.5),
          ## hide legend
          legend.position = 'none'
        )  # end of ggplot section
    }
    }
    # box
  })
  
  
  ## output: show box/barplot of indicator ratios in Summary Report #
  output$view1_summary_plot <- renderPlot({
    v1_summary_plot()
  })
  
  observeEvent(input$results_tabs, {
    
    if (!is.null(cur_button())) {
      cur_button(NULL)
      # Trigger a re-render of the summary plot
      output$view1_summary_plot <- renderPlot({
        v1_summary_plot()
      })
    }
    
  })
  
  
  #############################################################################  #
  
  # Function to generate HTML content ***
  # Modified community_download function to accept a row_index parameter
  # FUNCTION TO RENDER HTML REPORT ####
  
  community_download <- function(file, row_index = NULL) {
    
    # Create a progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Generating report", value = 0)
    
    # Ensure that the progress bar is closed when we exit this function
    on.exit(progress$close())
    
    progress$set(value = 0.1, detail = "Setting up temporary files...")
    if (!is.null(row_index)) {
      # single-site report
      tempReport <- setup_temp_files(Rmd_name = 'barplot_report_template.Rmd')
    } else {
      # overall summary multisite report
      tempReport <- setup_temp_files(Rmd_name = 'community_report_template.Rmd')
    }
    
    progress$set(value = 0.2, detail = "Defining parameters...")
    # Define parameters for Rmd rendering
    rad <- data_processed()$results_overall$radius.miles
    
    progress$set(value = 0.3, detail = "Adjusting data...")
    ################################################################### #
    
    # Adjust the data based on whether a specific row is selected
    
    if (!is.null(row_index)) {
      
      ## > report on just 1 site ####
      
      output_df <- data_processed()$results_bysite[row_index, ]
      popstr <- prettyNum(round(output_df$pop, 0), big.mark = ',')
      
      # Get the name of the selected location
      location_name <- output_df$statename
      
      locationstr <- paste0("Residents within ", rad, " mile", ifelse(rad > 1, "s", ""), 
                            " of this ", ifelse(submitted_upload_method() == 'SHP', "polygon", 
                                                ifelse(submitted_upload_method() == 'FIPS', "Census unit", "point")))
      
      # Create a filtered version of report_map for single location #####################  #
      
      progress$set(value = 0.4, detail = "Creating map...")
      
      single_location_map <- reactive({
        
        req(data_processed())
        validate(need(data_processed(), 'Please run an analysis to see results.'))
        
        filtered_data <- data_processed()$results_bysite[row_index, ]
        
        ####################### #        ####################### #
        if (submitted_upload_method() == "SHP") {
          # Handle shapefile case
          
          shp_valid <- data_uploaded()[data_uploaded()$ejam_uniq_id == filtered_data$ejam_uniq_id, ]
          d_up <- shp_valid
          d_up_geo <- d_up[,c("ejam_uniq_id","geometry")]
          d_merge = merge(d_up_geo, filtered_data, by = "ejam_uniq_id", all.x = FALSE, all.y = TRUE)
          
          popup_labels <- fixcolnames(namesnow = setdiff(names(d_merge),c('geometry', 'valid', 'invalid_msg')), oldtype = 'r', newtype = 'shortlabel')
          rad_buff <- sanitized_bt_rad_buff()
          
          if (!is.na(rad_buff) && rad_buff > 0) {
            d_uploads <- sf::st_buffer(d_merge[d_merge$valid == T, ], dist = units::set_units(rad_buff, "mi"))
            map <- leaflet(d_uploads) %>% addTiles() %>%
              addPolygons(data = d_uploads, color = '#000080',
                          popup = popup_from_df(d_uploads %>% sf::st_drop_geometry() %>% dplyr::select(-valid, -invalid_msg), labels = popup_labels),
                          popupOptions = popupOptions(maxHeight = 200))
          } else {
            data_spatial_convert <- d_merge[d_merge$valid == T, ] %>%
              dplyr::select(-valid, -invalid_msg) %>%
              sf::st_zm() %>% as('Spatial')
            map <- leaflet(data_spatial_convert) %>% addTiles() %>%
              addPolygons(color = '#000080',
                          popup = popup_from_df(data_spatial_convert %>% sf::st_drop_geometry(),
                                                labels = popup_labels),
                          popupOptions = popupOptions(maxHeight = 200))
          }
          
          # Get the bounding box of the shape
          bbox <- sf::st_bbox(d_merge)
          map %>% fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
            setView(lng = mean(c(bbox[1], bbox[3])), 
                    lat = mean(c(bbox[2], bbox[4])), 
                    zoom = 14)  
          
          ####################### #        ####################### #
        } else if (submitted_upload_method() != "FIPS") {
          # Handle non-FIPS case
          
          popup_labels <- fixcolnames(namesnow = names(filtered_data), oldtype = 'r', newtype = 'shortlabel')
          popup_labels[is.na(popup_labels)] <- names(filtered_data)[is.na(popup_labels)]
          
          map <- leaflet(filtered_data) %>%
            addTiles() %>%
            addCircles(
              radius = 1 * meters_per_mile,
              color = '#000080', fillColor = '#000080',
              fill = TRUE, weight = input$circleweight_in,
              popup = popup_from_df(
                filtered_data %>%
                  dplyr::mutate(dplyr::across(
                    dplyr::where(is.numeric), \(x) round(x, digits = 3))),
                labels = popup_labels),
              popupOptions = popupOptions(maxHeight = 200)
            )
          
          # Set view with a slightly more zoomed out level
          map %>% setView(lng = filtered_data$lon, 
                          lat = filtered_data$lat, 
                          zoom = 14) 
          
        } else {
          ####################### #        ####################### #
          # FIPS case
          
          leaflet() %>% addTiles() %>% fitBounds(-115, 37, -65, 48)
        }
      }) # end of map code  #####################  #
      map_to_use <- single_location_map()
      # end of map code  #####################  #
      
      ## make sure the barplots are for the 1 selected site
      
      # v1_summary_plot()
      
      # v1_summary_plot_state()
      
      
      params <- list(
        # output_df = output_df,
        analysis_title = input$analysis_title,
        totalpop = popstr,
        locationstr = locationstr,
        # include_ejindexes = (input$include_ejindexes == 'TRUE'),
        in_shiny = FALSE,
        filename = NULL,
        map = map_to_use,
        summary_plot = v1_summary_plot(),
        summary_plot_state = v1_summary_plot_state()
      )
      # end of report on 1 site 
      
    } else {
      ################################################################### #
      
      ## > report on just all sites overall ####
      
      output_df <- data_processed()$results_overall
      popstr <- prettyNum(round(total_pop(), 0), big.mark = ',')
      locationstr <- paste0("Residents within ", rad, " mile", ifelse(rad > 1, "s", ""), 
                            " of any of the ", NROW(data_processed()$results_bysite[data_processed()$results_bysite$valid == T,]), 
                            " selected ", ifelse(submitted_upload_method() == 'SHP', "polygons", 
                                                 ifelse(submitted_upload_method() == 'FIPS', "shapes", "points")))
      map_to_use <- report_map()
      
      v1_summary_plot_report <- reactive({
        # req(data_summarized()) # it used to say this is required here but I dont think it actually is used
        req(data_processed())
        # data_processed() needed for ridgeline or boxplot, and ratio.to.us.d() which is made from data_processed() is needed for boxplots,
        
        if (input$plotkind_1pager == 'bar') {
          if (!is.null(cur_button())) {
            selected_row <- as.numeric(gsub('button_', '', isolate(cur_button())))
            plot_barplot_ratios_ez(
              out = data_processed(),
              varnames = c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg),
              main = "Demographics at the Analyzed Location \n Compared to US Overall",
              single_location = TRUE,
              row_index = selected_row
            )
          } else {
            
            if (input$Custom_title_for_bar_plot_of_indicators == '') {
              
              #Default way
              plot_barplot_ratios_ez(
                out = data_processed(),
                varnames = c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg), 
                main = "Demographics at the Analyzed Location \n Compared to US Overall"
              )
              
            } else {
              #If there is a new title in advanced settings
              plot_barplot_ratios_ez(
                out = data_processed(),
                varnames = c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg), 
                main = input$Custom_title_for_bar_plot_of_indicators
              )
            }
            
          }
        }
        else if (input$plotkind_1pager == 'ridgeline') {
          
          ## ratios by site  (demog each site / demog avg in US)
          ratio.to.us.d.bysite <- data_processed()$results_bysite[ ,  c(
            ..names_d_ratio_to_avg,
            ..names_d_subgroups_ratio_to_avg
          )]
          plot_ridgeline_ratios(ratio.to.us.d.bysite)
          
        } else {if (input$plotkind_1pager == "box") {
          
          ## *BOXPLOTS for short report (all sites D ratios vs US avg) ####
          
          # compare / merge with  boxplots_ratios()  ***
          
          # ****************************************************************************
          
          ## ratios by site  (demog each site / demog avg in US)
          ratio.to.us.d.bysite <- data_processed()$results_bysite[ ,  c(
            ..names_d_ratio_to_avg,
            ..names_d_subgroups_ratio_to_avg
          )]
          ## assign column names (could use left_join like elsewhere)
          names(ratio.to.us.d.bysite) <-  fixcolnames(c(names_d, names_d_subgroups), 'r', 'short') # is this right?
          ## pivot data from wide to long - now one row per indicator
          ratio.to.us.d.bysite <- ratio.to.us.d.bysite %>%
            tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'indicator') %>%
            ## replace Infs with NAs - these happen when indicator at a site is equal to zero
            dplyr::mutate(value = dplyr::na_if(value, Inf)) #%>%
          # NOTE NOW ratio.to.us.d.bysite IS A tibble, not data.frame, and is in LONG format now. !!!
          
          ## find max of ratios
          max.ratio.d.bysite <- max(ratio.to.us.d.bysite$value, na.rm = TRUE)
          max.name.d.bysite <- ratio.to.us.d.bysite$indicator[which.max(ratio.to.us.d.bysite$value)]
          ## specify  upper bound for ratios (will drop values above this from graphic)
          q75.maxof75s <- max(quantile(ratio.to.us.d.bysite$value, 0.75, na.rm = TRUE),na.rm = TRUE)
          ylimit <- ceiling(q75.maxof75s) # max of 75th pctiles rounded up to nearest 1.0x?
          max_limit <- max(3, ylimit, na.rm = TRUE) #
          # perhaps want a consistent y limits to ease comparisons across multiple reports the user might run.
          #  If the max value of any ratio is say 2.6, we might want ylim to be up to 3.0,
          #  if the max ratio is 1.01, do we still want ylim to be up to 3.0??
          #  if the max ratio or even max of 95th pctiles is >10, don't show it, but
          #  what if the 75th pctile value of some indicator is >10? expand the scale to always include all 75ths.
          
          ## find 75th %ile of ratios for the indicator with the max ratio
          q75.ratio.d.bysite <- quantile(ratio.to.us.d.bysite$value[ratio.to.us.d.bysite$indicator == max.name.d.bysite], 0.75, na.rm = TRUE)
          
          # to use for dot showing the mean ratio of each indicator *** NOT USED?
          meanratios <- data.frame(
            indicator = fixcolnames(c(names_d, names_d_subgroups), 'r', 'short'),      # is this right?
            value = unlist(ratio.to.us.d()[c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg)])
          )
          ## paste subtitle for boxplot
          subtitle <- paste0('Within ', sanitized_bt_rad_buff(),' miles of one site, ',
                             max.name.d.bysite, ' is ', round(max.ratio.d.bysite,1), 'x the US average\n' #,
                             # 'and 1 in 4 sites is at least ',round(q75.ratio.d.bysite,2), 'x the US average'
          )
          ## specify # of characters to wrap indicator labels
          n_chars_wrap <- 13
          towhat_nicename <- "US Average"
          mymaintext <- paste0("Ratios to ", towhat_nicename, ", as distributed across these sites")
          
          ##################################################################################### #
          
          ## much of this is plotting code is based on boxplots_ratios() - should consolidate
          
          ggplot2::ggplot(
            ratio.to.us.d.bysite  ,
            # mydata,
            aes(x = indicator, y = value )
          ) + #, fill = indicator)) +
            ## draw boxplots
            geom_boxplot() +
            
            #  show average persons ratio to US,  for each boxplot column
            # xxx   Try to fix / use this:
            # geom_point(
            #   data =  meanratios,
            #   aes(x = reorder(indicator, meanratios), y = value), colour = "orange", size = 2
            # ) +
            
            ## wrap indicator labels on x axis
            scale_x_discrete(labels = function(x) stringr::str_wrap(x, n_chars_wrap)) +
            ## set limits for ratio on y axis - use hard limit at 0, make upper limit 5% higher than max limit
            scale_y_continuous(limits = c(0,max_limit), expand = expansion(mult = c(0, 0.05))) +
            ## alternate version that clips top and bottom axes exactly at (0, max_limit)
            # scale_y_continuous(limits = c(0,max_limit), expand = c(0,0)) +
            
            ## add horizontal line at 1
            geom_hline(aes(yintercept = 1)) +
            ## set plot axis labels and titles
            labs(x = "",
                 y = "Ratio of Indicator values in selected locations\n vs. US average value",
                 subtitle = subtitle,
                 title = mymaintext ) +
            # title = 'Ratio vs. US Average for Demographic Indicators') +
            
            ## draw individual dot per site? at least for small datasets?/few facilities - removed as they cover up boxplots with large datasets
            #geom_jitter(color = 'black', size = 0.4, alpha = 0.9, ) +
            
            ## set color scheme ?
            # actually do not need each a color, for boxplot.
            # scale_fill_brewer(palette = 'Dark2') +
            ## alternate color scheme
            # viridis::scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
            
            ggplot2::theme_bw() +
            ggplot2::theme(
              ## set font size of text
              text = ggplot2::element_text(size = 14),
              #axis.text  = ggplot2::element_text(size = 16),
              ## set font size of axis titles
              axis.title = ggplot2::element_text(size = 16),
              ## center and resize plot title
              plot.title = ggplot2::element_text(size = 22, hjust = 0.5),
              ## center subtitle
              plot.subtitle = ggplot2::element_text(hjust = 0.5),
              ## hide legend
              legend.position = 'none'
            )  # end of ggplot section
        }
        }
        # box
      })
      
      params <- list(
        output_df = output_df,
        analysis_title =  sanitized_analysis_title(),
        totalpop = popstr,
        locationstr = locationstr,
        include_ejindexes = (input$include_ejindexes == 'TRUE'),
        in_shiny = FALSE,
        filename = NULL,
        map = map_to_use,
        summary_plot = v1_summary_plot_report()
      )
    }
    
    # Render Rmd to HTML
    rmarkdown::render(tempReport,
                      output_format = 'html_document',
                      output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv()),
                      intermediates_dir = tempdir()
    )
  }
  #############################################################################  #
  
  # DOWNLOAD HTML REPORT Overall ####
  
  # downloadHandler for community_download - ALMOST THE SAME AS output$download_report 
  output$community_download_all <- downloadHandler(
    filename = function() {
      create_filename(
        file_desc = 'community report',
        title =  sanitized_analysis_title(),
        buffer_dist = submitted_radius_val(),
        site_method = submitted_upload_method(),
        with_datetime = TRUE,
        ext = ifelse(input$format1pager == 'pdf', '.pdf', '.html')
      )
    },
    content = function(file) {
      community_download(file)
    }
  )
  
  # DOWNLOAD HTML REPORT on 1 site ####
  
  # downloadHandler for the modal download button - Almost identical to code above. But content uses temp_file_path
  output$community_download_individual <- downloadHandler(
    filename = function() {
      location_suffix <- if (!is.null(selected_location_name())) {
        paste0(" - ", selected_location_name())
      } else {
        ""
      }
      create_filename(
        file_desc = paste0('community report', location_suffix),
        title =  sanitized_analysis_title(),
        buffer_dist = submitted_radius_val(),
        site_method = submitted_upload_method(),
        with_datetime = TRUE,
        ext = ifelse(input$format1pager == 'pdf', '.pdf', '.html')
      )
    },
    content = function(file) {
      req(temp_file_path())              # temp_file_path, unlike handler above
      file.copy(temp_file_path(), file)
    }
  )
  
  
  
  
  
  
  
  
  
  #############################################################################  #
  # .  ## ##
  # ______ DETAILED RESULTS ______ ####
  
  #############################################################################  #
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(id, ...))
    }
    inputs
  }
  #############################################################################  #
  #. ## ##
  ## *DATATABLE of sites = output$view3_table from data_processed()  ####
  #output: site by site datatable
  output$view3_table <- DT::renderDT(server = TRUE, expr = {
    req(data_processed())
    if (input$testing) {cat('view3_table - preparing (most columns of the) site by site table for DT view \n')
    }
    # --------------------------------------------------- #
    
    cols_to_select <- c('ejam_uniq_id', 'invalid_msg',
                        'pop', 'Barplot Report',
                        'EJScreen Report', 'EJScreen Map', 'ECHO report', # 'ACS Report',
                        names_d, names_d_subgroups,
                        names_e #,
                        # no names here corresponding to number above x threshold, state, region ??
    )
    
    tableheadnames <- c('Site ID', 'Invalid Reason','Est. Population', 'Barplot Report',  # should confirm that Barplot/Community Report belongs here
                        'EJScreen Report', 'EJScreen Map', 'ECHO report', #'ACS Report',
                        fixcolnames(c(names_d, names_d_subgroups, names_e), 'r', 'shortlabel'))
    ejcols          <- c(names_ej_pctile, names_ej_state_pctile, names_ej_supp_pctile, names_ej_supp_state_pctile)
    ejcols_short <- fixcolnames(ejcols, 'r', 'shortlabel')
    which_ejcols_here <- which(ejcols %in% names(data_processed()$results_bysite)  )
    cols_to_select <- c(cols_to_select, ejcols[         which_ejcols_here] )
    tableheadnames <- c(tableheadnames, ejcols_short[which_ejcols_here])
    tableheadnames <- c(tableheadnames,
                        names(data_summarized()$cols), 
                        # 'Max of selected indicators',  ###
                        # '# of indicators above threshold',   # will be made more flexible
                        'State', 'EPA Region')
    # --------------------------------------------------- #
    
    # use data_processed()
    dt <- data_processed()$results_bysite %>%
      as.data.frame() %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), .fns = function(x) {round(x, digits = 2)})
                    # *** This should not be hard coded to 2 digits - instead should follow rounding rules provided via table_round() and table_rounding_info() that use map_headernames$decimals  !
                    #
      ) %>%
      dplyr::mutate(index = row_number()) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        pop = ifelse(valid == T, pop, NA),
        # `EJScreen Report` = ifelse(valid == T, `EJScreen Report`, NA),
        # `ECHO Report` = ifelse(valid == T, `ECHO Report`, NA),
        # `EJScreen Map` = ifelse(valid == T, `EJScreen Map`, NA),
        `Barplot Report` = ifelse(valid == T, 
                                  shinyInput(FUN = actionButton, len = 1, 
                                             id = paste0('button_', index), 
                                             label = "Generate",
                                             onclick = paste0('Shiny.onInputChange(\"select_button', index,'\", this.id)' )
                                  ),
                                  '')
      ) %>%
      
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(cols_to_select), ST)
    
    # dt$`EJScreen Report` <- url_linkify(dt$`EJScreen Report`, text = 'EJScreen Report')
    # dt$`EJScreen Map` <-  url_linkify(dt$`EJScreen Map`, text = 'EJScreen Map')
    #    #### dt$`ACS Report` <-  url_linkify(dt$`ACS Report`, text = 'ACS Report')
    # dt$`ECHO report` <- ifelse(!is.na(dt$`ECHO report`),  url_linkify(dt$`ECHO report`, text = 'ECHO Report'), 'N/A')
    # dt_avg <- data_summarized()$rows[c('Average person','Average site'),] %>%
    #   dplyr::mutate(ejam_uniq_id = c('Average person', 'Average site'), ST = NA,
    #                 dplyr::across(dplyr::where(is.numeric), .fns = function(x) {round(x, digits = 2)}),
    #          ejam_uniq_id = as.character(ejam_uniq_id)) %>%
    #   dplyr::select(dplyr::all_of(cols_to_select), ST)
    
    # use data_summarized() that is from batch.summarize()
    batch.sum.cols = data_summarized()$cols
    batch.sum.cols[is.na(batch.sum.cols$pop), ] <- NA
    
    dt_final <- dt %>%
      dplyr::bind_cols(batch.sum.cols) %>%
      ## hide summary rows from table
      #dplyr::bind_rows(dt_avg) %>%
      #dplyr::bind_rows(dt_overall) %>%
      ## sort by Site ID - as numeric index
      #dplyr::arrange(ejam_uniq_id) %>%
      #dplyr::arrange(dplyr::desc(pop)) %>%
      # dplyr::mutate(
      #   Number.of.variables.at.above.threshold.of.90 = ifelse(
      #   is.na(pop), NA, 
      #   Number.of.variables.at.above.threshold.of.90)) %>%
      dplyr::mutate(pop = ifelse(is.na(pop), NA, pop)) %>% #prettyNum(round(pop), big.mark = ','))) %>%
      dplyr::left_join(stateinfo %>% dplyr::select(ST, statename, REGION), by = 'ST') %>%
      dplyr::mutate(
        REGION = factor(REGION, levels = 1:10),
        statename = factor(statename)
      ) %>%
      dplyr::select(-ST ) # , -Max.of.variables)    # should be made more flexible so column need not be Max.of.variables
    
    colnames(dt_final) <- tableheadnames
    
    dt_final <- dt_final %>%
      dplyr::relocate(c('Invalid Reason', State, 'EPA Region'),
                      # , '# of indicators above threshold'), 
                      .before = 2) # *** this cutoff should be dynamic, set by probs.default.values etc./ inputs
    
    ## set # of indicators above threshold to NA if population = 0
    # dt_final <- dt_final %>%
    #   dplyr::mutate(`# of indicators above threshold` = ifelse(`Est. Population` == 0, 'N/A',
    #                                                                `# of indicators above threshold`))
    
    ## drop indicator column until corrected
    # dt_final <- dt_final %>%
    #   select(-`# of indicators above threshold`)
    
    n_cols_freeze <- 1
    
    ## format data table of site by site table
    # see also  EJAM/inst/notes_MISC/DT_datatable_tips_options.R
    
    DT::datatable(dt_final,
                  rownames = FALSE,
                  ## add column filters (confirm that does work)
                  filter = 'top',
                  ## allow selection of one row at a time (remove to allow multiple)
                  #selection = 'single',
                  selection = 'none',
                  ## add-in for freezing columns
                  extensions = c('FixedColumns'),
                  options = list(
                    ## column width
                    autoWidth = TRUE,
                    ## remove global search box
                    dom = 'lrtip',
                    ## freeze header row when scrolling down
                    fixedHeader = TRUE,
                    fixedColumns = list(leftColumns = n_cols_freeze),
                    pageLength = 100,
                    ## allow scroll left-to-right
                    scrollX = TRUE,
                    ## set scroll height up and down
                    scrollY = '500px'
                  ),
                  ## set overall table height
                  height = 1500,
                  escape = FALSE  # *** escape = FALSE may add security issue but makes links clickable in table
    ) %>%
      DT::formatStyle(names(dt_final), 'white-space' = 'nowrap') %>%
      DT::formatRound('Est. Population', digits = 0, interval = 3, mark = ',')
    #DT::formatStyle(names(dt_final), lineHeight = '80%')
    ## code for bolding certain rows - not currently used
    #           ) %>%
    # DT::formatStyle(
    #   valueColumns = 'Site ID',
    #   target = 'row', columns = 'all',
    #   fontWeight = DT::styleEqual(c('All sites','Average person','Average site'), values = 'bold')
    # )
  })
  #############################################################################  #
  ## DOWNLOAD HTML REPORT on 1 SITE (via Button on Table of Sites) ####
  
  cur_button <- reactiveVal(NULL)
  temp_file_path <- reactiveVal(NULL)
  selected_location_name <- reactiveVal(NULL)
  
  ##############################################  #
  # Function to copy necessary files to temp directory
  setup_temp_files <- function(Rmd_name = 'community_report_template.Rmd', Rmd_folder = 'report/community_report/') {
    # or Rmd_name = 'barplot_report_template.Rmd' for single site barplot report
    tempReport <- file.path(tempdir(), Rmd_name)
    # Copy Rmd file to temp directory
    file.copy(from = EJAM:::app_sys(paste0(Rmd_folder, Rmd_name)),
              to = tempReport, overwrite = TRUE)
    # Copy CSS file
    if (!file.exists(file.path(tempdir(), 'communityreport.css'))) {
      file.copy(from = EJAM:::app_sys(paste0(Rmd_folder, 'communityreport.css')),
                to = file.path(tempdir(), 'communityreport.css'), overwrite = TRUE)
    }
    
    # Copy logo file
    if (!file.exists(file.path(tempdir(), 'www', 'EPA_logo_white_2.png'))) {
      dir.create(file.path(tempdir(), 'www'), showWarnings = FALSE, recursive = TRUE)
      file.copy(from = EJAM:::app_sys(paste0(Rmd_folder, 'EPA_logo_white_2.png')),
                to = file.path(tempdir(), 'www', 'EPA_logo_white_2.png'), overwrite = TRUE)
    }
    return(tempReport)
  }
  ##############################################  #
  
  ## Observe 1-site-report buttons ####
  # (1 button per site in the table of sites, to see barplot for that site)
  observeEvent(
    lapply(
      names(input)[grep("select_button[0-9]+", names(input))],
      function(name) {
        cur_button(input[[name]])
        input[[name]]
      }),  {
        req(data_processed())
        req(cur_button())
        x <- as.numeric(gsub('button_','', cur_button()))
        
        # Get the name of the selected location
        location_name <- data_processed()$results_bysite[x, "statename"]
        selected_location_name(location_name)
        
        # Create a temporary file name
        temp_file <- tempfile(fileext = ifelse(input$format1pager == 'pdf', '.pdf', '.html'))
        # Store the temporary file path in the reactive value
        temp_file_path(temp_file)
        
        # Call the community_download() function with the current row index
        community_download(file = temp_file, row_index = x)
        
        # Trigger the download
        showModal(modalDialog(
          title = "Download Ready",
          "Your report is ready. Click the button below to download.",
          footer = tagList(
            downloadButton("community_download_individual", "Download Report"),
            modalButton("Close")
          ),
          easyClose = TRUE,
          size = "m"
        ))
        
      })
  #############################################################################  #
  
  
  
  ## EXCEL DOWNLOAD  ####
  
  # SEE FUNCTION THAT CAN DO THIS AT ?table_xls_from_ejam() or ejam2excel()
  
  output$report_version_date <- renderUI({
    message(paste0("shinytestmode = ", getOption("shiny.testmode")))
    p(style = "margin-bottom: 0",
      paste("Version",
            ejam_app_version,
            "| Report created on",
            ifelse(
              isTRUE(getOption("shiny.testmode")),
              "[SHINYTEST DATE]",
              format(Sys.Date(), '%B %d, %Y'))))
  })
  output$download_results_table <- downloadHandler(
    filename = function() {
      
      create_filename(file_desc = 'results table',
                      title =  sanitized_analysis_title(),
                      buffer_dist = submitted_radius_val(),
                      site_method = submitted_upload_method(),
                      with_datetime = TRUE,
                      ext = '.xlsx')
    },
    content = function(fname) {
      showModal(
        modalDialog(title = 'Downloading',
                    'Downloading Excel file of results... Please wait.',
                    easyClose = FALSE)
      )
      if (input$testing) {
        cat('starting download code and  table_xls_format() \n') # ; xproc = data_processed(); save(xproc, file = 'table_data_processed-ejam.rda')
      }
      #  names( data_processed() )  #  "results_overall"  "results_bysite"  "results_bybg_people"  "longnames"  "count_of_blocks_near_multiple_sites"   "results_summarized"
      
      ## note analysis type or overview to 'notes' tab
      if (submitted_upload_method() %in% c("SHP","FIPS")) {
        radius_or_buffer_description <- 'Distance from each shape (buffering around each polygon)'
      } else {
        radius_or_buffer_description <- 'Distance from each site (radius of circle around a point/site)'
      }
      if (!input$calculate_ratios) {
        ratiocols <- names(data_processed()$results_overall) %in% c(names_d_ratio_to_avg, names_d_ratio_to_state_avg, names_e_ratio_to_avg, names_e_ratio_to_state_avg)
        keepcols <- !ratiocols
        # grepl("ratio.to", names(data_processed()$results_overall))
        
      } else {
        keepcols <- rep(TRUE, NCOL(data_processed()$results_overall))
        keepcols2 <- rep(TRUE, NCOL(data_processed()$results_bysite))
      }
      
      if ("ready to do this as a function" == "remove this when ready to switch") {
        
        # having to drop cols via keepcols here is a pain in the neck. is it really useful anyway
        x <- data_processed() # this copy must slow it down a bit, and waste memory, but can we just pass data_processed() reactive as a parameter without func expecting that or it being altered in this envt?
        x$results_overall <- x$results_overall[  , ..keepcols] # needs ..
        x$results_bysite  <- x$results_bysite[   , ..keepcols] # needs ..
        x$longnames       <- x$longnames[            keepcols] # no ..
        # see note below about extra tab for expert users with bg details
        wb_out <- table_xls_from_ejam(
          ejamitout = x, save_now = FALSE,
          
          #### *** to be finished. 
        )
        
      } else {
        progress_xl <- shiny::Progress$new(min = 0, max = 1)
        progress_xl$set(value = 0, message = 'Downloading', detail = 'Starting')
        updateProgress_xl <- function(value = NULL, message_detail=NULL, message_main = 'Preparing') {
          if (is.null(value)) { # - If value is NULL, it will move the progress bar 1/5 of the remaining distance.
            value <- progress_xl$getValue()
            value <- value + (progress_xl$getMax() - value) / 5
            message_main = paste0(value*100, '% done')
          }
          progress_xl$set(value = value, message = message_main, detail = message_detail)
        }
        
        #remove hyperlinks from excel output if shapefile is current_upload_method() - Temporary
        if (current_upload_method() == "SHP") {
          hyperlink_columns <- NULL
        }else{
          hyperlink_columns <- c("EJScreen Report", "EJScreen Map" ,'ECHO report')
        }
        
        html_content <- isolate({
          temp_file <- tempfile(fileext = '.html')
          community_download(file = temp_file, row_index = NULL)
        })
        
        wb_out <- table_xls_format(
          # note they seem to be data.frames, not data.tables, at this point, unlike how ejamit() had been returning results.
          overall   = data_processed()$results_overall |> dplyr::select(names( data_processed()$results_overall)[keepcols]),
          eachsite  = data_processed()$results_bysite |> dplyr::select(names( data_processed()$results_bysite)[keepcols2]),# needs ..  # 1 row per site
          longnames = data_processed()$longnames[           keepcols2], # not need ..       # 1 row, but full plain English column names.  keepcols here should be selecting cols not rows.
          
          custom_tab = data_summarized()$cols,
          custom_tab_name = "thresholds",
          
          # *** NOTE:  data_processed()$results_bybg_people  #considered not providing this to xlsx by default. It is huge and for expert users,
          # ***    but useful to create a plot of distance by group. Perhaps that could be created here to avoid passing the entire large table to table_xls_format() just for the plot. ***
          
          mapadd = FALSE,
          report_map = NULL,
          community_reportadd = TRUE,
          community_image = html_content,
          
          hyperlink_colnames = hyperlink_columns,  # need to ensure these get formatted right to work as links in Excel
          # heatmap_colnames = names(table_as_displayed)[pctile_colnums], # can use defaults
          # heatmap_cuts = c(80, 90, 95), # can use defaults
          # heatmap_colors = c('yellow', 'orange', 'red') # can use defaults
          ## optional, shiny-specific arguments to go in 'Plot' and 'Notes' sheets
          summary_plot   = v1_summary_plot(),
          ok2plot = input$ok2plot,
          plot_distance_by_group = TRUE,
          bybg = data_processed()$results_bybg_people,
          analysis_title =  sanitized_analysis_title(),
          buffer_desc    = "Selected Locations",
          radius_or_buffer_in_miles = sanitized_bt_rad_buff(),
          radius_or_buffer_description = radius_or_buffer_description,
          # saveas = fname,
          testing = input$testing,
          updateProgress = updateProgress_xl
        )
      }
      progress_xl$close()
      removeModal()
      ## save file and return for downloading - or do this within table_xls_format( , saveas=fname) ?
      
      openxlsx::saveWorkbook(wb_out, fname)
      
    } # end excel download contents
  ) # end download handler
  
  #############################################################################  #
  # ~ ###  #
  ## *MAP to find a site clicked in site-by-site table ####
  
  # specify what data will be mapped
  # data_sitemap <- reactiveVal(NULL)
  #
  # observeEvent(input$view3_table_rows_selected, {
  #   req(data_processed())
  #   #data_sitemap(data_uploaded()[input$view3_table_rows_selected,])
  #   if (submitted_upload_method() == 'SHP') {
  #     data_shp <- dplyr::inner_join(data_uploaded()[, c('ejam_uniq_id', 'geometry')], data_processed()$results_bysite[input$view3_table_rows_selected],
  #                                   by = c('ejam_uniq_id' = 'ejam_uniq_id'))
  #     print(input$view3_table_rows_selected)
  #     print(data_shp)
  #     data_sitemap(data_shp)
  #   } else {
  #     ## link selected row to doaggregate by site output for mapping
  #     data_sitemap(data_processed()$results_bysite[ejam_uniq_id %in% input$view3_table_rows_selected])
  #   }
  # })
  
  # output$v3_sitemap <- leaflet::renderLeaflet({
  #   ## wait for row to be selected
  #   ## note: summary rows are currently mapped but don't have a point location to map
  #   validate( need(!is.null(input$view3_table_rows_selected),
  #                  'Select a specific site in the table to see its location') )
  #
  #   ## zoom in from original map to show single point (can zoom out and see others)
  #
  #   #orig_leaf_map() #%>%
  #   # leaflet::setView(lat = data_sitemap()$lat, lng = data_sitemap()$lon, zoom = 8)
  #
  #   if (submitted_upload_method() == 'SHP') {
  #     ## alternate: plot single point individually on map (cannot zoom out and see others)
  #     leaflet(data_sitemap() %>% st_as_sf() %>% st_zm() %>% as('Spatial') ) %>%
  #       #setView(lat = data_sitemap()$lat, lng = data_sitemap()$lon, zoom = 13) %>%
  #       addTiles() %>%
  #       addPolygons(popup = popup_from_any(data_sitemap() %>% sf::st_drop_geometry()),
  #                   popupOptions = popupOptions(maxHeight =  200))
  #   } else {
  #     ## alternate: plot single point individually on map (cannot zoom out and see others)
  #     leaflet(data_sitemap()) %>%
  #       setView(lat = data_sitemap()$lat, lng = data_sitemap()$lon, zoom = 13) %>%
  #       addTiles() # %>%     ######## ***
  #     # addCircles(radius = 1 *  meters_per_mile, popup = popup_from_any(data_sitemap()), ######## ***
  #     #            popupOptions = popupOptions(maxHeight =  200))    ######## ***
  #   }
  # })
  #############################################################################  #
  #. ## ##
  ## *BARPLOTS interactive   ####
  #. ## ##
  # see ?ejam2barplot() in EJAM pkg
  # see notes on   https://exts.ggplot2.tidyverse.org/gallery/
  
  output$summ_bar_ind <- renderUI({
    if ((input$include_ejindexes == "TRUE")) {
      radioButtons(inputId = 'summ_bar_ind',
                   label = h5('Indicator type'),
                   choices = c('Demographic', 'Environmental', 'EJ','EJ Supplemental'), selected = "Environmental")
    } else {
      radioButtons(inputId = 'summ_bar_ind',
                   label = h5('Indicator type'),
                   choices = c('Demographic', 'Environmental'),
                   selected = "Environmental")
    }
  })
  
  # output:
  output$summ_display_bar <- renderPlot({
    req(data_summarized())
    req(input$summ_bar_ind)
    ## set indicator group column names
    mybarvars <- switch(input$summ_bar_ind,
                        'Demographic'   = c(names_d, names_d_subgroups),
                        'Environmental' = names_e,
                        'EJ'            = names_ej,
                        'EJ Supplemental'      = names_ej_supp
    )
    
    ## set indicator group friendly names - use shortlabel
    mybarvars.friendly <- fixcolnames(mybarvars, oldtype = 'r', newtype = 'shortlabel')
    
    ## only using average for now
    mybarvars.stat <- 'avg' #"med"
    
    ## defaulting to average only in this version of EJAM
    mybarvars.sumstat <- c('Average site',
                           'Average person at these sites')
    
    ## if adding median ('med') back in future, can use this
    #mybarvars.stat <- input$summ_bar_stat
    # mybarvars.sumstat <- switch(input$summ_bar_stat,
    #                             'med' =  c('Median site', 'Median person'),
    #                             'avg' = c('Average site','Average person')
    # )
    
    ## filter to necessary parts of batch.summarize output
    barplot_data <- data_summarized()$rows %>%
      tibble::rownames_to_column(var = 'Summary') %>%
      dplyr::mutate(Summary = gsub('Average person',
                                   'Average person at these sites',
                                   Summary)) %>%
      dplyr::filter(Summary %in% mybarvars.sumstat)
    
    ## set ggplot theme elements for all versions of barplot
    ggplot_theme_bar <- ggplot2::theme_bw() +
      ggplot2::theme(legend.position = 'bottom',
                     axis.text = ggplot2::element_text(size = 16),
                     axis.title = ggplot2::element_text(size = 16),
                     legend.title = ggplot2::element_text(size = 16),
                     legend.text = ggplot2::element_text(size = 16),
                     strip.text = ggplot2::element_blank(),
                     strip.background = ggplot2::element_blank()
      )
    
    ## raw data
    if (input$summ_bar_data == 'raw') {
      
      ## pivot from wide to long, 1 row per indicator
      barplot_data_raw <- barplot_data %>%
        dplyr::select(Summary, dplyr::all_of( mybarvars)) %>%
        tidyr::pivot_longer(cols = -1, names_to = 'indicator') %>%
        dplyr::mutate(type = 'raw')
      
      ## median - not currently displayed
      if (mybarvars.stat == 'med') {
        barplot_usa_med <- usastats %>%
          dplyr::filter(REGION == 'USA', PCTILE == 50) %>% # for median
          dplyr::mutate(Summary = 'Median person in US') %>%
          dplyr::select(Summary, dplyr::all_of(mybarvars)) %>%
          tidyr::pivot_longer(-Summary, names_to = 'indicator')
        
        ## NOTE: Median Person calculations are all 0s for now!
        barplot_input <- dplyr::bind_rows(barplot_data_raw, barplot_usa_med)
        
        ## average
      } else {
        barplot_usa_avg <- usastats %>%
          dplyr::filter(REGION == 'USA', PCTILE == 'mean') %>%
          dplyr::mutate(Summary = 'Average person in US') %>%
          dplyr::select(Summary, dplyr::all_of(mybarvars)) %>%
          tidyr::pivot_longer(-Summary, names_to = 'indicator')
        
        barplot_input <- dplyr::bind_rows(barplot_data_raw, barplot_usa_avg)
      }
      
      ## set # of characters to wrap labels
      n_chars_wrap <- 15
      
      barplot_input$Summary <- factor(barplot_input$Summary, 
                                      levels = c('Average person in US',
                                                 'Average site',
                                                 'Average person at these sites'))
      
      ## merge with friendly names and plot
      p_out <- barplot_input %>%
        dplyr::left_join( data.frame(indicator = mybarvars, indicator_label = gsub(' \\(.*', '', mybarvars.friendly))) %>%
        ggplot() +
        geom_bar(aes(x = indicator_label, y = value, fill = Summary), stat = 'identity', position = 'dodge') +
        #viridis::scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
        scale_fill_manual(values = c('Average person in US' = 'lightgray', 
                                     'Average person at these sites' = '#62c342',
                                     'Average site' = '#0e6cb5')) +
        #scale_fill_brewer(palette = 'Dark2') +
        scale_x_discrete(labels = function(x) stringr::str_wrap(x, n_chars_wrap)) +
        ## set y axis limits to (0, max value) but allow 5% higher on upper end
        scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))
      
      ## let environmental raw values have their own y axis
      if (input$summ_bar_ind == 'Environmental') {
        p_out <- p_out + facet_wrap(~indicator_label,
                                    #ncol = 4,
                                    scales = 'free')
      } else {
        p_out <- p_out + facet_wrap(~indicator_label,
                                    #ncol = 4,
                                    scales = 'free_x')
      }
      
      p_out +
        labs(x = NULL, y = 'Indicator Value', fill = 'Legend') +
        ggplot_theme_bar
      
      ## future: add % scaling and formatting for demographic indicators
      ## see ggplot2::scale_y_continuous and scales::label_percent
      
      ## ratio to us
    } else if (input$summ_bar_data == 'ratio') {
      
      barplot_data_raw <- barplot_data %>%
        dplyr::select(Summary, dplyr::all_of( mybarvars)) %>%
        tidyr::pivot_longer(cols = -1, names_to = 'indicator')
      
      ## average
      if (mybarvars.stat == 'avg') {
        ## pull US average values from usastats to compute ratios
        barplot_usa_avg <-  dplyr::bind_rows(
          usastats %>%
            dplyr::filter(REGION == 'USA', PCTILE == 'mean') %>%
            dplyr::mutate(Summary = 'Average person at these sites') %>%
            dplyr::select(Summary, dplyr::all_of(mybarvars)) %>%
            tidyr::pivot_longer(-Summary, names_to = 'indicator', values_to = 'usa_value'),
          usastats %>%
            dplyr::filter(REGION == 'USA', PCTILE == 'mean') %>%
            dplyr::mutate(Summary = 'Average site') %>%
            dplyr::select(Summary, dplyr::all_of(mybarvars)) %>%
            tidyr::pivot_longer(-Summary, names_to = 'indicator', values_to = 'usa_value')
        )
        
        ## combine raw data with US averages
        barplot_input <- dplyr::left_join(
          barplot_data_raw,
          barplot_usa_avg
        ) %>%
          ## divide to get ratios
          dplyr::mutate(ratio = value / usa_value) %>%
          ## add row of all 1s to represent US average ratio being constant at 1
          dplyr::bind_rows(
            data.frame(Summary = 'Average person in US', indicator = mybarvars, value = 1, usa_value = 1, ratio = 1)
          )
        
      } else {
        ## median - not currently displayed
        barplot_usa_med <-  dplyr::bind_rows(
          usastats %>%
            dplyr::filter(REGION == 'USA', PCTILE == 50) %>%
            dplyr::mutate(Summary = 'Median person') %>%
            dplyr::select(Summary, dplyr::all_of(mybarvars)) %>%
            tidyr::pivot_longer(-Summary, names_to = 'indicator', values_to = 'usa_value'),
          usastats %>%
            dplyr::filter(REGION == 'USA', PCTILE == 50) %>%
            dplyr::mutate(Summary = 'Median site') %>%
            dplyr::select(Summary, dplyr::all_of(mybarvars)) %>%
            tidyr::pivot_longer(-Summary, names_to = 'indicator', values_to = 'usa_value')
        )
        
        barplot_input <- dplyr::left_join(barplot_data_raw, barplot_usa_med) %>%
          ## calc ratio
          dplyr::mutate(ratio = value / usa_value) %>%
          dplyr::bind_rows(
            data.frame(Summary = 'Median person in US', indicator = mybarvars, value = 1, usa_value = 1, ratio = 1)
          )
      }
      
      ## set # of characters to wrap labels
      n_chars_wrap <- 15
      
      barplot_input$Summary <- factor(barplot_input$Summary, levels = c(
        'Average person in US',
        'Average site',
        'Average person at these sites'
      ))
      
      ## join and plot
      barplot_input %>%
        dplyr::left_join( data.frame(indicator = mybarvars, indicator_label =  gsub(' \\(.*', '', mybarvars.friendly))) %>%
        ggplot() +
        ## add bars - position = 'dodge' places the 3 categories next to each other
        geom_bar(aes(x = indicator_label, y = ratio, fill = Summary), stat = 'identity', position = 'dodge') +
        ## add horizontal line at 1
        geom_hline(aes(yintercept = 1)) +
        ## set color scheme
        scale_fill_manual(values = c(
          'Average person in US'          = 'lightgray',
          'Average person at these sites' = '#62c342',
          'Average site'                  = '#0e6cb5'
        )) +
        # scale_fill_brewer(palette = 'Dark2') +
        ## alternate color scheme
        #viridis::scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
        ## wrap long indicator labels on x axis
        scale_x_discrete(labels = function(x) stringr::str_wrap(x, n_chars_wrap)) +
        ## set y axis limits to (0, max value) but allow 5% higher on upper end
        scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
        ## set axis labels
        labs(x = '', y = 'Indicator Ratio', fill = 'Legend') +
        ## break plots into rows of 4
        facet_wrap(~indicator_label,
                   #ncol = 4,
                   scales = 'free_x') +
        ggplot_theme_bar
    }
  }) # end of summ_display_bar  for barplot
  
  #############################################################################  #
  ## *HISTOGRAM interactive    ####
  #. ####
  
  # other plot ideas
  # https://exts.ggplot2.tidyverse.org/gallery/
  current_hist_ind <- reactive({
    input$summ_hist_ind
  })
  
  output$summ_hist_ind <- renderUI({
    
    req(input$include_ejindexes)
    req(input$summ_hist_data)
    
    if ((input$include_ejindexes == "TRUE")) {
      
      ## use same root names so dropdown does not reset with other settings changing
      root_nms <-  c(names_d,
                     names_d_subgroups,
                     names_e, names_ej, names_ej_supp)
      
      if (input$summ_hist_data == 'pctile') {
        nms <-  c(names_d_pctile,
                  names_d_subgroups_pctile,
                  names_e_pctile, names_ej_pctile,
                  names_ej_supp_pctile)
        
        friendly_nms <- fixcolnames(nms, oldtype = 'r', newtype = 'shortlabel')
        
      } else if (input$summ_hist_data == 'raw') {
        friendly_nms <- fixcolnames(root_nms, oldtype = 'r', newtype = 'shortlabel')
      }
      
    } else {
      
      ## use same root names so dropdown does not reset with other settings changing
      root_nms <- c(names_d,
                    names_d_subgroups,
                    names_e)
      
      if (input$summ_hist_data == 'pctile') {
        nms <-  c(names_d_pctile, names_d_subgroups_pctile,names_e_pctile)
        
        friendly_nms <- fixcolnames(nms, oldtype = 'r', newtype = 'shortlabel')
        
      } else if (input$summ_hist_data == 'raw') {
        nms <-  c(names_d, names_d_subgroups, names_e)
        friendly_nms <- fixcolnames(root_nms, oldtype = 'r', newtype = 'shortlabel')
      }
      
    }
    selectInput('summ_hist_ind', label = 'Choose indicator',
                choices = setNames(
                  object = root_nms,
                  nm = friendly_nms
                ), # end setNames
                selected = current_hist_ind()
    ) # end selectInput
  })
  
  ## output:
  output$summ_display_hist <- renderPlot({
    
    # req(data_summarized()) # it used to say this was required here but i dont think it is anymore
    req(data_processed())
    req(input$summ_hist_ind)
    ## set font sizes
    ggplot_theme_hist <- ggplot2::theme(
      plot.title = ggplot2::element_text(size = 18, hjust = 0.5),
      axis.text  = ggplot2::element_text(size = 16),
      axis.title = ggplot2::element_text(size = 16)
    )
    
    ## future settings: bin sizes, reference lines
    
    if (input$summ_hist_distn == 'Sites') {
      if (input$summ_hist_data == 'raw') {
        
        ## subset doaggregate results_bysite to selected indicator
        if (submitted_upload_method() == 'SHP') {
          hist_input <- as.data.frame(data_processed()$results_bysite[, current_hist_ind()])#input$summ_hist_ind])
          
        } else {
          hist_input <- data_processed()$results_bysite[, current_hist_ind(), with=F]#input$summ_hist_ind, with = F]
          
        }
        names(hist_input)[1] <- 'indicator'
        
        ## plot histogram
        ggplot(hist_input) +
          geom_histogram(aes(x = indicator), fill = '#005ea2',
                         bins = input$summ_hist_bins) +
          ## set y axis limits to (0, max value) but allow 5% higher on upper end
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
          labs(
            x = '',
            y = 'Number of sites',
            title = 'Histogram of Raw Indicator Values Across Sites'
          ) +
          theme_bw() +
          ggplot_theme_hist
        
      } else if (input$summ_hist_data == 'pctile') {
        
        ## subset doaggregate results_bysite to selected indicator
        if (submitted_upload_method() == 'SHP') {
          
          hist_input <- as.data.frame(data_processed()$results_bysite[, paste0('pctile.',current_hist_ind())])#input$summ_hist_ind])
          
        } else {
          hist_input <- data_processed()$results_bysite[, paste0('pctile.',current_hist_ind()), with=F]#input$summ_hist_ind, with = F]
          
        }
        names(hist_input)[1] <- 'indicator'
        
        ggplot(hist_input) +
          geom_histogram(aes(x = indicator), fill = '#005ea2',
                         #bins = input$summ_hist_bins,
                         breaks = seq(0,100, length.out = input$summ_hist_bins+1)
          ) +
          labs(
            x = '',
            y = 'Number of Sites',
            title = 'Histogram of US Percentile Indicator Values Across Sites'
          ) +
          theme_bw() +
          ggplot_theme_hist
      }
    } else if (input$summ_hist_distn == 'People') {
      if (input$summ_hist_data == 'raw') {
        
        ## subset doaggregate results_bysite to selected indicator
        if (submitted_upload_method() == 'SHP') {
          
          hist_input <- as.data.frame(data_processed()$results_bysite[, c('pop',current_hist_ind())])
          
        } else {
          hist_input <- data_processed()$results_bysite[, c('pop',current_hist_ind()), with=F]
          
        }
        names(hist_input)[2] <- 'indicator'
        
        ## plot population weighted histogram
        ggplot(hist_input) +
          geom_histogram(aes(x = indicator, y = after_stat(density), weight = pop), fill = '#005ea2',
                         bins = input$summ_hist_bins) +
          ## set y axis limits to (0, max value) but allow 5% higher on upper end
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
          labs(
            x = '',
            y = 'Weighted Density',
            title = 'Population Weighted Histogram of Raw Indicator Values'
          ) +
          theme_bw() +
          ggplot_theme_hist
        
      } else if (input$summ_hist_data == 'pctile') {
        
        ## subset doaggregate results_bysite to selected indicator
        if (submitted_upload_method() == 'SHP') {
          
          hist_input <- as.data.frame(data_processed()$results_bysite[, c('pop',paste0('pctile.',current_hist_ind()))])
          
        } else {
          hist_input <- data_processed()$results_bysite[, c('pop',paste0('pctile.',current_hist_ind())), with=F]
          
        }
        names(hist_input)[2] <- 'indicator'
        
        ## plot population weighted histogram
        ggplot(hist_input) +
          geom_histogram(aes(x = indicator, y = after_stat(density), weight = pop), fill = '#005ea2',
                         #bins = input$summ_hist_bins,
                         breaks = seq(0,100, length.out = input$summ_hist_bins+1)
          ) +
          ## set y axis limits to (0, max value) but allow 5% higher on upper end
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
          labs(
            x = '',
            y = 'Weighted Density',
            title = 'Population Weighted Histogram of US Percentile Values'
          ) +
          theme_bw() +
          ggplot_theme_hist
      }
    }
  }) # end of summ_display_hist for histogram
  
  
  ## format textinput in Full Report tab using current input radius
  output$rg_enter_miles <- renderUI({   #   E.G.,   within 10 miles of
    
    shiny::textInput(inputId = "rg_enter_miles",
                     label = "Analysis Location:",
                     value = paste0("within ", sanitized_bt_rad_buff(),
                                    ' miles of')#,
                     #input$radius_units, " of")
    )
  })
  
  
  #############################################################################  #
  # ~--------------------------- ###
  # ______ FULL REPORT (Word doc) _________ ####
  # .  ## ##
  
  ## code for storing all shiny input values - not used currently
  # observeEvent(input$all_tabs == 'Generate Report',
  #  {
  #    list_of_inputs <- reactiveValuesToList(input)
  #  })
  
  ## Create and download FULL static report
  output$rg_download <- downloadHandler(
    filename = function(){
      create_filename(file_desc = 'full report',
                      title = sanitized_analysis_title(),
                      buffer_dist = submitted_radius_val(),
                      site_method = submitted_upload_method(),
                      with_datetime = TRUE,
                      ext = '.doc')
      
    },
    
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      ## copy Rmd from inst/report to temp folder
      file.copy(from = EJAM:::app_sys('report/written_report/report.Rmd'),  # treats EJAM/inst/ as root
                to = tempReport, overwrite = TRUE)
      ## pass image and bib files needed for knitting to temp directory
      for (i in list.files(EJAM:::app_sys('report/written_report'), pattern = '.png|.bib')) {   # treats what was in source/EJAM/inst/report/ as installed/EJAM/report/  once pkg is installed
        file.copy(from = EJAM:::app_sys('report/written_report', i),    # source/EJAM/inst/report/ = installed/EJAM/report/
                  to = file.path(tempdir(), i),
                  overwrite = TRUE)
      }
      
      # Set up parameters to pass to Rmd document -
      #  MAKE SURE all parameter names are used (identical names, and all are there) in these 4 places:
      #  1. input$ ids in app_ui.R, from user, to customize the long report
      #  2. params$ list passed by app_server.R to render the Rmd doc
      #  3. params: accepted in  .Rmd yaml info header
      #  4. params$  as used within body of  .Rmd text inline and in r code blocks.
      
      isolate({ # need someone to confirm this is needed/helpful and not a problem, to isolate this.
        
        params <- list(
          testmode = FALSE,
          
          #------- WHERE was analyzed? (where/ what sector/zones/types of places)
          
          analysis_title =   sanitized_analysis_title(),
          zonetype =  input$rg_zonetype,
          where = input$rg_enter_miles,
          distance = paste0(sanitized_bt_rad_buff(),' miles'), #input$radius_units),
          sectorname_short = input$rg_enter_sites,
          ## allow for either or
          in_the_x_zone = ifelse(nchar(input$in_the_x_zone_enter) > 0,
                                 input$in_the_x_zone_enter,
                                 input$in_the_x_zone),
          facilities_studied = ifelse(nchar(input$facilities_studied_enter) > 0,
                                      input$facilities_studied_enter,
                                      input$facilities_studied),
          within_x_miles_of = paste0("within ", paste0(sanitized_bt_rad_buff(),' miles'), " of"),
          
          in_areas_where = paste0(input$in_areas_where, ' ', input$in_areas_where_enter),
          risks_are_x = input$risks_are_x,
          source_of_latlons = input$source_of_latlons,
          sitecount = nrow(data_processed()$results_bysite),
          
          #------- RESULTS (tables and map and plots)
          
          total_pop  = prettyNum( total_pop(), big.mark = ","),
          results =  data_processed(),  # do we need to pass the entire table? may want to use it in appendices, etc.
          results_formatted =  table_tall_from_overall(data_processed()$results_overall, data_processed()$longnames),
          map =  report_map(),
          # map_placeholder_png =                 "map_placeholder.png",
          envt_table =  v1_envt_table(),
          # envt_table_placeholder_png =   "envt_table_placeholder.png",
          # envt_table_placeholder_rda =   "envt_table_placeholder.rda",
          demog_table = v1_demog_table(),
          # demog_table_placeholder_png = "demog_table_placeholder.png",
          # demog_table_placeholder_rda = "demog_table_placeholder.rda",
          boxplot =     v1_summary_plot(), # actually a barplot
          ## also note  v1_summary_plot_state()
          # boxplot_placeholder_png =         "boxplot_placeholder.png",
          # barplot= NA
          # barplot_placeholder_png =         "barplot_placeholder.png",
          
          #------- TEXT PHRASES DESCRIBING AND INTERPRETING RESULT
          
          demog_how_elevated = input$demog_how_elevated,
          envt_how_elevated = input$envt_how_elevated,
          demog_high_at_what_share_of_sites = input$demog_high_at_what_share_of_sites,
          envt_high_at_what_share_of_sites = input$envt_high_at_what_share_of_sites,
          conclusion1 = input$conclusion1,
          conclusion2 = input$conclusion2,
          conclusion3 = input$conclusion3,
          
          #------- METHODS, AUTHORS, ETC.
          
          authorname1     = input$rg_author_name,
          authoremail1    = input$rg_author_email,
          coauthor_names  = input$coauthor_names,
          coauthor_emails = input$coauthor_emails,
          fundingsource   = input$fundingsource,   # need to add input
          acs_version =  acs_version_global,
          ejscreen_version =  ejscreen_version_global
        )
      })
      # [TEMPORARILY SAVE PARAMS FOR TEST ING] ## ##
      # if (input$testing) {saveRDS(params, file = "./inst/testparams.RDS")} ################################ TEMPORARILY SAVE PARAMS FOR TESTING# #
      
      # Knit report to Word Doc ## ##
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_format = 'word_document',
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()),
                        intermediates_dir = tempdir()
      )
    } # end of download function
  ) # end of long report download handler
  
  ##
  
  # ______ Community Report page output as HTML ______ ####
  
  output$comm_report_html <- renderUI({
    req(data_processed())
    
    sitetype <- tolower(submitted_upload_method())
    rad <- data_processed()$results_overall$radius.miles # input radius can be changed by user and would alter the report text but should just show what was run not what slider currently says
    nsites <- NROW(data_processed()$results_bysite[data_processed()$results_bysite$valid == T, ])
    popstr <- prettyNum(total_pop(), big.mark = ',') # rounded already?
    
    # report_residents_within_xyz <- function(sitetype = c('latlon', 'fips', 'shp')[1], radius = NULL, nsites = 1) {
    #   
    #   report_xmilesof <- function(radius) {
    #     xmilesof <- ifelse(
    #       (is.null(radius) || radius == 0), "",
    #       paste0(radius, " mile", ifelse(radius > 1, "s", ""), " of ")
    #     )
    #     return(xmilesof)
    #   }
    #   xmilesof <- report_radiusstr(radius)
    #   
    #   if (sitetype == 'shp') {
    #     location_type <- " selected polygon"
    #   } else if (sitetype == 'fips') {
    #     location_type <- " selected Census unit"
    #   } else if (sitetype == 'latlon') {
    #     location_type <- " selected point"
    #   } else {
    #     # unknown sitetype?
    #     location_type <- " place"
    #   }
    #   anyoftheplaces <- ifelse(nsites == 1, 
    #                            paste0('this', location_type),
    #                            paste0("any of the ", nsites, location_type, "s")
    #   )
    #   residents_within_xyz <- paste0("Residents within ",
    #                                  xmilesof,
    #                                  anyoftheplaces)
    #   return(residents_within_xyz)
    # }
    
    residents_within_xyz <- report_residents_within_xyz(
      sitetype = sitetype,
      radius = rad,
      nsites = nsites
    )
    
    ## generate full HTML using external functions
    full_page <- build_community_report(
      output_df = data_processed()$results_overall,
      analysis_title =  sanitized_analysis_title(),
      totalpop = popstr,
      locationstr = residents_within_xyz,
      include_ejindexes = (input$include_ejindexes == 'TRUE'),
      # plotonly = ,
      in_shiny = TRUE
    )
    ## return generated HTML
    full_page
  })
  # end of observer that send results of calculation to UI
  
  #. ####
  #############################################################################  #
  #
  # ______ ejscreenapi MODULE _________ ####
  # (to get batch via API)  see default_hide_ejscreenapi_tab in global.R etc.
  
  # create UI part for main EJAM app here rather than in app_ui because we need access to input$ radius of main app to pass that to module that uses it as the initial radius shown on its slider
  # pass from server code of app to server code of module by creating the main radius UI in server of app (cannot access the input$ in UI of app)
  # Not sure if I can pass a reactive or need to pass reactive value so it can get updated by the module without needing to return a list of values from the module?
  # default_radius_react_passed <- reactiveVal() # initialize/create the variable that will store the latest radius set by outer app
  # observe(
  #   default_radius_react_passed(sanitized_bt_rad_buff()) # update the value of this reactiveVal anytime outer app slider is adjusted
  # )
  
  # output$mod_ejscreenapi_ui_TO_SHOW_IN_APP_UI <- renderUI({
  #   mod_ejscreenapi_ui("x2",
  #                      simpleradius_default_for_ui = 1 # ,
  #                      # default_radius_react = default_radius_react_passed
  #                      ) # reactive object gets passed without parentheses. pass a reactive radius HERE to server not ui.
  # })
  
  # default_radius_react_passed <- reactiveVal(sanitized_bt_rad_buff()) # pass to UI of module not server code of module
  # default_points_react_passed <- reactiveVal() # initialize it empty
  # observe(
  #   default_points_react_passed(  data_uploaded()  ) # update default_points_react_passed when data_uploaded() changes
  # )
  # table_as_displayed_reactive <- reactive(
  #
  #   mod_ejscreenapi_server(
  #     "x2",
  #     default_points_shown_at_startup_react = default_points_react_passed, #reactive(testpoints_5[1:2,]),
  #     use_ejscreenit = T # use_ejscreenit_tf
  #   )
  #
  #   # mod_ejscreenapi_server("x2",
  #   #                        # default_points = testpoints_5[1:2,],
  #   #                        default_radius_react = default_radius_react_passed,
  #   #                        default_points_shown_at_startup_react = default_points_react_passed  #reactive value object gets passed without parentheses
  #   # )
  # )
  
  # NOTE:
  # If a module needs to use a reactive expression, the outer function should take the reactive expression as a parameter.
  # If a module wants to return reactive expressions to the calling app, then return a list of reactive expressions from the function.
  # If a module needs to access an input that isn’t part of the module, the
  #   containing app should pass the input value wrapped in a reactive expression (i.e. reactive(...)):
  #   myModule("myModule1", reactive(input$checkbox1))
  #
  # x = 0
  # x <- reactive({
  #   req(data_uploaded())
  #   mod_ejscreenapi_server("x2", default_points_react = data_uploaded())
  # }) %>%
  #   bindCache(req(data_uploaded, ))
  # if (is.reactive(x)) {cat("API module output is a reactive value \n")} else {cat("API module output is not reactive \n")}
  
  # Not sure if or when control would get passed back to EJAM main app code or when x might be assigned or what***
  #  I think x will be a reactive that is the output table?? ***
  # try to pass (to module) the data_uploaded() points already uploaded in EJAM app points
  # try to get output here? and do what? display or pass back to the EJAM app code that can show all the info and download it?
  
  #. ####
  
} # end of app_server
