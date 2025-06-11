#' app_server - EJAM app server
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @import data.table
#' @importFrom data.table ":="
#' @import shinycssloaders
#' @importFrom magrittr '%>%'
#' @importFrom grDevices dev.off png heat.colors colors colorRampPalette palette.colors
#' @importFrom graphics abline barplot legend points
#' @importFrom methods Summary as
#' @importFrom stats aggregate density na.omit quantile runif setNames
#' @importFrom utils data download.file installed.packages object.size read.csv stack tail
#' @rawNamespace import(dplyr, except = c(first, last, between))
#'
#'
#' @keywords internal
#'
app_server <- function(input, output, session) {

  #############################################################################  #
  # Key reactives  data_uploaded(), data_processed(), data_summarized() ####
  ##  data_uploaded   reactive holds selected latlon points or shapefiles. It is defined later.
  ##  data_processed  reactive holds results of analysis, like output of doaggregate(getblocksnearby(points))
  ##  data_summarized reactive holds results of batch.summarize()

  data_processed <-  reactiveVal(NULL) # initialized so it can be set later in reaction to an event, using data_processed(newvalue)

  sanitized_standard_analysis_title <- reactive({
    EJAM:::global_or_param("sanitize_text")(input$standard_analysis_title)
  })

  sanitized_analysis_title <- reactive({
    EJAM:::global_or_param("sanitize_text")(input$analysis_title)
  })

  sanitized_an_threshgroup1 <- reactive({
    EJAM:::global_or_param("sanitize_text")(input$an_threshgroup1)
  })

  sanitized_an_threshgroup2 <- reactive({
    EJAM:::global_or_param("sanitize_text")(input$an_threshgroup2)
  })

  sanitized_bt_rad_buff <- reactive({
    req(input$bt_rad_buff)
    EJAM:::global_or_param("sanitize_numeric")(input$bt_rad_buff)
  })
  ################################################################### #
  # testing/dev mode settings ####
  observe({
    if (input$testing) {cat("testing == TRUE \n ")} else {cat("testing == FALSE \n----------------------\n")}
  })
  observe({
    if (length(EJAM:::global_or_param("default_shiny.testmode")) > 0  ) { # allow params in run_app() to override default
      if (!isTRUE(EJAM:::global_or_param("default_shiny.testmode"))) {
        updateRadioButtons(session = session, inputId = "shiny.testmode", selected = FALSE)
        # if (!isTRUE(getOption("shiny.testmode"))) {
        #   options(shiny.testmode = FALSE)
        # }
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
  # -- can also be used with render functions (like renderText() and renderPlot()).
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

  ## "back to site selection" buttons (to go back to site selection tab from results tab)
  observeEvent(input$back_to_site_sel,  {updateTabsetPanel(session = session, inputId = 'all_tabs', selected = 'Site Selection')})  # might not  be used
  observeEvent(input$back_to_site_sel2, {updateTabsetPanel(session = session, inputId = 'all_tabs', selected = 'Site Selection')})
  ## "return to results" button (to go from site selection to results)
  observeEvent(input$return_to_results, {updateTabsetPanel(session = session, inputId = "all_tabs", selected = "See Results")})  # updateTabsetPanel(session, 'results_tabs', 'Summary')
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

  # could use      global_or_param()   here instead
  if (isTRUE(EJAM:::global_or_param("default_hide_advanced_settings"))) {
    hideTab(inputId = 'all_tabs', target = 'Advanced Settings')
  } else {
    showTab(inputId =  "all_tabs", target = 'Advanced Settings')
  }

  ## hide vs show ADVANCE tab on button click (button in 'About EJAM' tab) ***

  observeEvent(input$ui_show_advanced_settings,
               {showTab(inputId = 'all_tabs', target = 'Advanced Settings')})
  observeEvent(input$ui_hide_advanced_settings,
               {hideTab(inputId = 'all_tabs', target = 'Advanced Settings')})

  ## hide vs show ABOUT tab  ---------------------- #   ***
  if (EJAM:::global_or_param("default_hide_about_tab")) {
    hideTab(inputId = 'all_tabs', target = 'About')
  }
  ## hide vs show WRITTEN REPORT tab ---------------------- #   ***
  if (EJAM:::global_or_param("default_hide_written_report")) {
    hideTab(inputId = 'results_tabs', target = 'Written Report')
  }

  ## hide vs show BARPLOTS tab  ---------------------- #   ***
  if (EJAM:::global_or_param("default_hide_plot_barplot_tab")) {
    hideTab(inputId = 'details_subtabs', target = 'Plot Average Scores')
  }

  ## hide vs show HISTOGRAMS tab  ---------------------- #   ***
  if (EJAM:::global_or_param("default_hide_plot_histo_tab")) {
    hideTab(inputId = 'details_subtabs', target = 'Plot Full Range of Scores')
  }

  ## advanced tab provides size cap on file uploads  ---------------------- #

  max_mb_upload_react <- reactive({
    x <- as.numeric((input$max_mb_upload))
    if (is.null(x) || is.na(x) || length(x) == 0) {
      x <- default_max_mb_upload
      shiny::updateNumericInput(session = session, inputId = "max_mb_upload", value = x)
    } else {
      if (x > EJAM:::global_or_param("maxmax_mb_upload")) {x <- EJAM:::global_or_param("maxmax_mb_upload")}
      if (x < EJAM:::global_or_param("minmax_mb_upload")) {x <- EJAM:::global_or_param("minmax_mb_upload")}
      shiny::updateNumericInput(session = session, inputId = "max_mb_upload", value = x)
    }
    x
  })
  observe({
    # Adjusts cap on file size user can upload, and resets file inputs in case
    #   last attempt failed due to size, so you can retry with new cap.
    options(shiny.maxRequestSize = max_mb_upload_react() * 1024^2)
    ids_of_fileInput_lines <- c("ss_upload_latlon", "ss_upload_shp",
                                "ss_upload_frs", "ss_upload_program","ss_upload_fips")
    for (idx in ids_of_fileInput_lines) {shinyjs::reset(idx)}
    # *** and also note   ns("pointsfile") and  ns("shapefile")  in  modules should be added and handled if those modules get used
  })
  # output$max_mb_upload_react_txt <- renderText({
  #   paste0("Maximum file size allowed for upload: ", max_mb_upload_react(), " MB")
  # })

  ## buttons to see help info  ---------------------- #

  observeEvent(input$latlon_help, {
    showModal(modalDialog(HTML(EJAM:::global_or_param("latlon_help_msg")), easyClose = TRUE))})
  observeEvent(input$frs_help, {
    showModal(modalDialog(HTML(EJAM:::global_or_param("frs_help_msg")),    easyClose = TRUE))})
  observeEvent(input$epa_program_help, {
    showModal(modalDialog(HTML(EJAM:::global_or_param("epa_program_help_msg")), easyClose = TRUE))})
  observeEvent(input$fips_help, {
    showModal(modalDialog(HTML(EJAM:::global_or_param("fips_help_msg")),   easyClose = TRUE))})
  observeEvent(input$shp_help, {
    showModal(modalDialog(HTML(EJAM:::global_or_param("shp_help_msg")),    easyClose = TRUE))})
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
    updateSelectizeInput(session = session, inputId = 'ss_select_naics',
                         ## use named list version, grouped by first two code numbers
                         choices = naics_choices, # need to keep formatting
                         selected = vals,
                         #choices = NAICS, # named list of codes, data loaded with EJAM package
                         server = TRUE)
  })

  # update ss_select_SIC input options ###
  updateSelectizeInput(session = session, inputId = 'ss_select_sic',
                       choices = SIC, # named list of codes
                       server = TRUE)
  #############################################################################  #

  # SELECT Facility Type vs UPLOAD Latlon/ id/ fips/ shape (radio button) ####

  output$ss_choose_method_ui <- renderUI({
    req(input$default_ss_choose_method)
    if (input$testing) {cat("input$default_ss_choose_method is ", input$default_ss_choose_method, "\n")}
    radioButtons(inputId = 'ss_choose_method',
                 label = 'How would you like to identify locations?',
                 choiceNames = c('Select a category of locations',
                                 'Upload specific locations'),
                 choiceValues = c('dropdown',
                                  'upload'),
                 # selected = "upload" # hard-coded default selection
                 selected = input$default_ss_choose_method  # flexible default selection
    )
    if (input$testing) {cat(" and input$ss_choose_method selected is ", input$ss_choose_method, "\n")}
  })
  # observe({
  #   if (is.null(input$ss_choose_method) || input$ss_choose_method == "") {
  #     updateRadioButtons(inputId = "ss_choose_method", selected = input$default_ss_choose_method)
  #   }
  # })

  # keep track of currently used method of site selection
  current_upload_method <- reactive({
    if (input$testing) {
      cat('\nThe input$ss_choose_method is ', input$ss_choose_method, '\n')
      if (input$ss_choose_method == "dropdown") {cat(' and input$ss_choose_method_drop is ', input$ss_choose_method_drop, '\n')}
      if (input$ss_choose_method == "upload")   {cat(' and input$ss_choose_method_upload is ', input$ss_choose_method_upload, '\n')}
      }
    x <- switch(
      as.character(input$ss_choose_method),
      'dropdown' = switch(input$ss_choose_method_drop,
                          NAICS =          "NAICS",     # 'NAICS (industry name or code)'
                          EPA_PROGRAM =    "EPA_PROGRAM_sel",
                          SIC =            "SIC" ,
                          MACT =           "MACT",
                          FIPS_PLACE =           "FIPS_PLACE"  # FROM fips picker
      ),
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
      cat(' reactive current_upload_method() now is ', x, '\n')
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

  invalid_alert <- reactiveValues('latlon' = 0, 'NAICS' = 0, 'SIC' = 0, 'MACT' = 0,
                                  'FRS' = 0,
                                  'EPA_PROGRAM_up' = 0, 'EPA_PROGRAM_sel' = 0,
                                  'FIPS' = 0, 'FIPS_PLACE' = 0,
                                  'SHP' = 0)

  ## reactive: SHAPEFILES (from file upload) ####

  num_valid_pts_uploaded <- reactiveValues('SHP' = 0)

  ## initialize SHP file extension error message
  error_message <- reactiveVal(NULL)

  data_up_shp <- reactive({

    req(input$ss_upload_shp)
    infiles <- input$ss_upload_shp$datapath # get path and temp (not original) filename of the uploaded file
    print(infiles)

    infile_ext <- tools::file_ext(infiles)

    required_extensions <- c('shp', 'shx', 'dbf', 'prj', 'json')
    valid_zip <- 'zip'
    has_required_files <- all(required_extensions %in% infile_ext) || any(infile_ext == valid_zip)

    if (EJAM:::global_or_param("use_shapefile_from_any")) {
      ###################################### #
      # newer way ... uses shapefile_from_any() which uses shapefix()
      allowed_extensions <- EJAM:::global_or_param("default_shp_oktypes_1") # c("zip", "gdb", "geojson", "json", "kml", "shp") # see shapefile_from_any()
      if (all(infile_ext %in% allowed_extensions)) {
        error_message(NULL)
        disable_buttons[['SHP']] <- FALSE
      } else {
        error_message(paste("Not an allowed file type. Must be one of these:", paste(allowed_extensions, collapse = ", ")))
        disable_buttons[['SHP']] <- TRUE
      }

      shp <- shapefile_from_any(infiles, cleanit = FALSE, inputname = input$ss_upload_shp$name)

      # if shp contains point features, present message in app
      ## this case is not caught by shapefile_from_any currently - but could use shapefix somehow
      if (any(sf::st_geometry_type(shp) == "POINT")) {
        shp <- NULL
        disable_buttons[['SHP']] <- TRUE
        shiny::validate("Shape file must be of polygon geometry.")
      }

      if (!is.null(attr(shp, "validate_errmsg")))            {shiny::validate(validate_errmsg)}
      if (!is.null(attr(shp, "disable_buttons_SHP")))        {disable_buttons[['SHP']]        <- attr(shp, "disable_buttons_SHP")}
      if (!is.null(attr(shp, "num_valid_pts_uploaded_SHP"))) {num_valid_pts_uploaded[['SHP']] <- attr(shp, "num_valid_pts_uploaded_SHP")}
      if (!is.null(attr(shp, "invalid_alert_SHP")))          {invalid_alert[['SHP']]          <- attr(shp, "invalid_alert_SHP")}
      #if (!is.null(attr(shp, "an_map_text_shp")))            {an_map_text_shp[['SHP']]            <- attr(shp, "an_map_text_shp")}

      shp

    } else {
      ###################################### #
      # older way

      required_extensions <- c('shp', 'shx', 'dbf', 'prj')
      valid_zip <- 'zip'
      has_required_files <- all(required_extensions %in% infile_ext) || any(infile_ext == valid_zip)

      if (!has_required_files) {
        missing_files <- required_extensions[!required_extensions %in% infile_ext]
        error_message(paste("Missing required file types:", paste(missing_files, collapse = ", ")))
        disable_buttons[['SHP']] <- TRUE
      } else {
        error_message(NULL)
        disable_buttons[['SHP']] <- FALSE
      }

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
        sf::st_geometry(shp) <- "geometry"
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

        errmsg    = 'No shapes found in file uploaded.'
        placetype = 'SHP'

        invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
        an_map_text_pts[[placetype]] <- NULL  # hide count of uploaded sites
        disable_buttons[[placetype]] <- TRUE
        shiny::validate(errmsg)

      }
      disable_buttons[['SHP']] <- FALSE                                                 # a reactiveValues object
      shp_proj$valid <- shp_is_valid
      shp_proj <- cbind(ejam_uniq_id = 1:nrow(shp_proj), shp_proj)   #  UNIQUE ID HERE
      shp_proj$invalid_msg <- NA
      shp_proj$invalid_msg[shp_proj$valid == F] <- shp_valid_check$reason[shp_proj$valid == F]
      shp_proj$invalid_msg[is.na(shp_proj$geometry)] <- 'bad geometry'
      class(shp_proj) <- c(class(shp_proj), 'data.table')
      shp_proj
    } # end of old way of reading shp, replaced by shapefile_from_any() that uses shapefix()
    ###################################### #

  }) # END OF SHAPEFILE UPLOAD

  ## show error message when any SHP file extensions are missing/invalid
  output$error_message <- renderText({
    if (!is.null(error_message())) {
      error_message()
    }
  })
  #############################################################################  #

  ### *** note: repeated file reading code below could be replaced by  sitepoints_from_any() ####
  #### #
  ## this part could be replaced each time it happens, by the function sitepoints_from_any, like this:
  # sitepoints <- data.table(
  #   REGISTRY_ID = sitepoints$REGISTRY_ID,
  #   sitepoints_from_any(sitepoints, invalid_msg_table = TRUE)
  # )
  # 'bad lat/lon coordinates'
  # sitepoints$invalid_msg[sitepoints$REGISTRY_ID == 'NA'] <- 'bad REGISTRY_ID'
  ###
  ###  which does latlon_infer() and latlon_as.numeric() and latlon_is.valid() and adds ejam_uniq_id
  ###  and returns data.table of lat,lon,valid,invalid_msg,ejam_uniq_id
  ###  but does not note bad REGISTRY_ID, and returns blank not NA for invalid_msg if OK,
  ###  and says "latlon missing" not "bad lat/lon coordinates"

  #############################################################################  #   #############################################################################  #
  #############################################################################  #   #############################################################################  #

  ## reactive: latlon (typed in on-screen via MODULE) ####
  ## see also  EJAM/R/mod_dataentry_EXAMPLE.R

  ## -------------------  DISABLED UNTIL FINISHED/FIXED  *** -------------------  ##
  ##
  ####      IDEA IS TO LET USER TYPE FROM SCRATCH LAT LONS, AND
  ####      ALSO MAYBE EDIT LATLONS ALREADY UPLOADED FROM A FILE
  # #
  # # Use a default initial template of lat lon values table ready for user to type into
  # # and then the module updates that reactive_data1 object as the user types
  # latlon_template <- data.table(lat = 0, lon = 0, sitenumber = 1, sitename = "")  # default_points_shown_at_startup[1:2, ] #  testpoints_5[1:2, ]
  # reactive_data1 <-  reactiveVal(latlon_template)
  # ## or... try something like this:   Try to pass to module as param the last uploaded pts() ?
  # observe(
  #   # if data_up_latlon() gets updated, then also update this reactive for use in the edited module
  #   reactive_data1(data_up_latlon())
  # )
  #
  # MODULE_SERVER_latlontypedin(id = "pts_entry_table1", reactdat = reactive_data1) # pass points table that is reactive_data1(), but must pass it with NO parens
  #
  ## If a module needs to use any reactive expressions, the outer function should take the reactive expression as a parameter.
  ## If a module needs to access an input that isn’t part of the module, the
  ##   containing app should pass the input value wrapped in a reactive expression (i.e. reactive(...)):
  ##   myModule("myModule1", reactive(input$checkbox1))
  ## If a module wants to return reactive expressions to the calling app, then return a list of reactive expressions from the function.
  #
  # #   )
  # # }) %>%
  # #   bindEvent(input$latlontypedin_submit_button) # (updates only when the "Done entering points" button is pressed)
  #
  #   data_typedin_latlon <- reactive({
  #     #   ## wait for typed in data to be submitted, then return cleaned lat lon table data.frame, as data_typedin_latlon() which eventually becomes data_uploaded()
  #       req(reactive_data1() )
  #     ext <- reactive_data1()  # NEED TO TEST THAT THIS IS ACTUALLY THE USER-EDITED OUTPUT OF THE MODULE   # ss_typedin_latlon()
  #     #   # ext <- data.frame( sitenumber = 1, lat = 0, lon = 0) # dummy data for testing
  #     ###   # another approach, not used:   # ext <- DataEditR::data_edit(latlon_template)
  #     cat("COUNT OF ROWS IN TYPED IN DATA: ", NROW(ext),"\n")
  #     ## Validate the lat lon values. If column names are found in lat/long alias comparison, clean and return the table of lat lon values
  #     if (any(tolower(colnames(ext)) %in% lat_alias) & any(tolower(colnames(ext)) %in% lon_alias)) {
  #       sitepoints <- ext %>%
  #         EJAM::latlon_df_clean() #%>%   # This does latlon_infer() and latlon_as.numeric() and latlon_is.valid()
  #       cat("COUNT OF VALID LAT/LON POINTS IN TYPED IN DATA: ", NROW(sitepoints),"\n")
  #       sitepoints
  #       # returns it here, as the last thing in the reactive
  #     } else {
  #       # warn zero places
  #       ## if not matched, show this message instead
  #       shiny::validate('No lat lon coordinate columns found.')
  #     }
  #   })
  #############################################################################  #   #############################################################################  #
  #############################################################################  #   #############################################################################  #

  #############################################################################  #
  ## reactive: latlon (from table passed to run_app() as param) ####
  ## NOT YET IMPLEMENTED

  data_up_tablepassed_latlon <- reactive({

    ################################# #
    prepare_table_from_run_app <- function(sitepoints, input_max_pts_upload, input_testing) {

      if (all(sitepoints == 0)) {return(NULL)}
      if (NROW(sitepoints) > input_max_pts_upload) {
        if (input_testing) {cat("ROW COUNT TOO HIGH IN FILE THAT SHOULD provide lat lon: ", NROW(sitepoints), "\n")}
        return(NULL)
      }
      if (input_testing) {cat("ROW COUNT IN FILE THAT SHOULD provide lat lon: ", NROW(sitepoints), "\n")}
      ## if column names are found in lat/long alias comparison, process
      if (any(tolower(colnames(sitepoints)) %in% lat_alias) & any(tolower(colnames(sitepoints)) %in% lon_alias)) {
        sitepoints[, ejam_uniq_id := .I]
        data.table::setcolorder(sitepoints, 'ejam_uniq_id')
        sitepoints <- sitepoints %>%
          latlon_df_clean() #%>%   # This does latlon_infer() and latlon_as.numeric() and latlon_is.valid()
        sitepoints$invalid_msg <- NA
        sitepoints$invalid_msg[is.na(sitepoints$lon) | is.na(sitepoints$lat)] <- 'bad lat/lon coordinates'
        if (input$testing) {cat("ROW COUNT after latlon_df_clean(): ", NROW(sitepoints), "\n")}
        return(sitepoints)
      } else {
        return("No coordinate columns found.")
      }
    }
    ################################# #

    sitepoints <- 0 # since never set in global_defaults_*.R, only exists if at all via  get_golem_options()

    sitepoints <- global_or_param("sitepoints")
    if (all(sitepoints == 0)) {
      # warn zero places
      cat('no input sitepoints\n')
      return(NULL) # ?
    } else {
      cat('got input sitepoints\n')
    }

    sitepoints <- prepare_table_from_run_app(sitepoints, input_max_pts_upload = input$max_pts_upload, input_testing = input$testing)

    if (all(is.null(sitepoints))) {

      errmsg    = paste0('Max allowed points is ', as.character(input$max_pts_upload))
      placetype = 'latlon'

      invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
      an_map_text_pts[[placetype]] <- NULL # hide count of uploaded sites
      disable_buttons[[placetype]] <- TRUE
      shiny::validate(errmsg)

    }
    if (all(sitepoints == "No coordinate columns found.")) {

      errmsg    = 'No coordinate columns found.'
      placetype = 'latlon'

      invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
      an_map_text_pts[[placetype]] <- NULL # hide count of uploaded sites
      disable_buttons[[placetype]] <- TRUE
      shiny::validate(errmsg)

    }
    # ok
    disable_buttons[['latlon']] <- FALSE
  })

  #############################################################################  #
  ## reactive: latlon (from file upload) ####

  data_up_latlon <- reactive({

    ## wait for file to be uploaded
    req(input$ss_upload_latlon)

    ## if acceptable file type, read in; if not, send warning text
    input_file_path <- input$ss_upload_latlon$datapath
    # ideally would quickly check file size here before actually trying to read the entire file in case it is > cap.

    ## this part could be replaced each time it happens, by the function sitepoints_from_any

    sitepoints <- as.data.table(read_csv_or_xl(fname = input_file_path))

    # DO NOT USE THE UPLOAD IF IT HAS MORE THAN MAX POINTS ALLOWED FOR UPLOAD
    #
    if (NROW(sitepoints) > input$max_pts_upload) {

      cat("ROW COUNT TOO HIGH IN FILE THAT SHOULD provide lat lon: ", NROW(sitepoints), "\n")

      errmsg    = paste0('Max allowed upload of points is ', as.character(input$max_pts_upload))
      placetype = 'latlon'

      invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
      an_map_text_pts[[placetype]] <- NULL # hide count of uploaded sites
      disable_buttons[[placetype]] <- TRUE
      shiny::validate(errmsg)

    } else {

      cat("ROW COUNT IN FILE THAT SHOULD provide lat lon: ", NROW(sitepoints), "\n")
      ## if column names are found in lat/long alias comparison, process
      if (any(tolower(colnames(sitepoints)) %in% lat_alias) & any(tolower(colnames(sitepoints)) %in% lon_alias)) {
        sitepoints[, ejam_uniq_id := .I]
        data.table::setcolorder(sitepoints, 'ejam_uniq_id')
        sitepoints <- sitepoints %>%
          latlon_df_clean(invalid_msg_table = TRUE) #%>%   # This does latlon_infer() and latlon_as.numeric() and latlon_is.valid()
        sitepoints$invalid_msg <- NA
        sitepoints$invalid_msg[is.na(sitepoints$lon) | is.na(sitepoints$lat)] <- 'bad lat/lon coordinates'
        cat("ROW COUNT after latlon_df_clean(): ", NROW(sitepoints), "\n")
        disable_buttons[['latlon']] <- FALSE
        #print(sum(sitepoints$valid))
        invalid_alert[['latlon']] <- sum(!sitepoints$valid)
        sitepoints
      } else {

        errmsg    = 'No coordinate columns found.'
        placetype = 'latlon'

        invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
        an_map_text_pts[[placetype]] <- NULL # hide count of uploaded sites
        disable_buttons[[placetype]] <- TRUE
        shiny::validate(errmsg)

      }
    }
  })

  #############################################################################  #
  ## reactive: latlon by FRS registry IDs (from file upload) ####

  data_up_frs <- reactive({

    ## wait for file to be uploaded
    req(input$ss_upload_frs)
    placetype = 'FRS'

    input_file_path <- input$ss_upload_frs$datapath
    ## if acceptable file type, read in; if not, send warning text
    read_frs <- as.data.table(read_csv_or_xl(fname = input_file_path))
    # returns a data.frame
    cat("ROW COUNT IN FILE THAT SHOULD provide FRS REGISTRY_ID: ", NROW(read_frs), "\n")

    if (frs_is_valid(read_frs)) {

      if ("regid" %in% colnames(read_frs) & !("REGISTRY_ID" %in% colnames(read_frs))) {
        colnames(read_frs) <- gsub("regid", "REGISTRY_ID", colnames(read_frs))
      }
      #converts registry id to character if not already in that class ( frs registry ids are character)
      if (('REGISTRY_ID' %in% colnames(read_frs)) & (class(read_frs$REGISTRY_ID) != "character")) {
        read_frs$REGISTRY_ID = as.character(read_frs$REGISTRY_ID)
      }

      ## this part could be replaced each time it happens, by the function sitepoints_from_any

      # convert registry ids to latlon coordinates
      sitepoints <- dplyr::left_join(read_frs, frs_from_regid(read_frs$REGISTRY_ID))

      ##  >this part could be replaced each time it happens, by the function sitepoints_from_any
      data.table::setDT(sitepoints)
      sitepoints[, ejam_uniq_id := .I]
      data.table::setcolorder(sitepoints, 'ejam_uniq_id')
      sitepoints[, valid := !(REGISTRY_ID == 'NA' | is.na(lon) | is.na(lat))]
      sitepoints$invalid_msg <- NA
      sitepoints$invalid_msg[is.na(sitepoints$lon) | is.na(sitepoints$lat)] <- 'bad lat/lon coordinates'
      sitepoints$invalid_msg[sitepoints$REGISTRY_ID == 'NA'] <- 'bad REGISTRY_ID'
      #### #
      # dont need to check and warn if zero valid latlon (e.g., none of the registry ids recognized should ever have missing or invalid lat/lon)
      sitepoints

    } else {

      errmsg    = 'Records with invalid Registry IDs'

      invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
      an_map_text_pts[[placetype]] <- NULL # hide count of uploaded sites
      disable_buttons[[placetype]] <- TRUE
      shiny::validate(errmsg)

    }
    ## return merged dataset
    cat("SITE COUNT VIA FRS frs_is_valid() looking for REGISTRY_ID: ", NROW(sitepoints), "\n")
    disable_buttons[[placetype]] <- FALSE
    invalid_alert[[placetype]] <- sitepoints[valid == F, .N]
    sitepoints
  })

  #############################################################################  #
  ## reactive: latlon by NAICS (selected from list) ####

  data_up_naics <- reactive({

    ## check if anything has been selected or entered
    req(isTruthy(input$ss_select_naics))
    placetype = 'NAICS'
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

        sitepoints <- frs_from_naics(inputnaics, childrenForNAICS = add_naics_subcategories)[, .(lat,lon,REGISTRY_ID,PRIMARY_NAME,NAICS)] # xxx

        ## this part could be replaced each time it happens, by the function sitepoints_from_any

        sitepoints[, ejam_uniq_id := .I]
        data.table::setcolorder(sitepoints, 'ejam_uniq_id')
        # print(sitepoints)
        if (rlang::is_empty(sitepoints) | nrow(sitepoints) == 0) {

          errmsg    = 'No valid locations found under this NAICS code.'

          invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
          an_map_text_pts[[placetype]] <- NULL # hide count of uploaded sites
          disable_buttons[[placetype]] <- TRUE
          shiny::validate(errmsg)

        } else
          if (NROW(sitepoints) > input$max_pts_select) {

            cat("ROW COUNT TOO HIGH from selected NAICS code(s): ", NROW(sitepoints), "\n")

            errmsg    = paste0('Max allowed selection of points is ', as.character(input$max_pts_select))

            invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
            an_map_text_pts[[placetype]] <- NULL # hide count of uploaded sites
            disable_buttons[[placetype]] <- TRUE
            shiny::validate(errmsg)

          }
      } else {
        sitepoints <- frs_from_naics(inputnaics, childrenForNAICS = add_naics_subcategories)[, .(lat,lon,REGISTRY_ID,PRIMARY_NAME,NAICS)] # xxx

        # consider using sitepoints_from_any()
        sitepoints[, ejam_uniq_id := .I]
        data.table::setcolorder(sitepoints, 'ejam_uniq_id')
        sitepoints$invalid_msg <- NA
        sitepoints$invalid[is.na(sitepoints$NAICS)] <- 'bad NAICS Code'
        sitepoints$invalid_msg[is.na(sitepoints$lon) | is.na(sitepoints$lat)] <- 'bad lat/lon coordinates'
        # print(sitepoints)
        showNotification('Points submitted successfully!', duration = 1)
      }
    } else {

      errmsg    = 'Invalid NAICS Input'

      invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
      an_map_text_pts[[placetype]] <- NULL # hide count of uploaded sites
      disable_buttons[[placetype]] <- TRUE
      shiny::validate(errmsg)

    }
    cat("SITE COUNT VIA NAICS from frs_from_naics: ", NROW(sitepoints), "\n")
    ## assign final value to data_up_naics reactive variable
    sitepoints <- sitepoints %>% latlon_df_clean(invalid_msg_table = T)
    sitepoints$invalid_msg <- NA
    sitepoints$invalid_msg[is.na(sitepoints$NAICS)] <- 'bad NAICS Code'
    sitepoints$invalid_msg[is.na(sitepoints$lon) | is.na(sitepoints$lat)] <- 'bad lat/lon coordinates'
    cat("SITE COUNT VIA NAICS after latlon_df_clean: ", NROW(sitepoints), "\n")
    disable_buttons[[placetype]] <- FALSE
    invalid_alert[[placetype]] <- sitepoints[valid == F, .N]
    sitepoints
  })

  #############################################################################  #
  ## reactive: latlon by EPA Program IDs (from file upload) ####

  data_up_epa_program_up <- reactive({

    ## wait for file to be uploaded
    req(isTruthy(input$ss_upload_program))
    req(input$ss_upload_program)
    placetype <- 'EPA_PROGRAM_up'
    ## check if file extension is appropriate
    input_file_path <- input$ss_upload_program$datapath
    ## if acceptable file type, read in; if not, send warning text

    read_pgm <- as.data.table(read_csv_or_xl(fname = input_file_path))

    # returns a data.frame
    cat("ROW COUNT IN file that should have program, pgm_sys_id: ", NROW(read_pgm), "\n")
    ## error if no columns provided
    if (!any(c('program','pgm_sys_id') %in% tolower(colnames(read_pgm)))) {

      errmsg    = 'Please add a file with at least these two columns: program, pgm_sys_id \n and possibly these columns as well: REGISTRY_ID,lat,lon'
      # placetype = 'EPA_PROGRAM_up'

      invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
      an_map_text_pts[[placetype]] <- NULL # hide count of uploaded sites
      disable_buttons[[placetype]] <- TRUE
      shiny::validate(errmsg)
    }

    ## convert pgm_sys_id and REGISTRY_ID columns to character before joining
    if (('pgm_sys_id' %in% colnames(read_pgm)) & (class(read_pgm$pgm_sys_id) != "character")) {
      read_pgm$pgm_sys_id = as.character(read_pgm$pgm_sys_id)
    }
    if (('REGISTRY_ID' %in% colnames(read_pgm)) & (class(read_pgm$REGISTRY_ID) != "character")) {
      read_pgm$REGISTRY_ID = as.character(read_pgm$REGISTRY_ID)
    }

    ## add check for program and pgm_sys_id

    ## look for program in list from unique(frs_by_programid$program)

    if (!exists("frs_by_programid")) dataload_dynamic("frs_by_programid")

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

    ## this part could be replaced each time it happens, by the function sitepoints_from_any

    pgm_out[, ejam_uniq_id := .I]
    data.table::setcolorder(pgm_out, 'ejam_uniq_id')

    ## clean so that any invalid latlons become NA
    pgm_out <- pgm_out %>%
      latlon_df_clean(invalid_msg_table = T)

    pgm_out$invalid_msg <- NA
    pgm_out$invalid_msg[pgm_out$REGISTRY_ID == 'NA'] <- 'bad REGISTRY_ID'
    pgm_out$invalid_msg[is.na(pgm_out$lon) | is.na(pgm_out$lat)] <- 'bad lat/lon coordinates'

    ## return output dataset
    cat("SITE COUNT VIA PROGRAM ID: ", NROW(pgm_out), "\n")
    disable_buttons[[placetype]] <- FALSE
    invalid_alert[[placetype]] <- pgm_out[valid == F, .N]
    pgm_out
  })
  #############################################################################  #
  ## reactive: latlon by EPA Program IDs (selected from list) ####

  data_up_epa_program_sel <- reactive({

    if (!exists("frs_by_programid")) dataload_dynamic("frs_by_programid")

    #if (input$ss_choose_method_drop == 'EPA_PROGRAM') {
    req(isTruthy(input$ss_select_program))

    placetype = 'EPA_PROGRAM_sel'
    ## filter frs_by_programid to currently selected program
    pgm_out <- frs_by_programid[ program == input$ss_select_program]

    if (nrow(pgm_out) > 0) {

      if (NROW(pgm_out) > input$max_pts_select) {

        cat("ROW COUNT TOO HIGH from selected EPA program(s): ", NROW(pgm_out), "\n")

        errmsg    = paste0('Max allowed selection of points is ', as.character(input$max_pts_select))
        placetype = 'EPA_PROGRAM_sel'

        invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
        an_map_text_pts[[placetype]] <- NULL # hide count of uploaded sites
        disable_buttons[[placetype]] <- TRUE
        shiny::validate(errmsg)
      }

      ## this part could be replaced each time it happens, by the function sitepoints_from_any

      pgm_out[, ejam_uniq_id := .I]
      data.table::setcolorder(pgm_out, 'ejam_uniq_id')
      ## clean so that any invalid latlons become NA
      pgm_out <- pgm_out %>%
        latlon_df_clean(invalid_msg_table = T)
      pgm_out$invalid_msg <- NA
      pgm_out$invalid_msg[pgm_out$REGISTRY_ID == 'NA'] <- 'bad REGISTRY_ID'
      pgm_out$invalid_msg[is.na(pgm_out$lon) | is.na(pgm_out$lat)] <- 'bad lat/lon coordinates'
      #}
      ## return output dataset
      cat("SITE COUNT VIA PROGRAM ID: ", NROW(pgm_out), "\n")
      disable_buttons[[placetype]] <- FALSE
      invalid_alert[[placetype]] <- pgm_out[valid == F, .N]
      pgm_out
    } else {

      errmsg    = 'No valid locations found under this EPA program.'

      invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
      an_map_text_pts[[placetype]] <- NULL # hide count of uploaded sites
      disable_buttons[[placetype]] <- TRUE
      shiny::validate(errmsg)
    }
  })

  #############################################################################  #
  ## reactive: latlon by SIC (selected from list) ####

  data_up_sic <- reactive({

    ## check if anything has been selected or entered
    req(isTruthy(input$ss_select_sic))
    #req(shiny::isTruthy(input$ss_enter_sic) || shiny::isTruthy(input$ss_select_sic))
    placetype = 'SIC'
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

        sitepoints <- frs_from_sic(inputsic, children = add_sic_subcategories)[, .(lat,lon,REGISTRY_ID,PRIMARY_NAME,SIC)] # xxx
        sitepoints[, `:=`(ejam_uniq_id = .I,
                          valid = !is.na(lon) & !is.na(lat))]
        data.table::setcolorder(sitepoints, 'ejam_uniq_id')
        sitepoints$invalid_msg <- NA
        sitepoints$invalid_msg[is.na(sitepoints$SIC)] <- 'bad SIC Code'
        sitepoints$invalid_msg[is.na(sitepoints$lon) | is.na(sitepoints$lat)] <- 'bad lat/lon coordinates'

        if (rlang::is_empty(sitepoints) | nrow(sitepoints) == 0) {

          errmsg    = 'No valid locations found under this SIC code.'

          invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
          an_map_text_pts[[placetype]] <- NULL # hide count of uploaded sites
          disable_buttons[[placetype]] <- TRUE
          shiny::validate(errmsg)

        }  else if (NROW(sitepoints) > input$max_pts_select) {

          errmsg    = paste0('Max allowed selection of points is ', as.character(input$max_pts_select))

          invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
          an_map_text_pts[[placetype]] <- NULL # hide count of uploaded sites
          disable_buttons[[placetype]] <- TRUE
          shiny::validate(errmsg)

          cat("ROW COUNT TOO HIGH from selected SIC code(s): ", NROW(sitepoints), "\n")
        }
      } else {

        sitepoints <- frs_from_sic(inputsic, children = add_sic_subcategories)[, .(lat,lon,REGISTRY_ID,PRIMARY_NAME,SIC)] # xxx

        ## this part could be replaced each time it happens, by the function sitepoints_from_any

        sitepoints[, `:=`(ejam_uniq_id = .I,
                          valid = !is.na(lon) & !is.na(lat))]
        data.table::setcolorder(sitepoints, 'ejam_uniq_id')
        sitepoints$invalid_msg <- NA
        sitepoints$invalid_msg[is.na(sitepoints$SIC)] <- 'bad SIC Code'
        sitepoints$invalid_msg[is.na(sitepoints$lon) | is.na(sitepoints$lat)] <- 'bad lat/lon coordinates'
        #invalid_alert[[placetype]] <- sitepoints[valid == F, .N]
        showNotification('Points submitted successfully!', duration = 1)
      }
    } else {

      errmsg    = 'Invalid SIC Input'

      invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
      an_map_text_pts[[placetype]] <- NULL # hide count of uploaded sites
      disable_buttons[[placetype]] <- TRUE
      shiny::validate(errmsg)

    }
    cat("SITE COUNT VIA SIC: ", NROW(sitepoints), "\n")
    ## assign final value to data_up_sic reactive variable
    disable_buttons[['SIC']] <- FALSE
    invalid_alert[[placetype]] <- sitepoints[valid == F, .N]
    return(sitepoints)
  })
  ################################################################################### #

  ## reactive: places by FIPS (selected from list) via module fipspicker ####

  # data_fips_place() is a reactive reactiveVal() sort of like the reactive() data_up_fips().
  #  It is updated by an observer only when input$fipspicker_done_button is clicked, not in response to any uploaded file.
  #  It gets its value from a shiny module's output, not a pulldown menu in the main app.
  #  So unlike data_up_fips() that updates based on file upload button input$ss_upload_fips,
  #  or that input$ that updates based on dropdown menu selections,
  #  data_fips_place does not need an upload button nor an input$ that stores dropdown menu selections like NAICS etc. uses.

  data_fips_place = reactiveVal()

  # fips_dt_react() as returned from the module happens to be a reactive
  fips_dt_react <- fipspicker_module_server(id = "pickermoduleid",
                                            testing_this_module = testing_this_module,
                                            reactdat = reactive("") # can pass reactive param
  )

  observe({ # only when Done button clicked

    req(fips_dt_react())
    fips_dt <- fips_dt_react()

    fips_dt <- data.table::as.data.table(fips_dt)
    placetype <- 'FIPS_PLACE'
    ########################################## #

    ## copy of code used in data_up_fips()

    cat("COUNT OF ROWS IN FIPS FILE: ", NROW(fips_dt),"\n")
    fips_vec <- fips_from_table(fips_table = fips_dt, addleadzeroes = TRUE, inshiny = TRUE)
    ftypeUpload <- fipstype(fips_vec)
    typesUpload <- c('blockgroup', 'tract', 'city', 'county', 'state')

    if (is.null(fips_vec)) {
      # fips_alias <- c('FIPS','fips','fips_code','fipscode','Fips','statefips','countyfips', 'ST_FIPS','st_fips','ST_FIPS','st_fips', 'FIPS.ST', 'FIPS.COUNTY', 'FIPS.TRACT')
      # see fips_from_table(), fixnames_aliases(), and fixcolnames_infer()
      errmsg    = paste0('No FIPS column found. Please use "FIPS" or a synonym.')
      invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
      an_map_text_pts[[placetype]] <- NULL
      disable_buttons[[placetype]] <- TRUE
      shiny::validate(errmsg)
    } else if (length(intersect(ftypeUpload, typesUpload)) > 1) {
      errmsg    = paste0('This dataset contains more than one type of FIPS code. Analysis can only be ran on datasets with one type of FIPS codes.')
      invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
      an_map_text_pts[[placetype]] <- NULL
      disable_buttons[[placetype]] <- TRUE
      shiny::validate(errmsg)
    } else {
      disable_buttons[['FIPS']] <- FALSE
      cat("COUNT OF FIPS via fips_from_table(): ", length(fips_vec), '\n')
      # now let ejamit() do the rest for the FIPS case
      # fips_vec # ??? this line was here but should have no effect?
    }

    fips_vec <- fips_lead_zero(fips_vec)
    fips_is_valid <- fips_valid(fips_vec)

    if (sum(fips_is_valid) > 0) {
      numna <- sum(!fips_is_valid)
      num_notna <- length(fips_vec) - sum(!fips_is_valid)
      #num_valid_pts_uploaded[['FIPS']] <- num_notna
      invalid_alert[[placetype]] <- numna # this updates the value of the reactive invalid_alert()
      cat("Number of FIPS codes:  "); cat(length(fips_vec), 'total,', num_notna, 'valid,', numna, ' invalid \n')
      # (fips_vec)
    } else {
      errmsg    = 'No valid FIPS codes found in this file.'
      invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
      an_map_text_pts[[placetype]] <- NULL
      disable_buttons[[placetype]] <- TRUE
      shiny::validate(errmsg)
      fips_vec <- NULL  # ??? ***
    }
    ########################################## #

    # fips_vec <- fips_clean_shiny_input(fips_dt = fips_dt, placetype = placetype)
    #
    ## to do this via function would require handling the reactive values correctly and the shiny::validate(errmsg) too
    ## so instead of via function, for now copied same code as used by data_up_fips()
    ## reactiveVal() objects in calling envt whose values may be updated here: invalid_alert, an_map_text_pts, disable_buttons,
    ## and it sometimes does shiny::validate(errmsg)


    ## update the reactive object data_fips_place() with this new vector of fips
    data_fips_place(fips_vec)

  })  %>% shiny::bindEvent(input$fipspicker_done_button)
  # END OF FIPS_PLACES fipspicker
  ################################################################################### #


  ## reactive: places by FIPS (from file upload) ####

  data_up_fips <- reactive({

    req(input$ss_upload_fips)
    input_file_path <- input$ss_upload_fips$datapath
    ## if acceptable file type, read in; if not, send warning text
    fips_dt <- as.data.table(read_csv_or_xl(fname = input_file_path))
    placetype <- 'FIPS'
    ########################################## #

    # fips_clean_shiny_input <- function(fips_dt, placetype) {

    cat("COUNT OF ROWS IN FIPS FILE: ", NROW(fips_dt),"\n")
    fips_vec <- fips_from_table(fips_table = fips_dt, addleadzeroes = TRUE, inshiny = TRUE)
    ftypeUpload <- fipstype(fips_vec)
    typesUpload <- c('blockgroup', 'tract', 'city', 'county', 'state')

    if (is.null(fips_vec)) {
      # fips_alias <- c('FIPS','fips','fips_code','fipscode','Fips','statefips','countyfips', 'ST_FIPS','st_fips','ST_FIPS','st_fips', 'FIPS.ST', 'FIPS.COUNTY', 'FIPS.TRACT')
      # see fips_from_table(), fixnames_aliases(), and fixcolnames_infer()
      errmsg    = paste0('No FIPS column found. Please use "FIPS" or a synonym.')
      invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
      an_map_text_pts[[placetype]] <- NULL
      disable_buttons[[placetype]] <- TRUE
      shiny::validate(errmsg)
    } else if (length(intersect(ftypeUpload, typesUpload)) > 1) {
      errmsg    = paste0('This dataset contains more than one type of FIPS code. Analysis can only be ran on datasets with one type of FIPS codes.')
      invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
      an_map_text_pts[[placetype]] <- NULL
      disable_buttons[[placetype]] <- TRUE
      shiny::validate(errmsg)
    } else {
      disable_buttons[['FIPS']] <- FALSE
      cat("COUNT OF FIPS via fips_from_table(): ", length(fips_vec), '\n')
      # now let ejamit() do the rest for the FIPS case
      # fips_vec # ??? this line was here but should have no effect?
    }

    fips_vec <- fips_lead_zero(fips_vec)
    fips_is_valid <- fips_valid(fips_vec)

    if (sum(fips_is_valid) > 0) {
      numna <- sum(!fips_is_valid)
      num_notna <- length(fips_vec) - sum(!fips_is_valid)
      #num_valid_pts_uploaded[['FIPS']] <- num_notna
      invalid_alert[[placetype]] <- numna # this updates the value of the reactive invalid_alert()
      cat("Number of FIPS codes:  "); cat(length(fips_vec), 'total,', num_notna, 'valid,', numna, ' invalid \n')
      # (fips_vec)
    } else {
      errmsg    = 'No valid FIPS codes found in this file.'
      invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
      an_map_text_pts[[placetype]] <- NULL
      disable_buttons[[placetype]] <- TRUE
      shiny::validate(errmsg)
      fips_vec <- NULL  # ??? ***
    }
    # return(fips)
    # } ## for now, use copy of code, not use it as a function
    ########################################## #

    # fips_vec <- fips_clean_shiny_input(fips_dt = fips_dt, placetype = placetype)
    #
    ## to do this via function would require handling the reactive values correctly and the shiny::validate(errmsg) too
    ## so instead of via function, for now copied same code as used by data_up_fips()
    ## reactiveVal() objects in calling envt whose values may be updated here: invalid_alert, an_map_text_pts, disable_buttons,
    ## and it sometimes does shiny::validate(errmsg)


    fips_vec
    ## update the reactive object data_up_fips() with this new vector of fips

  }) # END OF FIPS UPLOAD
  ################################################################################### #


  ## reactive: latlon by MACT subpart (selected from list) ####

  data_up_mact <- reactive({

    req(isTruthy(input$ss_select_mact))
    placetype = 'MACT'
    if (!exists("frs_by_mact")) dataload_dynamic("frs_by_mact")

    ## filter frs_by_mact to currently selected subpart
    mact_out <- frs_by_mact[subpart == input$ss_select_mact]
    cat("COUNT OF FACILITIES BY MACT: ", NROW(mact_out), "\n")
    ## remove any facilities with invalid latlons before returning
    #mact_out <- mact_out[!is.na(lat) & !is.na(lon),]
    cat("COUNT OF FACILITIES BY MACT with lat lon values: ", NROW(mact_out), "\n")
    if (all(is.na(mact_out$lat)) & all(is.na(mact_out$lon))) {

      errmsg    = 'No valid locations found under this MACT subpart'

      invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
      an_map_text_pts[[placetype]] <- NULL # hide count of uploaded sites
      disable_buttons[[placetype]] <- TRUE
      shiny::validate(errmsg)

      # invalid_alert[['MACT']] <- nrow(mact_out)

    } else if (NROW(mact_out) > input$max_pts_select) {

      cat("ROW COUNT TOO HIGH from selected MACT subpart(s): ", NROW(mact_out), "\n")

      errmsg    = paste0('Max allowed selection of points is ', as.character(input$max_pts_select))
      placetype = 'latlon'

      invalid_alert[[  placetype]] <- 0    # hide warning of invalid sites
      an_map_text_pts[[placetype]] <- NULL # hide count of uploaded sites
      disable_buttons[[placetype]] <- TRUE
      shiny::validate(errmsg)

    } else {
      disable_buttons[[placetype]] <- FALSE
      # mact_out[, `:=`(ejam_uniq_id = .I,
      #                 valid = !is.na(lon) & !is.na(lat))]
      ## this part could be replaced each time it happens, by the function sitepoints_from_any
      mact_out$ejam_uniq_id <- 1:nrow(mact_out)
      mact_out$valid <- !is.na(mact_out$lon) & !is.na(mact_out$lat)
      data.table::setcolorder(mact_out, 'ejam_uniq_id')
      mact_out$invalid_msg <- NA
      mact_out$invalid_msg[is.na(mact_out$lon) | is.na(mact_out$lat)] <- 'bad lat/lon coordinates'
      invalid_alert[[  placetype]] <- mact_out[valid == F, .N]
      ## return output dataset
      mact_out
    }
  })
  ##################################################### #

  # PLACES ARE READY TO ANALYZE ####

  ## data_uploaded() reactive holds the points or shapefiles or fips ####

  data_uploaded <- reactive({

    ## if >1 upload method used, use the one currently indicated by radio button ss_choose_method

    if        (current_upload_method() == 'latlon'         ) {data_up_latlon()
   #} else if (current_upload_method() == 'latlontypedin'  ) {data_typedin_latlon() # enable if implemented/ready ***
   #} else if (current_upload_method() == 'ECHO'           ) {data_up_echo()        # enable if implemented/ready ***
    } else if (current_upload_method() == 'FRS'            ) {data_up_frs()

    } else if (current_upload_method() == 'EPA_PROGRAM_sel') {data_up_epa_program_sel()
    } else if (current_upload_method() == 'EPA_PROGRAM_up' ) {data_up_epa_program_up()
    } else if (current_upload_method() == 'NAICS'          ) {data_up_naics()
    } else if (current_upload_method() == 'SIC'            ) {data_up_sic()
    } else if (current_upload_method() == 'MACT'           ) {data_up_mact()
    } else if (current_upload_method() == 'FIPS'           ) {data_up_fips()    # fips vector to use in ejamit(fips = xyz), not lat/lon values
    } else if (current_upload_method() == 'FIPS_PLACE'     ) {data_fips_place() # fips vector to use in ejamit(fips = xyz), not lat/lon values
    } else if (current_upload_method() == 'SHP'            ) {data_up_shp()     # spatial data.frame to use in ejamit(shapefile = xyz) finding blocks in polygons,     not lat/lon values
    }
  })
  #############################################################################  #

  disable_buttons <- reactiveValues('latlon' = TRUE,
                                    # 'latlontypedin' = TRUE,
                                    # 'ECHO' = TRUE,
                                    'FRS' = TRUE,

                                    'EPA_PROGRAM_sel' = TRUE,
                                    'EPA_PROGRAM_up' = TRUE,
                                    'NAICS' = TRUE,
                                    'SIC' = TRUE,
                                    'MACT' = TRUE,

                                    'FIPS' = TRUE,
                                    'FIPS_PLACE' = TRUE,
                                    'SHP' = TRUE
  )

  ## disable run, hide preview button, until an upload. input$ss_upload_latlon or whatever and current_upload_method() say there is data uploaded; then enable and show
  observe({

    if (disable_buttons[[current_upload_method()]]) {

      shinyjs::disable(id = 'bt_get_results'); shinyjs::hide(id = 'show_data_preview')
      invalid_alert[[current_upload_method()]] <- 0
      an_map_text_pts[[current_upload_method()]] <- NULL

    } else {
      shinyjs::enable( id = 'bt_get_results'); shinyjs::show(id = 'show_data_preview')
    }

    ## add warning and disable button if radius is set to 0 for points

    if (!(current_upload_method() %in% c('FIPS', 'FIPS_PLACE', 'SHP'))) {

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
    } else {#else if (disable_buttons[[current_upload_method()]] == FALSE) {
      removeNotification(id = 'radius_warning', session = session)
    }
    if (NROW(data_uploaded()) > input$max_pts_run) {
      shinyjs::disable(id = 'bt_get_results')
      showNotification(id = 'max_pts_warning', session = session,
                       duration = NULL, type = 'error', closeButton = F,
                       paste0('Too many sites provided. This web app currently caps analyses at ', input$max_pts_run,' sites.'))
    } else {
      shinyjs::enable(id = 'bt_get_results')
      removeNotification(id = 'max_pts_warning', session = session)
    }
    if (input$testing) {
      cat("Enabled/disabled button to get results, and showed/hid data preview based on current_upload_method() ==  ", current_upload_method(), "\n")
    }
  })
  #############################################################################  #
  #. ####
  # ______ VIEW SITES (uploaded; not yet processed) ####
  #. ####

  output$invalid_sites_alert2 <- renderUI({

    req(invalid_alert[[current_upload_method()]])
    build_invalid_alert(
      invalid_count = invalid_alert[[current_upload_method()]],
      method = input$ss_choose_method
    )
  })

  ## initialize reactive for count of uploaded places
  #
  ## ideally recode to be flexible and depend on  default_choices_for_type_of_site_category, default_choices_for_type_of_site_upload
  ## BUT EPA_PROGRAM_up and EPA_PROGRAM_sel were used due to overlap! and FIPS will overlap now too
  # intersect(as.vector(default_choices_for_type_of_site_category), as.vector(default_choices_for_type_of_site_upload))
  ## otherwise this could work:
  # an_map_text_pts <-  reactiveValues()
  # intersect(as.vector(default_choices_for_type_of_site_category), as.vector(default_choices_for_type_of_site_upload))
  # x = union(as.vector(default_choices_for_type_of_site_category), as.vector(default_choices_for_type_of_site_upload))
  # for (i in 1:length(x)) {an_map_text_pts[[x[i]]] <- NULL }

  an_map_text_pts <-  reactiveValues('latlon' = NULL,
                                     # 'latlontypedin' = NULL,
                                     # 'ECHO' = NULL,
                                     'FRS' = NULL,
                                     'EPA_PROGRAM_up' = NULL,
                                     'EPA_PROGRAM_sel' = NULL,
                                     'NAICS' = NULL,
                                     'SIC' = NULL,
                                     'MACT' = NULL,
                                     'FIPS' = NULL,
                                     'FIPS_PLACE' = NULL,
                                     'SHP' = NULL
  )

  observe({
    req(data_uploaded())
    an_map_text_pts[[current_upload_method()]] <- build_sitecount_text(current_upload_method(), data_uploaded())
  })

  ## Which points are valid?(and  how many; warn if 0) ####

  output$an_map_text <- renderUI({

    req(data_uploaded())
    HTML(an_map_text_pts[[current_upload_method()]])
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
    } else if (current_upload_method() %in% c('FIPS', 'FIPS_PLACE')) {
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
                                   )
                    ), # end options
                    escape = FALSE) # escape = FALSE may add security issue but makes links clickable in table
    })
  ######################################  #

  ## use external download buttons for preview data
  ## this allows loading the table on the server-side which improves speed and avoids
  ## crashes with larger datasets
  output$download_preview_data_xl <- downloadHandler(
    filename = 'epa_raw_data_download.xlsx',
    content = function(file) {
      writexl::write_xlsx(data_preview(), file)
    }
  )
  output$download_preview_data_csv <- downloadHandler(
    filename = 'epa_raw_data_download.csv',
    content = function(file) {
      ## shapefile is not of type data.table
      if (current_upload_method() == 'SHP') {
        readr::write_csv(data_preview(), file, append = F)
      } else {
        data.table::fwrite(data_preview(), file, append = F)
      }
    }
  )
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
        step = EJAM:::global_or_param("stepradius"),
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
    if (current_upload_method() %in% c('FIPS', 'FIPS_PLACE')) {
      shinyjs::disable(id = 'bt_rad_buff')
    } else {
      shinyjs::enable(id = 'bt_rad_buff')
    }
  })

  ## create different initial (and minimum?) radius values for each site selection type
  ### input$default_miles is set in advanced tab by global_defaults_*.R and then based on user input if any
  ### or via e.g., radius=3.1 or radius_shapefile=1 param that can be provided to run_app()

  current_slider_min <- list(
    # constants defined in global_defaults_*.R
    'latlon' =  EJAM:::global_or_param("minradius"),
    'NAICS' =  EJAM:::global_or_param("minradius"),
    'SIC' =  EJAM:::global_or_param("minradius"),
    'FRS' =  EJAM:::global_or_param("minradius"),
    'MACT' =  EJAM:::global_or_param("minradius"),
    'EPA_PROGRAM_up' =  EJAM:::global_or_param("minradius"),
    'EPA_PROGRAM_sel' =  EJAM:::global_or_param("minradius"),
    'FIPS' = EJAM:::global_or_param("minradius_shapefile"),
    'FIPS_PLACE' = EJAM:::global_or_param("minradius_shapefile"),
    'SHP' = EJAM:::global_or_param("minradius_shapefile") # but disabled for FIPS
  )
  current_slider_val <- reactiveValues(
    # these are just placeholders that should get updated at startup, though.
    'latlon' = 1,
    'NAICS' = 1,
    'SIC' = 1,
    'FRS' = 1,
    'MACT' = 1,
    'EPA_PROGRAM_up' = 1,
    'EPA_PROGRAM_sel' = 1,
    'FIPS' = EJAM:::global_or_param("minradius_shapefile"),
    'FIPS_PLACE' = EJAM:::global_or_param("minradius_shapefile"),
    'SHP' = EJAM:::global_or_param("minradius_shapefile")  # but disabled for FIPS
  )

  ## record radius at time of analysis
  submitted_radius_val <- reactiveVal(NULL)

  # set/update based on advanced tab set by global_defaults_*.R and then might be changed by a user

  observe({
    # Sanitize the input: Convert to numeric or set a default value
    sanitized_miles <- as.numeric(input$default_miles)
    # Handle cases where the input cannot be converted to a numeric value
    if (is.na(sanitized_miles)) {
      sanitized_miles <- 0
    }
    these <- c(
      'latlon',
      'FRS',
      'EPA_PROGRAM_up',
      'EPA_PROGRAM_sel',
      'NAICS',
      'SIC',
      'MACT'
      # note   FIPS and SHP types handled separately
    )
    for (this in these) {
      current_slider_val[[this]] <- sanitized_miles
    }
  }) %>% bindEvent(input$default_miles)

  # set/update based on advanced tab set by global_defaults_*.R and then might be changed by a user

  observe({
    these <- c("FIPS", "FIPS_PLACE", "SHP") # but disabled for FIPS
    for (this in these) {current_slider_val[[this]] <- input$default_miles_shapefile}
  }) %>% bindEvent(input$default_miles_shapefile)

  ## update/restore previous radius (and reset the min value) when site selection type changes/changes back
  observe({
    updateSliderInput(session = session, inputId = 'bt_rad_buff',
                      value = current_slider_val[[current_upload_method()]])
  }) %>% bindEvent(current_upload_method())

  ## update stored radius when slider changes
  # except this would initially save the one current radius (default) as the current value for all methods including shapefiles which we dont want to do
  observe({
    current_slider_val[[current_upload_method()]] <- sanitized_bt_rad_buff()
  }) %>% bindEvent(sanitized_bt_rad_buff())

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
    #
    #
    #
    #
    #

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
        leaflet::leaflet() %>%
          leaflet::addTiles() %>%
          leaflet::fitBounds(
            lng1 = as.numeric(bbox[1]), lng2 = as.numeric(bbox[3]),
            lat1 = as.numeric(bbox[2]), lat2 = as.numeric(bbox[4])
          )
        #try(map_shapes_leaflet(shapes = data_uploaded() %>% st_zm())) # popups?
      }

    } else if (current_upload_method() %in% c('FIPS', 'FIPS_PLACE')) {

      ## ---------------------------------------------- __MAP FIPS CENSUS UNITS uploaded ####
      req(data_uploaded())

      canmap <- TRUE
      max_pts <- input$max_shapes_map
      if (NROW(data_uploaded()) > max_pts) {
        warning(paste0('Too many FIPS polygons (> ', prettyNum(max_pts, big.mark = ','),') for map to show'))
        validate(paste0('Too many FIPS polygons (> ', prettyNum(max_pts, big.mark = ','),') for map to show'))

        # add code here to show just a sample, subset up to max allowed ***

        canmap <- FALSE
      } else {

        ### **Download FIPS Boundaries via API ---------------------------- ####

        FTYPES <- fipstype(data_uploaded())

        #Old line had error where it checked all(FTYPES) in xyz which doesn't make sense
        #This is the proper check to make sure all FTYPES are in one of the 5 categories, all in this case makes sure they're all true
        if (all(FTYPES %in% c("state", "county", "city", "tract", "blockgroup"))) {
          shps <- tryCatch({
            shapes_from_fips(data_uploaded())
          }, error = function(e) {
            canmap <- FALSE
            validate(need(FALSE, e$message))
          })
        } else {
          warning('cannot map FIPS types other than states, counties, cities/CDPs, tracts, or blockgroups')
          validate("cannot map FIPS types other than states, counties, cities/CDPs, tracts, or blockgroups")
          canmap <- FALSE
        }
        if (canmap) {
          #Check for and fix invalid geometries, (ie duplicate verticies)
          if (!is.null(shps) && any(!sf::st_is_valid(shps))) {
            shps <- sf::st_make_valid(shps)
          }
          xyz <- try(map_shapes_leaflet(shps), silent = TRUE)  # popups?
          if (inherits(xyz, "try-error")) {
            canmap <- FALSE
            validate("problem with map_shapes_leaflet() function")
            } else {xyz}
        }
      }

    } else {
      ## ---------------------------------------------- __MAP LAT LON points uploaded ####
      d_upload <- data_uploaded()
      max_pts <- input$max_pts_map # was the fixed max_pts_map

      if (nrow(data_uploaded()) > max_pts) { # would have already been stopped probably
        ## Max allowed points was exceeded! see code in ejscreenapi that handled that case using  input$max_pts_map
        if (nrow(data_uploaded) > input$max_pts_run) {
          validate(paste0('Too many points (> ', prettyNum(max_pts, big.mark = ','),
                          ') uploaded for map to be displayed'))
        } else {
          validate(paste0('Too many points (> ', prettyNum(max_pts, big.mark = ','),
                          ') uploaded to map all, so only showing a random sample, but you can still run the analysis on all.'))

          # show just a subset up to max_pts allowed ***
          sampling_of_rows <- sample(1:nrow(data_uploaded()[!is.na(data_uploaded()$lat) &
                                                              !is.na(data_uploaded()$lon),]), max_pts)
          data_tomap <- data_uploaded()[!is.na(data_uploaded()$lat) &
                                          !is.na(data_uploaded()$lon),][sampling_of_rows, ]
          ## If more than one valid point...
          if (nrow(data_tomap) > 1) {
            leaflet::leaflet() %>%
              leaflet::addTiles() %>%
              leaflet::fitBounds(lng1 = min(data_tomap$lon, na.rm = T),
                                 lng2 = max(data_tomap$lon, na.rm = T),
                                 lat1 = min(data_tomap$lat, na.rm = T),
                                 lat2 = max(data_tomap$lat, na.rm = T))
            ## if only one valid point
          } else {
            leaflet::leaflet() %>%
              leaflet::addTiles() %>%
              leaflet::setView(lat = data_tomap$lat, lng = data_tomap$lon, zoom = 10)
          }
        }

      } else {

        ## If more than one valid point...
        if (sum((
          !is.na(data_uploaded()$lat) &
          !is.na(data_uploaded()$lon)
        )) > 1) {
          leaflet::leaflet() %>%
            leaflet::addTiles() %>%
            leaflet::fitBounds(lng1 = min(data_uploaded()$lon, na.rm = T),
                               lng2 = max(data_uploaded()$lon, na.rm = T),
                               lat1 = min(data_uploaded()$lat, na.rm = T),
                               lat2 = max(data_uploaded()$lat, na.rm = T))
          ## if only one valid point
        } else {
          leaflet::leaflet() %>%
            leaflet::addTiles() %>%
            leaflet::setView(lat = data_uploaded()$lat, lng = data_uploaded()$lon, zoom = 10)
        }
      }
    }

  })
  ######################################  #######################################  #

  # __ render map of uploaded  ####

  output$an_leaf_map <- leaflet::renderLeaflet({

    ## check if data has been uploaded yet
    ## make errors silent by default; print below
    m <- try(data_uploaded(), silent = TRUE)
    ## if no data, show empty map
    if (inherits(m, 'try-error')) {
      ## only print non-empty error messages
      error_msg <- attr(m,'condition')$message
      if (error_msg != "") {print(paste0("Error: ", error_msg))}
      # blank US map
      leaflet::leaflet() %>% leaflet::addTiles() %>% leaflet::setView(lat = 39.8283, lng = -98.5795, zoom = 4)
    } else {
      ## try to load the reactive map
      tryCatch({orig_leaf_map()},
               error = function(e) {validate(conditionMessage(e))}
      )
    }
  })

  #############################################################################  #
  # . --------------------------------------------------------------- # ###

  #. ####
  # ______ RUN ANALYSIS  (when button is pressed) ________####
  #. ####

  ## data_processed()  reactive holds results of ejamit()

  observeEvent(input$bt_get_results, {  # (button is pressed)
    submitted_upload_method(current_upload_method())
    submitted_radius_val(current_slider_val[[submitted_upload_method()]])

    showNotification('Processing sites now!', type = 'message', duration = 1)

    ## progress bar setup overall for 3 operations  (getblocksnearby, doaggregate, batch.summarize)
    progress_all <- shiny::Progress$new(min = 0, max = 1)
    progress_all$set(value = 0, message = 'Step 1 of 3', detail = 'Getting nearby census blocks')

    ################################################# #

    if (submitted_upload_method() %in% c('FIPS', 'FIPS_PLACE')) {  # if FIPS, do everything in 1 step right here.

      out <- ejamit(fips = data_uploaded(),              # unlike for SHP or latlon cases, this could include invalid FIPS!
                    radius = 999, # because FIPS analysis
                    maxradius = input$maxradius,
                    avoidorphans = input$avoidorphans,
                    quadtree = localtree,
                    # countcols      = NULL,
                    # wtdmeancols    = NULL,
                    # calculatedcols = NULL,
                    # calctype_maxbg = NULL,
                    # calctype_minbg = NULL,
                    subgroups_type = input$subgroups_type,
                    include_ejindexes   = (input$include_ejindexes == "TRUE" || input$include_ejindexes == TRUE), # it was character not logical because of how input UI done?
                    calculate_ratios = input$calculate_ratios,
                    extra_demog = input$extra_demog,
                    need_proximityscore = FALSE, #input$need_proximityscore, # not relevant for FIPS
                    # infer_sitepoints = FALSE,
                    # need_blockwt = TRUE,
                    # updateProgress = ??? , # not sure this is needed or works here
                    in_shiny = TRUE, # not sure this is needed or works here
                    progress_all = progress_all,
                    # quiet = TRUE,
                    silentinteractive = TRUE,
                    # called_by_ejamit = TRUE, # not sure this is needed or works here
                    testing = input$testing,
                    download_fips_bounds_to_calc_areas = EJAM:::global_or_param("default_download_fips_bounds_to_calc_areas"), # SLOW IF TRUE, defined in global_defaults_*.R
                    thresholds   = list(input$an_thresh_comp1, input$an_thresh_comp2), # thresholds = list(90, 90), # or 80,80
                    threshnames  = list(input$an_threshnames1, input$an_threshnames2), # list(c(names_ej_pctile, names_ej_state_pctile), c(names_ej_supp_pctile, names_ej_supp_state_pctile)),
                    threshgroups = list(sanitized_an_threshgroup1(), sanitized_an_threshgroup2()) # list("EJ-US-or-ST", "Supp-US-or-ST")
      )
      # sitetype is "fips" and
      # now includes area_sqmi columns as output of ejamit(), for fips case, but if download_fips_bounds_to_calc_areas=F, it is NA values

      ## note which FIPS dropped by ejamit() (i.e., in getting step or doagg step) as invalid,
      ## and add those back into results as mostly empty rows (which non-FIPS cases do later)
      if (is.null(out)) {
        validate('No valid blockgroups found matching these FIPS codes.')
      }
      # end of ejamit() for FIPS

      ################################################# #
    } else { # everything other than FIPS code analysis:
      #############################################################################  #

      ## get blocks in POLYGONS / SHAPEFILES ####

      if (submitted_upload_method() == "SHP") {

        rad_buff <- sanitized_bt_rad_buff()

        if (!is.na(rad_buff) && rad_buff > 0) {
          # if (!silentinteractive) {
          cat('Adding buffer around each polygon.\n')
          # }
          shp <- shape_buffered_from_shapefile(
            shapefile = data_uploaded(),
            radius.miles =  rad_buff
          ) # default crs
        } else {
          shp <- data_uploaded()
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

        out <- ejamit(shapefile = shp,
                      radius = sanitized_bt_rad_buff(),
                      maxradius = input$maxradius,
                      avoidorphans = input$avoidorphans,
                      quadtree = localtree,
                      # countcols      = NULL,
                      # wtdmeancols    = NULL,
                      # calculatedcols = NULL,
                      # calctype_maxbg = NULL,
                      # calctype_minbg = NULL,
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
                      silentinteractive = TRUE,
                      # called_by_ejamit = TRUE, # not sure this is needed or works here
                      progress_all = progress_all,
                      testing = input$testing,
                      download_fips_bounds_to_calc_areas = default_download_fips_bounds_to_calc_areas, # SLOW IF TRUE, defined in global.R
                      thresholds   = list(input$an_thresh_comp1, input$an_thresh_comp2), # thresholds = list(90, 90), # or 80,80
                      threshnames  = list(input$an_threshnames1, input$an_threshnames2), # list(c(names_ej_pctile, names_ej_state_pctile), c(names_ej_supp_pctile, names_ej_supp_state_pctile)),
                      threshgroups = list(sanitized_an_threshgroup1(), sanitized_an_threshgroup2()), # list("EJ-US-or-ST", "Supp-US-or-ST")
                      updateProgress_getblocks = updateProgress_getblocks_shp
        )

        ## close getblocks progress bar
        progress_getblocks_shp$close()
      } # end of ejamit() for SHP type
      ################################################# #

      ## get blocks near LAT/LON  POINTS  facilities/latlon # ####

      if (!(submitted_upload_method() %in% c('SHP', 'FIPS', 'FIPS_PLACE'))) {  # if LATITUDE AND LONGITUDE (POINTS), find blocks nearby

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

        out <-  ejamit(sitepoints = data_uploaded(),#d_upload,
                       radius = sanitized_bt_rad_buff(),
                       #quadtree = localtree,
                       # countcols      = NULL,
                       # wtdmeancols    = NULL,
                       # calculatedcols = NULL,
                       # calctype_maxbg = NULL,
                       # calctype_minbg = NULL,
                       subgroups_type = input$subgroups_type,
                       include_ejindexes = (input$include_ejindexes == "TRUE"),
                       calculate_ratios = input$calculate_ratios,
                       extra_demog = input$extra_demog,
                       need_proximityscore = input$need_proximityscore,
                       infer_sitepoints = FALSE,
                       called_by_ejamit = TRUE,
                       progress_all = progress_all,
                       avoidorphans = input$avoidorphans,
                       testing = input$testing,
                       maxradius = input$maxradius,
                       quiet = TRUE,
                       updateProgress_getblocks = updateProgress_getblocks)

        ## close getblocks progress bar
        progress_getblocks$close()
        sitetype <- ejamit_sitetype_from_output(out)

        ## stop and return NULL if no valid sites left after doaggregate
        if (all(out$results_bysite$valid == FALSE)) {
          message('No valid sites remaining. Quitting analysis')
          data_processed(NULL)

          #progress_doagg$close()
          progress_all$close()
          shiny::showNotification('No valid site remaining, so the analysis was stopped. Please try a larger buffer radius or different dataset.', type = 'error', duration = 5)
          validate('No valid sites remaining - analysis stopped')
        }

        ## close doaggregate progress bar
        # progress_doagg$close()
      } # end of ejamit() for latlon
    } # end of ejamit() for non-fips, ie all latlon or shapefile aggregation
    # done with all ways of analyzing ...  latlon, Shapefiles, and FIPS codes

    ## assign doaggregate output to data_processed reactive
    data_processed(out)

    ## update overall progress bar
    progress_all$inc(1/3, message = 'Step 3 of 3', detail = 'Summarizing')

    #############################################################################  #

    ## update overall progress bar
    progress_all$inc(1/3, message = 'Done processing! Loading results now', detail = NULL)
    num_invalid_sites <- NROW(data_processed()$results_bysite[data_processed()$results_bysite$valid == F,])

    if (num_invalid_sites > 0) {
      showNotification(paste(num_invalid_sites, " sites were excluded from the Community Report results. Refer to the 'Invalid Reason' column in the details panel for more information."),
                       type = "warning", duration = 7)
    }

    ## close overall progress bar
    progress_all$close()

    if (exists("ejamitRunTimeNotification") && !is.null(ejamitRunTimeNotification)) {
      removeNotification(ejamitRunTimeNotification)
    }
    ## switch tabs and jump to top of screen
    shinyjs::js$toTop();
    showTab(session = session, inputId = 'all_tabs', target = 'See Results') # in case was hidden because app had just launched
    updateTabsetPanel(session = session, inputId = "all_tabs",     selected = "See Results")
    updateTabsetPanel(session = session, inputId = 'results_tabs', selected = 'Community Report')

  })  # end of observeEvent based on Start analysis button called input$bt_get_results

  #############################################################################  #
  # if (input$calculate_ratios) {  ## ratios can be dropped from output table of results but are used by summary report, plots, etc. so simplest is to still calculate them
  #############################################################################  #
  # . 4) ratios ####
  # ______ AVERAGES and RATIOS TO AVG
  #
  # # *** Ratios were already calculated by doaggregate()
  # # *** UNLESS somehow the user has changed the defaults to doaggregate(  calculate_ratios = FALSE  )
  #
  # ############################# #
  # ##   demog RATIOS of overall scores to US or state D AVG ####
  #
  ratio.to.us.d <- reactive({unlist(
    data_processed()$results_overall[ , c(..names_d_ratio_to_avg, ..names_d_subgroups_ratio_to_avg)]
  ) })
  #############################################################################  #
  # }
  #############################################################################  #
  # . --------------------------------------------------------------- ####
  #. ## ##
  # ______ SEE RESULTS _________ ####
  #. ####

  # #############################################################################  #

  # ________ RESULTS OVERALL -- SUMMARY REPORT  __________ ####
  #. ####
  # HTML REPORT - FOR UI ####
  # *** The code here builds the html report shown in the shiny app but also see code used to download it.

  ## * Header ####

  ### ( Total Population count ) ####
  total_pop <- reactive({
    req(data_processed())
    ## format and return total population
    round(data_processed()$results_overall$pop, table_rounding_info("pop") )
  })

  ### ( Title of analysis ) ####

  # Unless user changes it here, use a standard title that has been determined by global_defaults_*.R but then optionally modified by advanced settings tab

  output$analysis_title_ui <- renderUI({
    shiny::textInput('analysis_title',
                     label = 'Name of Your Analysis',
                     value = sanitized_standard_analysis_title())
  })
  #############################################################################  #
  ## *MAP ALL SITES ####
  ############################################ #

  ### report_map() ####

  report_map <- reactive({

    validate(need(data_processed(), 'Please run an analysis to see results.'))
    circle_color <- '#000080'

    # if shapefile, merge geometry and create buffer if nonzero buffer is set
    if (submitted_upload_method() == "SHP") {

      # SHP map ------------------------------ #

      #   not sure this validate and circle_color belong here stil:
      validate(need(data_processed(), 'Please run an analysis to see results.'))
      circle_color <- '#000080'

      map_ejam_plus_shp(
        out = data_processed(),
        shp = data_uploaded(),  # here, shp already has ejam_uniq_id assigned (after which invalid rows were dropped I think)
        radius_buffer = sanitized_bt_rad_buff()
      )

    } else { #  not shapefile

      if (!(submitted_upload_method() %in% c('FIPS', 'FIPS_PLACE'))) {

        # LATLON map ------------------------------ #

        popup_labels <- fixcolnames(namesnow = names(data_processed()$results_bysite), oldtype = 'r', newtype = 'shortlabel')
        popup_labels[is.na(popup_labels)] <- names(data_processed()$results_bysite)[is.na(popup_labels)]

        ## similar to previous map but remove controls and only add circles, not circleMarkers

        mapfast(data_processed()$results_bysite,
                radius = submitted_radius_val(),
                launch_browser = FALSE,
                column_names = 'ej', #'all',
                labels = popup_labels,color = circle_color)
      } else {

        # FIPS map - download boundaries then map ------------------------------ #

        req(data_uploaded())
        FTYPES <- fipstype(data_uploaded())
        if (all(FTYPES %in% c("state", "county", "city", "tract", "blockgroup"))) {

          fips_shapes <- try( shapes_from_fips(data_uploaded()), silent = TRUE)
          if (inherits(fips_shapes, "try-error")) {
            fips_shapes <- NULL
            shiny::validate('Unable to obtain FIPS Census units boundaries for map')
          }

          if (!is.null(fips_shapes) && any(!sf::st_is_valid(fips_shapes))) {
            fips_shapes <- sf::st_make_valid(fips_shapes)
          }
          if (!is.null(fips_shapes) && nrow(fips_shapes) > 0) {

            popups <- popup_from_ejscreen(data_processed()$results_bysite)
            map_shapes_leaflet(fips_shapes, popup = popups)

          } else {
            #Possible failsafe needed if fips is invalid? Will it get to this stage? Blank map returned
            leaflet::leaflet() %>% leaflet::addTiles() %>% leaflet::fitBounds(-115, 37, -65, 48)
          }

        } else {
          # Not mappable fipstype, return empty map and warning ------------------------------ #

          warning('Cannot map FIPS types other than state, cities, counties, Census tracts, or Census blockgroups currently.')

          leaflet::leaflet(width = if (isTRUE(getOption("shiny.testmode"))) 1000 else NULL) %>%
            leaflet::addTiles() %>%
            leaflet::fitBounds(-115, 37, -65, 48)
        }
      }
    }
  }) # end of report_map
  ############################################ #

  ### output$quick_view_map of report_map() html ####

  output$quick_view_map <- leaflet::renderLeaflet({
    report_map()
  })
  ############################################ #

  ### leafletProxy()  "an_leaf_map"  ####

  observe({
    ## This statement needed to ensure site selection map stops if too many points uploaded
    req(isTruthy(orig_leaf_map()))
    # clear shapes from map so buffers don't show twice
    leaflet::leafletProxy(mapId = 'an_leaf_map', session) %>% leaflet::clearShapes()
    rad_buff <- sanitized_bt_rad_buff()

    # SHP map ------------------------------ #

    if (current_upload_method() == "SHP") {

      if (!is.na(rad_buff) && rad_buff > 0) {
        shp_valid <- data_uploaded()[data_uploaded()$valid == T, ] # *** remove this if shapefile_clean() will do it
        d_uploads <- sf::st_buffer(shp_valid, # was "ESRI:102005" but want 4269
                                   dist = units::set_units(rad_buff, "mi"))
        leaflet::leafletProxy(mapId = 'an_leaf_map', session) %>%
          leaflet::addPolygons(data = d_uploads, color = "red")
      }
      d_uploads <- data_uploaded() %>%
        dplyr::select(-any_of(c('valid', 'invalid_msg'))) %>%
        sf::st_zm() %>% sf::as_Spatial() # st_zm() was already done? ***

      leaflet::leafletProxy(mapId = 'an_leaf_map', session) %>%
        map_shapes_leaflet_proxy(shapes = d_uploads, popup = popup_from_df(d_uploads %>% sf::st_drop_geometry()))

    } else if (current_upload_method() %in% c('FIPS','FIPS_PLACE')) {

      # FIPS map - download boundaries then map ------------------------------ #

      shps <- shapes_from_fips(fips = data_uploaded())

      leaflet::leafletProxy(mapId = 'an_leaf_map', session) %>%
        map_shapes_leaflet_proxy(shapes = shps,popup = popup_from_df(shps %>% sf::st_drop_geometry()))

    } else {

      # latlon map ------------------------------ #

      d_upload <- data_uploaded()
      base_color      <- '#000080'
      circle_color <- base_color
      popup_vec = popup_from_df(d_upload %>%
                                  dplyr::select(-any_of(c('valid', 'invalid_msg'))))
      suppressMessages(
        leaflet::leafletProxy(mapId = 'an_leaf_map', session, data = d_upload) %>%
          map_facilities_proxy(rad = sanitized_bt_rad_buff(),
                               highlight = TRUE, #input$an_map_clusters,
                               popup_vec = popup_vec,
                               use_marker_clusters = nrow(d_upload) > EJAM:::global_or_param("marker_cluster_cutoff"),
                               clustered = FALSE) # is_clustered())
      )
    }
  }) # end of leafletProxy()  an_leaf_map
  #############################################################################  #
  ## *BARPLOT for short and long reports (avg person D ratios vs US avg) ####

  # compare / merge with  EJAM/R/plot_barplot_ratios.R ***
  # https://exts.ggplot2.tidyverse.org/gallery/
  ###################  #

  v1_summary_plot_state <- reactive({

    req(data_processed())
    if (!is.null(cur_button())) {
      #  1-SITE REPORT BARPLOT - Extract the selected row number from cur_button
      selected_row <- as.numeric(gsub('button_', '', isolate(cur_button())))

      ejam2barplot(
        data_processed(),
        sitenumber = selected_row,
        varnames = c(names_d_ratio_to_state_avg, names_d_subgroups_ratio_to_state_avg),
        main = "Residential Populations at the Analyzed Location \n Compared to State Averages"
      )
    } else {
      # No specific location selected, use a default plot setup (overall results)
      if (input$Custom_title_for_bar_plot_of_indicators == '') {
        # Default plot title
        ejam2barplot(
          data_processed(),
          varnames = c(names_d_ratio_to_state_avg, names_d_subgroups_ratio_to_state_avg),
          main = "Residential Populations at the Analyzed Location \n Compared to State Averages"
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

    req(data_processed())
    # data_processed() needed for ridgeline or boxplot, and ratio.to.us.d() which is made from data_processed() is needed for boxplots,

    ################## #  BARPLOT

    if (input$plotkind_1pager == 'bar') {

      if (!is.null(cur_button())) {
        # 1-site report barplot
        selected_row <- as.numeric(gsub('button_', '', isolate(cur_button())))
        plot_barplot_ratios_ez(
          out = data_processed(),
          varnames = c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg),
          main = "Residential Populations at the Analyzed Location \n Compared to US Overall",
          single_location = TRUE,
          row_index = selected_row
        )
      } else {
        # general overall barplot (not 1-site)
        if (input$Custom_title_for_bar_plot_of_indicators == '') {
          plot_barplot_ratios_ez(
            out = data_processed(),
            varnames = c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg),
            main = NULL # uses default
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
      ################## # RIDGELINE PLOT
    } else if (input$plotkind_1pager == 'ridgeline') {
      req(data_processed())
      rationames = c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg)
      ratio.to.us.d.bysite <- data_processed()$results_bysite[ ,  c(
        ..rationames
      )]
      plot_ridgeline_ratios(ratio.to.us.d.bysite,
                            main = input$Custom_title_for_bar_plot_of_indicators)
      ################## # BOXPLOT
    } else if (input$plotkind_1pager == "box") {

      ## *BOXPLOTS for short report (all sites D ratios vs US avg) ####
      ejam2boxplot_ratios(ejamitout = data_processed(), radius = sanitized_bt_rad_buff(),
                          main = input$Custom_title_for_bar_plot_of_indicators)
    } # box
    ################## #
  })

  ## output: show box/barplot of indicator ratios in html Summary Report #
  output$view1_summary_plot <- renderPlot({
    v1_summary_plot()
  }, width = function() {
    ifelse(isTRUE(getOption("shiny.testmode")), 717, "auto")
  })

  observeEvent(input$results_tabs, {

    if (!is.null(cur_button())) {
      cur_button(NULL)
      # Trigger a re-render of the html summary report's barplot
      output$view1_summary_plot <- renderPlot({
        v1_summary_plot()
      }, width = function() {
        ifelse(isTRUE(getOption("shiny.testmode")), 717, "auto")
      })
    }
  })
  ####################################################### #
  if (isTRUE(getOption("shiny.testmode"))) {
    htmlwidgets::setWidgetIdSeed(12345) # ensures consistent element IDs across runs
    set.seed(12345)
  }
  #############################################################################  #

  ## Render for UI via build_community_report() ####

  output$comm_report_html <- renderUI({
    req(data_processed())

    sitetype <- tolower(submitted_upload_method()) #
    rad <- data_processed()$results_overall$radius.miles # input radius can be changed by user and would alter the report text but should just show what was run not what slider currently says
    nsites <- NROW(data_processed()$results_bysite[data_processed()$results_bysite$valid == T, ])
    popstr <- prettyNum(total_pop(), big.mark = ',') # rounded already?

    if (sitetype %in% "latlon") {
      area_in_square_miles <- nsites * area_sqmi(df = data_processed()$results_overall)
    } else {
      if (sitetype %in% "shp") {
        area_in_square_miles <- data_processed()$results_overall$area_sqmi
      } else {
        if (sitetype %in% "fips") {
          area_in_square_miles <-  data_processed()$results_overall$area_sqmi
        } else {
          area_in_square_miles <- NULL
        }
      }
    }
    residents_within_xyz <- report_residents_within_xyz(
      sitetype = sitetype,
      radius = rad,
      nsites = nsites,
      area_in_square_miles = area_in_square_miles
    )

    ## generate full HTML using external functions -- can this use ejam2report() instead?? ***

    full_page <- build_community_report(in_shiny = TRUE,

      output_df = data_processed()$results_overall,
      analysis_title =  sanitized_analysis_title(),
      totalpop = popstr,
      locationstr = residents_within_xyz,
      include_ejindexes = (input$include_ejindexes == 'TRUE'),
      show_ratios_in_report = (input$show_ratios_in_report == 'TRUE'),
      extratable_show_ratios_in_report = (input$extratable_show_ratios_in_report == 'TRUE'),
      extratable_title = input$extratable_title, # above the table, not in the upper left cell
      extratable_title_top_row = input$extratable_title_top_row,
      extratable_list_of_sections = EJAM:::global_or_param("default_extratable_list_of_sections"),
      extratable_hide_missing_rows_for = input$extratable_hide_missing_rows_for, # c(names_d_language, names_health),

      filename = NULL,
      report_title = NULL, #
      logo_path = NULL,
      logo_html = NULL
    )
    ## return generated HTML
    full_page
  })
  # end of observer that send results of calculation to UI

  #. ####
  #############################################################################  #

  # HTML REPORT - FOR DOWNLOAD ####

  # community_download() and report_community_download() NO LONGER USED HERE

  ### ejam2report() in downloadHandler() ####

  # report_fname <- reactiveVal() # never used now?

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

      # report_fname(file) # never used now?

      if (submitted_upload_method() == 'SHP') {
        shp_for_report <- data_uploaded()
      } else {
        shp_for_report <- NULL
      }
      ejam2report(
        fileextension = '.html',
        filename = file,
        ejamitout = data_processed(),
        return_html = FALSE,
        submitted_upload_method = submitted_upload_method(),
        shp = shp_for_report,
        analysis_title =  sanitized_analysis_title(),
        launch_browser = FALSE,
        show_ratios_in_report = input$show_ratios_in_report,
        extratable_show_ratios_in_report = input$extratable_show_ratios_in_report,
        extratable_title = input$extratable_title,
        extratable_title_top_row = input$extratable_title_top_row,
        extratable_list_of_sections = EJAM:::global_or_param("default_extratable_list_of_sections"),
        extratable_hide_missing_rows_for = input$extratable_hide_missing_rows_for
      )
    }
  )
  #############################################################################  #
  # .  ####
  # ______ RESULTS BY SITE -- DETAILED RESULTS ______ ####

  ## HTML 1-SITE REPORT downloadHandler() ####

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

      # rather than happening here, a separate observer of the 1-site buttons does the work

      req(temp_file_path())
      file.copy(temp_file_path(), file)
    }
  )
  ##############################################  #

  cur_button <- reactiveVal(NULL)
  temp_file_path <- reactiveVal(NULL)
  selected_location_name <- reactiveVal(NULL)

  ### Observe 1-site-report buttons ####
  # (1 button per site in the table of sites, to see barplot for that site)
  observeEvent(
    lapply(
      names(input)[grep("select_button[0-9]+", names(input))],
      function(name) {
        cur_button(input[[name]])
        input[[name]]
      }), {
        req(data_processed())
        req(cur_button())

        # Call ejam2report() with the sitenumber based on which site's button was clicked
        sitenumber <- as.numeric(gsub('button_','', cur_button()))

        # Get the name of the selected location
        location_name <- data_processed()$results_bysite[sitenumber, "statename"]
        selected_location_name(location_name)

        # Store a temporary file name/path in the reactive value
        temp_file <- tempfile(fileext = ifelse(input$format1pager == 'pdf', '.pdf', '.html'))
        temp_file_path(temp_file)

        # if shapefile was used for analysis, provide it to ejam2report()
        if (submitted_upload_method() == 'SHP') {
          shp_for_report <- data_uploaded()
        } else {
          shp_for_report <- NULL
        }

        ejam2report(

          fileextension = '.html',
          filename = temp_file,
          ejamitout = data_processed(),
          sitenumber = sitenumber,
          return_html = FALSE,
          submitted_upload_method = submitted_upload_method(),
          shp = shp_for_report, # NULL or data_uploaded()
          analysis_title =  sanitized_analysis_title(),
          launch_browser = FALSE,
          show_ratios_in_report = input$show_ratios_in_report,
          extratable_show_ratios_in_report = input$extratable_show_ratios_in_report,
          extratable_title = input$extratable_title,
          extratable_title_top_row = input$extratable_title_top_row,
          extratable_list_of_sections = EJAM:::global_or_param("default_extratable_list_of_sections"),
          extratable_hide_missing_rows_for = input$extratable_hide_missing_rows_for
        )

        # modal pops up so user can trigger the download
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
  #. ## ##
  ## INTERACTIVE TABLE of sites = output$interactive_table from data_processed()  ####

  output$interactive_table <- DT::renderDT(server = TRUE, expr = {
    req(data_processed())
    create_interactive_table(data_processed = data_processed(),
                             testing = input$testing)
  })
  #############################################################################  #

  #_____ALL RESULTS AS EXCEL DOWNLOAD ____ ####

  # SEE FUNCTION THAT CAN DO THIS AT ?table_xls_from_ejam() or ejam2excel()

  output$report_version_date <- renderUI({
    message(paste0("shinytestmode = ", getOption("shiny.testmode")))
    p(style = "margin-bottom: 0",
      paste("Version",
            EJAM:::global_or_param("ejam_app_version"),
            "| Report created on",
            ifelse(
              isTRUE(getOption("shiny.testmode")),
              "[SHINYTEST DATE]", # so the snapshot of the report is consistent, not diff date each time tested
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
        cat('starting download code  \n')
      }
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

      # remove hyperlinks from excel output if shapefile is current_upload_method() - Temporary
      if (submitted_upload_method() == "SHP") {
        hyperlink_columns <- NULL
      } else {
        ### disable ejscreen report links while the site is down
        if ("ejscreen_is_down" == "ejscreen_is_down") {
          hyperlink_columns <-  'ECHO Report'
        } else {
          hyperlink_columns <- c("EJScreen Report", "EJScreen Map" ,'ACS Report','ECHO Report')
        }
      }

      ## ejam2report() builds report to put in an excel tab ####

      html_content <- isolate({

        temp_file <- tempfile(fileext = '.html')

        if (submitted_upload_method() == 'SHP') {
          shp_for_report <- data_uploaded()
        } else {
          shp_for_report <- NULL
        }

        # *** ejam2report() or this server code should be made to handle FIPS upload and FIPS dropdown
        # & use obtained FIPS polygons + analysis results as shp
        ## note the mapping code in server already does that, creating report_map()

        ejam2report(

          fileextension = '.html',
          filename = temp_file,
          ejamitout = data_processed(),
          return_html = FALSE,
          submitted_upload_method = submitted_upload_method(),
          shp = shp_for_report,
          analysis_title =  sanitized_analysis_title(),
          launch_browser = FALSE,
          show_ratios_in_report = input$show_ratios_in_report,
          extratable_show_ratios_in_report = input$extratable_show_ratios_in_report,
          extratable_title = input$extratable_title,
          extratable_title_top_row = input$extratable_title_top_row,
          extratable_list_of_sections = EJAM:::global_or_param("default_extratable_list_of_sections"),
          extratable_hide_missing_rows_for = input$extratable_hide_missing_rows_for
        )
      })

      ## ejam2excel() ####

      wb_out <- ejam2excel(

        ejamitout = data_processed(),
        fname = fname,
        ok2plot = input$ok2plot,
        react.v1_summary_plot = v1_summary_plot(),
        launchexcel = FALSE,
        radius_or_buffer_in_miles = sanitized_bt_rad_buff(),
        radius_or_buffer_description = NULL, # radius_or_buffer_description, # the function will figure it out
        buffer_desc = NULL, # "Selected Locations", # the function will figure it out
        in.analysis_title = sanitized_analysis_title(),
        save_now = FALSE,
        interactive_console = FALSE,
        hyperlink_colnames = hyperlink_columns,
        in.testing = input$testing,

        mapadd = FALSE, # redundant if getting report as a tab since report has map snapshot
        report_map = NULL,
        community_reportadd = TRUE,
        community_html = html_content,

        site_method = submitted_upload_method(),
        #show_ratios_in_excel = input$calculate_ratios,
        updateProgress = updateProgress_xl
      )

      progress_xl$close()
      removeModal()
      ## save file and return for downloading
      openxlsx::saveWorkbook(wb_out, fname)

    } # end excel download contents
  ) # end download handler
  #############################################################################  #
  #. ## ##
  ## *BARPLOTS interactive   ####
  #. ## ##
  # see ?ejam2barplot() in EJAM pkg
  # see notes on   https://exts.ggplot2.tidyverse.org/gallery/

  output$summ_bar_ind_out <- renderUI({
    if ((input$include_ejindexes == "TRUE")) {
      radioButtons(inputId = 'summ_bar_ind',
                   label = h5('Indicator type'),
                   choiceNames = c('Residential Populations', 'Environmental',
                                   'Summary Index','Suppl. Summary Index'), # displayed versions of names
                   choiceValues = c('Demographic', 'Environmental',
                                    'EJ Index','Supplementary Index'), # internal versions of name types as found in map_headernames
                   selected = "Environmental")
    } else {
      radioButtons(inputId = 'summ_bar_ind',
                   label = h5('Indicator type'),
                   choiceNames = c('Residential Populations', 'Environmental'),
                   choiceValues = c('Demographic', 'Environmental'),
                   selected = "Environmental")
    }
  })

  output$summ_bar_data <- renderUI({
    req(input$summ_bar_ind)
    if (input$summ_bar_ind == 'Demographic') {
      radioButtons(inputId = 'summ_bar_data',
                   label = 'Data Type',
                   choiceValues = c('ratio',      'raw'),      # no 'pctile' at this time
                   choiceNames  = c('Ratio to US','Raw data'), # no 'Percentile of population' at this time
                   selected = 'ratio')
    } else if (input$summ_bar_ind == 'Environmental') {
      radioButtons(inputId = 'summ_bar_data',
                   label = 'Data Type',
                   choiceValues = c('ratio',      'raw'),      # no 'pctile' at this time
                   choiceNames  = c('Ratio to US','Raw data'), # no 'Percentile of population' at this time
                   selected = 'ratio')
    } else if (input$summ_bar_ind == 'EJ Index') {
      radioButtons(inputId = 'summ_bar_data',
                   label = 'Data Type',
                   choiceValues = c('raw'),
                   choiceNames  = c('Percentile'),
                   selected = 'raw')
    }
    else if (input$summ_bar_ind == 'Supplementary Index') {
      radioButtons(inputId = 'summ_bar_data',
                   label = 'Data Type',
                   choiceValues = c('raw'),
                   choiceNames  = c('Percentile'),
                   selected = 'raw')
    }
  })

  output$summ_display_bar <- renderPlot({
    req(data_processed())
    req(input$summ_bar_ind)
    req(input$summ_bar_data)

    ejam2barplot_indicators(ejamitout = data_processed(), indicator_type = input$summ_bar_ind, data_type = input$summ_bar_data)

  }) # end of summ_display_bar  for barplot

  #############################################################################  #
  ## *HISTOGRAM interactive    ####
  #. ####

  # other plot ideas
  # https://exts.ggplot2.tidyverse.org/gallery/
  current_hist_ind <- reactive({
    input$summ_hist_ind
  })

  output$summ_hist_ind_out <- renderUI({

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

  output$summ_display_hist <- renderPlot({

    req(data_processed())
    req(input$summ_hist_ind)

    ejam2histogram(ejamitout = data_processed(),
                   varname = current_hist_ind(),
                   distn_type = input$summ_hist_distn,
                   data_type = input$summ_hist_data,
                   n_bins = input$summ_hist_bins,
                   sitetype = submitted_upload_method())

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
  # ______ FULL STATIC REPORT (Long Word doc download) _________ ####
  # .  ## ##

  ## code for storing all shiny input values - not used currently
  # observeEvent(input$all_tabs == 'Generate Report',
  #  {
  #    list_of_inputs <- reactiveValuesToList(input)
  #  })
  #############################################################################  #
  ## *downloadHandler() ####

  output$rg_download <- downloadHandler(
    filename = function() {
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
      file.copy(from = app_sys('report/written_report/report.Rmd'),  # treats EJAM/inst/ as root
                to = tempReport, overwrite = TRUE)
      ## pass image and bib files needed for knitting to temp directory
      for (i in list.files(app_sys('report/written_report'), pattern = '.png|.bib')) {   # treats what was in source/EJAM/inst/report/ as installed/EJAM/report/  once pkg is installed
        file.copy(from = app_sys('report/written_report', i),    # source/EJAM/inst/report/ = installed/EJAM/report/
                  to = file.path(tempdir(), i),
                  overwrite = TRUE)
      }

      # Set up parameters to pass to Rmd document -
      #  MAKE SURE all parameter names are used (identical names, and all are there) in these 4 places:
      #  1. input$ ids in app_ui.R, from user, to customize the long report
      #  2. params$ list passed by app_server.R to render the Rmd doc
      #  3. params: accepted in  .Rmd yaml info header
      #  4. params$  as used within body of  .Rmd text inline and in r code blocks.

      isolate({

        # TABLE DEMOG (for long report) ####

        v1_demog_table <- reactive({
          req(data_processed())
          table_gt_from_ejamit_overall(
            data_processed()$results_overall,
            type = 'demog'
          )
        })
        # TABLE ENVT. (for long report) ####

        v1_envt_table <- reactive({
          req(data_processed())
          table_gt_from_ejamit_overall(
            data_processed()$results_overall,
            type = "envt"
          )
        })
        # LONG REPORT PARAMETERS ####

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
          ## see newer helper  report_residents_within_xyz()  and code used by ejam2report() ejam2excel() etc.
          within_x_miles_of = paste0("within ", paste0(sanitized_bt_rad_buff(),' miles'), " of"), # see https://cli.r-lib.org/articles/pluralization.html
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

  # ______ ejscreenapi MODULE _________ ####

  # (to get batch via API)  see default_hide_ejscreenapi_tab in global_defaults_*.R etc.

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
