
# can the app_server_EJAMejscreenapi() be wrapped inside moduleServer() to use as a module in EJAM
# and was that already drafted somewhere? 
# R/MODULE_ejscreenapi.R  had older approach where all of the code was in that one file?

# mod_ejscreenapi_server <- function(id,
#                                    # default_radius_react,
#                                    # default_points_react = NULL, # do we wrap NULL in reactive() in this case?
#                                    default_points_shown_at_startup_react,
#                                    # default_points = NULL,  #
#                                    use_ejscreenit = FALSE  #  use_ejscreenit = FALSE # SWITCH TO TRUE WHEN CONFIRM IT WORKS ***
# ) {
#   # figure out what default points to show at startup, which could be a reactive passed here from the parent server/overall app.
#   # if (!missing(default_points)) default_points_shown_at_startup <- default_points # stop using this and use reactive input only
#   # if (!missing(default_points_react)) {
#   #   # ?
#   #   stopifnot(is.reactive(default_points_react))
#   # }
#   # stopifnot(!is.reactive(default_points_shown_at_startup)) # currently this is not supposed to be reactive but would be if we wanted app-controlled default not just passed when calling function
#   # stopifnot(!is.reactive(parameter2))
#   
#   source(system.file("global_EJAMejscreenapi.R", package = "EJAM"))  ### IMPORTANTLY THESE MUST STAY WITHIN THE MODULE NAMESPACE, AND NOT BE AVAILABLE TO OR INTERFERE WITH MAIN APP'S DEFAULTS/ETC.
#   
#   # moduleServer ####
#   
#   shiny::moduleServer( id, function(input, output, session) {
#     ns <- session$ns
#     
#      then the server code would go here
########################################### # 

#' app_server for ejscreenapi app
#'
#' @param input dont change
#' @param output dont change
#' @param session dont change
#' 
#' @noRd
#' 
app_server_EJAMejscreenapi <- function(input, output, session) {
  
  cat(session$user, "\n", file = stdout())
  
  asynchronous <- FALSE
  if (asynchronous) {
    ## easy candidate is waiting for one api request.
    ## maybe good candidate is waiting for full loop done by ejscreenapi() ? any pieces that get sent to a reactive,
    ## would let user navigate (why?) while waiting for batch. 
    ## *** most importantly, it should free up session for other users on server.
    ##  but not all of ejscreenit() maybe
    # 
    # library(future)
    # library(promises)
    # plan(multisession)
    # 
    ### Most quick code happens beforehand, in process A
    #  myinfoinreactive <- reactive(0)
    #  ## then make a reactive that includes a slow step
    #  must_use_only_in_plot_or_render_or_assign_to_a_reactiveval_toupdateit <- reactive({
    #    # Note a reactive that uses future_promise() can only be passed to other such code or render or assign as reactive promise.
    #  ## so how would you do a func like ejscreenapi?  
    # ejscreenapi_async <- function(a,b,c all ejscreenapi  would use) {
    #   promises::future_promise(ejscreenapi(), globals = LIST THE DATASETS AND CONSTANTS IT WILL NEED  )
    # }
    # snapshot_of_reactiveA <- myinfoinreactive()
    #    promises::future_promise({
    #      snapshot_of_reactiveA + 3
    #      ### SLOW CODE here runs in (child) process B, but you must specify what data objects and values to pass to it as copies,
    #      ### and avoid those being big, and you CANNOT refer to results of any reactives within this child process!  
    #      "SLOW STEPS"  })  %...>% 
    #        ### regular code then uses results of slow, when ready, in process A, and can refer to reactiveA() here 
    #            (function(results) {  ## notice this func is wrapped in parentheses, since it has argument that is result of promise
    #    ### Code here runs in process A
    #  }) # last thing in chain is output as reactive promise.
    # } )
    # 
    # # to update a reactive value in async way
    # revaltoupdate <- reactiveVal(readRDS("cached.rds")) # initial value is read from that file
    #   observeEvent(input$refresh_data, {
    #     future_promise({
    #       df <- read.csv(url)  # read it slowly
    #       saveRDS(df, "cached.rds") # saved it to disk  slowly
    #       df # update the revaltoupdate() so it gets assigned this new value, df
    #     }) %...>%
    #       revaltoupdate() # back in original process, this gets updated once it can be
    #   })
    
    
  }
  
  ## Use Alt-O in RStudio to fold code, then expand top level to see sections.
  ## Use Ctrl-Shift-O in RStudio to view the document Outline panel 
  
  # library(shiny) 
  
  ## Should not need library() at all if each package is in DESCRIPTION file Depends line, right? 
  # But if the pkg is not installed on RStudio server, but 
  # is just loaded via pkgload::load_all(  in app.R, does that use the DESCRIPTION file and load those pkgs? or does RStudio figure out which are needed
  # only by looking at where packagename::etc was used in source code? And that wont work with nonCRAN pkgs?
  # What about data files the package would make available by lazy load?
  
  # [temp button helps debug] ####
  # (REMOVE BEFORE DEPLOYING)
  if (golem::app_dev()) {   
    # cat('NOTE: remove the debugging button from app_server.R before deploying\n')
    # observeEvent(input$browser, {browser()}) # if developer clicks the button, the app pauses
  }
  # (REMOVE BEFORE DEPLOYING)
  # and to unhide the button in the app, while debugging, go 
  # to your web browser, open the JS console, and type:
  #   $('#browser').show();
  
  # output$debugbutton_ui <- renderUI({
  #   conditionalPanel(
  #     condition = golem::app_dev(), 
  #     actionButton(inputId = "browser", label = "PAUSE APP via browser function FOR DEBUGGING"),
  #     tags$script("$('#browser').hide();")
  #   )
  # })
  
  # ____________________________________________________________________ #######
  
  # clean up global env when done with app ?####
  # if the user had any of these on purpose before launching app, this might be a problem to remove them?
  # but check which envt this happens in... globalenv or within function only
  on.exit({ # these are leftovers created by the shiny app via global.R probably  
    rm(list = intersect(ls(), c("apptitle", "asExcel", "base_color_default", "default_circleweight", 
                                "cluster_color_default", "echo_message", "echo_url", "highlight_color_default", "ips",
                                "default_max_pts_upload", 
                                # "input$max_pts_map", 
                                "default_max_miles",  "maxmax_miles",
                                "minradius", "opacitymax", 
                                "opacitymin", "opacityratio", "opacitystart", "opacitystep", 
                                "perhourfast", "perhourguess", "perhourslow", "stepradius", "tabletips_message", "whichip")))
  })
  
  # 1__ should be made into a MODULE__ * SPECIFY PLACES ######################### 
  # ###################   #####################   #####################   #################### # 
  
  # If ECHO search button clicked, Show popup window ####
  
  bindEvent(observe({
    showModal(
      modalDialog(title = "Use ECHO facility search tools to specify list of sites",
                  echo_message,
                  shiny::HTML(paste('<a href=\"', echo_url, '\", target=\"_blank\">', echo_url,  '</a>', sep = '')),
                  easyClose = TRUE)
    )
  }), input$echobutton)
  
  #  pts() = Upload point locations csv file ####
  
  # THIS CODE ALLOWS UPLOAD TO SPECIFY BY LAT/LON, FacLat/FacLong, FRS registry_id, or FRS pgm_sys_id 
  # ALSO WILL ALLOW SEARCH BY NAICS (INDUSTRIAL SECTOR) 
  # ALSO WILL ALLOW UPLOAD OF SHAPEFILE/ GIS DATA LAYER TO SPECIFY BUFFER AREAS, ETC.
  
  ############################################################# #
  ############################################################# #
  ############################################################# #
  
  pts <- shiny::reactive({
    
    # THIS GETS UPDATED WHEN THERE IS A CHANGE IN input$pointsfile
    ## if not uploaded yet, show default example. ####
    if (is.null(input$pointsfile)) {
      pts_filecontents <- default_points_shown_at_startup  # defined in global.R
    } else {
      
      # try to read the file ####
      cat(paste0('Trying to upload a file',  '\n'), file = stderr()) # just prints to console
      filepath <- input$pointsfile$datapath
      
      confirmed_read_and_clean_points_works <- FALSE # ***
      # ____________________________________________________________________ ####### 
      
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
                
                # QUERY FRS for lat lon via facility registry ID ####
                
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
                ## QUERY FRS for lat lon via program system ID ####
                
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
                  }
                }
              }
            }
          } }
        
        ############################################### # 
        # ______DELETE the chunk above when replaced by function_________###############################################  
        ##  ____________________________________________________________________ ####### 
        
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

    pts_filecontents$mapurl  <- paste0('<a href=\"', mapurl, '\", target=\"_blank\">EJScreen Map ', rownames(pts_filecontents), '</a>')
    
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
  output$radius_slider <- renderUI({sliderInput(inputId =  ("radius_via_slider"),
                                                label = paste0("Radius of ",     input$default_miles, " miles ",
                                                               paste0("(", round(input$default_miles      * meters_per_mile / 1000, 3), ' km)')),
                                                value =                          input$default_miles,
                                                min = minradius, max = input$max_miles, step = stepradius
  ) })
  # minradius and stepradius are defined in   global_EJAMejscreenapi.R file
  
  shiny::observe({     shiny::updateSliderInput(session = session, inputId =  ('radius_via_slider'),
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
  if ("works in server not module" == "works in server not module") {
    # Use typedin radius or slider ####
    #  allow radius to get typed into a text box as an alternative !!! ***
  
  radius_miles <- reactive({
    if (input$slider_vs_text_radius == "type_in") {
      x = as.numeric(radius_via_text_validated())
      # cat('radius is ', x, '\n')
    }
    if (input$slider_vs_text_radius == "use_slider") {
      x = input$radius_via_slider
      # cat('radius is ', x, '\n')
    }
    x
  })
  }  else {
    stop('not done like this here - see module')
  #   radius_miles <- reactive({ input$SIMPLERADIUS })
  }
  ############################################################# #
  ############################################################# #
  ############################################################# #
  # . ####
  
  
  # _...Output Text: point count, radius in km, etc. ####
  
  output$count       <- renderText({paste0(NROW(pts()),    ' points have been uploaded.')}) 
  # is this always identical to number of points in results table, since fills in row even if bad lat/lon? not sure it does!
  # or no blockpoint in circle?
  # and if upload is NAICS etc. then pts() and results_table() will have different counts! 
  output$max_pts_upload_txt  <- renderText({paste0(format(input$max_pts_upload,big.mark = ',', scientific = FALSE), ' points max. can be uploaded.')})   
  output$max_pts_map_txt     <- renderText({paste0(format(input$max_pts_map,   big.mark = ',', scientific = FALSE), ' points max. can be shown on a map.')})
  output$max_pts_showtable_txt <- renderText({paste0(format(input$max_pts_showtable, big.mark = ',', scientific = FALSE), ' points max. can be shown on the interactive table.')})
  output$max_pts_run_txt       <- renderText({paste0(format(input$max_pts_run   ,big.mark = ',',     scientific = FALSE), ' points max. can be analyzed.')})    
  output$speed <- renderText({speedmessage(NROW(pts()), perhourslow = perhourslow, perhourfast = perhourfast, perhourguess = perhourguess)})  # speedmessage defined as function in R folder, default variables defined in global.R
  
  # ####################   #####################   #####################   #################### # 
  # ####################   #####################   #####################   #################### # 
  
  # ____________________________________________________________________ ####### 
  # 2__ MODULE__ * GET STATS VIA API #### separable module could just GET BASIC STATS ON EACH BUFFER VIA EJSCREEN API, ####
  # using  params: pts() , maxpts , whichip , radius_miles(), but not blockgroupdata, since that is via the API    #################### # 
  ####################   #####################   #####################   #################### # 
  
  # GET BASIC RESULTS ####   ##################### 
  
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
          title="RESULTS TABLE LOADING - PLEASE WAIT...",
          speedmessage(NROW(pts()), perhourslow = perhourslow, perhourfast = perhourfast, perhourguess = perhourguess),
          size="l", footer=NULL
        ))
        ############################################################## #
        
        # NOTE MAY REPLACE THIS SECTION OF CODE WITH ejscreenapi_plus() which does this itself  :
        #   or maybe even use  ejscreenit() or ejscreenit_for_ejam()  
        
        # *_EJScreen API Get Results*  ####
        batchtableout <- ejscreenapi(
          lon = pts()$lon, lat = pts()$lat,  # ignored if fips not NULL
          radius = radius_miles(), unit = 'miles', wkid = 4326 , 
          fips = pts()$fips, # will be NULL if no such column
          format_report_or_json = 'pjson', ipurl = whichip,  # was  input$whichip
          report_every_n = report_every_n_default,
          save_when_report = FALSE, on_server_so_dont_save_files = TRUE,
          updateProgress = updateProgress, # updateProgress is a function that has been defined already and gets passed here to the slow function that needs to report progress bar updates
          drop_redundant_indicators = TRUE,
          getstatefromplacename = TRUE, verbose = FALSE
        )
        
        # if (attr(batchtableout, 'noresults') ) { # needs to be fixed ****
        #   showModal(modalDialog(title = "Warning", "No results - API might not be accessible or no site has data available", easyClose = TRUE))
        # }
        
        ########################## #
        ## Add more columns to basic batch results ####
        ### Combine input (from user) + output (from EJ stat buffering) ####
        #      (e.g., that user input uploaded may have site name, address, etc.) 
        #      & return combined table: 1 row per site (buffer), 1 col per indicator (variable).
        ### Add links to EJScreen ####
        ### Flag sites near others ####
        ### Put best cols 1st #### 
        results_table <- cbind(pts(), batchtableout) # this would be a problem if we did not isolate or use bindEvent
        results_table <- urls_clusters_and_sort_cols(results_table)
        
        results_table <- results_table[, names(results_table) != 'mapurl']   # drop this column that was only useful while viewing uploaded points but is redundant in final results here
        
        ############################################################## #
        # NOTE THE VARIABLE NAMES HERE ARE AS PROVIDED BY API, NOT RENAMED TO LOWERCASE MORE OBVIOUS NAMES
        # WHILE ejscreenapi_plus() or ejscreenit()  provide renamed indicators (by default).
        # }
      }
    }) # end isolated part
    
    results_table
    
  }) %>% bindCache(pts(), radius_miles()) %>% 
    bindEvent(input$runbutton ) # this makes it react only when button is pushed (or if other non-isolated reactives above change, like input$usewhichnames)
  ####################   #####################   #####################   #################### # 
  
  ##################### Remove modal window once results ready ####
  
  observe({
    req(results_table())
    removeModal()
    # also, maybe remove UI here, to not show the input table anymore
    
    # maybe save the results to some server here, for large batches where the user may not see it finished! ***
    
  })
  
  ####################   #####################   #####################   #################### # 
  # DISPLAY INTERACTIVE TABLE ####   ##################### 
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
  ) # *** CAUTION -- MAY NEED TO CHANGE THIS *** 
  # escape=TRUE is better for security reasons (XSS attacks).
  # escape=FALSE fixes ejscreen pdf URL to be a link, 
  # but it breaks links from ECHO table download (wont be clickable in table) ***
  ############################################################## #
  
  table_as_displayed_reactive <- reactive({   # params: input$usewhichnames map_headernames and results_table()
    req(results_table())
    
    table_as_displayed <- results_table() # this results_table() is always in original names format, with no added columns. 
    
    # _Toggle column names ####
    if (is.null(input$usewhichnames)) {usewhichnames <- 'long'} else {
      usewhichnames <- input$usewhichnames
    }
    
    # cat('....................... usewhichnames is ', usewhichnames, '\n')
    # almost identical to code in ejscreenapi_plus() and ejamit() which relies on that:
    
    # colnames in results table are always api version of names
    #_Rename columns from original format to use R-friendly names ####
    
    names(table_as_displayed) <- fixcolnames(
      namesnow = names(table_as_displayed), 
      oldtype = 'api', # because results_table()  never has renamed colnames
      newtype = 'r', 
      mapping_for_names = map_headernames
    )
    
    ############################################################# #
    
    # note: ejscreenapi_plus() vs app_server_EJAMejscreenapi vs MODULE ############################################################# #
    
    ### Add Ratios to us or state averages ####
    
    if (input$include_ratios) {
      table_as_displayed <- calc_ratios_to_avg_for_ejscreenapi(table_as_displayed = table_as_displayed)
    } # end of ratio calculations   
    
    ############################################################## #
    
    #_Rename columns from R scheme to what user requested ####
    
    names(table_as_displayed) <- fixcolnames(
      namesnow = names(table_as_displayed), 
      oldtype = 'r' ,  # had temporarily renamed to r type
      newtype = usewhichnames,  # now will be named using headers user wanted
      mapping_for_names = map_headernames
    )
    ############################################################## #
    
    #-_Commas for pop count ####
    if ('totalPop' %in% names(table_as_displayed)) table_as_displayed$totalPop <- prettyNum(round(table_as_displayed$totalPop, 0), big.mark = ',') 
    if ('pop'      %in% names(table_as_displayed)) table_as_displayed$pop      <- prettyNum(round(table_as_displayed$pop, 0),      big.mark = ',') 
  # } # end of if (!use_ejscreenit())
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
    # DT::renderDT aka DT::renderDataTable() is used here. 
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
    searchPanes=TRUE                      # does not seem to work
  ),
  # filter='top', orderCellsTop = TRUE),  # does not seem to work. 
  # filter=list(position='top')           # does not seem to work.  is this how?
  # server = TRUE, # should already be the default for DT::renderDataTable, and is better if table is large
  
  escape = FALSE
  ) # *** CAUTION -- MAY NEED TO CHANGE THIS *** 
  # escape=TRUE is better for security reasons (XSS attacks).
  # escape=FALSE fixes ejscreen pdf URL to be a link, 
  # but it breaks links from ECHO table download (wont be clickable in table) ***
  ############################################################## #
  
  
  ## Show inputs or results, whatever was just updated ####
  # keeps track of which is latest one updated, to display that
  
  
  
  
  
  
  
  
  
  
  
  
  ## Show inputs or results, whatever was just updated ##  ##
  output$table_ui <-  renderUI({
    # if (most_recently_changed() == 'run') DT::DTOutput('rendered_results_table')
    if (most_recently_changed() == 'upload'   ) {
      DT::DTOutput('rendered_input_table')
    } else {
      DT::DTOutput('rendered_results_table')
    }
  })
  
  #####################   #####################   #####################   #################### # 
  #####################   #####################   #####################   #################### # 
  # . ####
  ## Downloading the Table + Excel format + Links!* ####
  
  ### Buttons to Rename table header row -show only if results ready ####
  output$renameButton_ui <- renderUI({
    
    shiny::radioButtons(
      'usewhichnames', label = 'Rename columns?',
      inline = TRUE,
      choiceNames = list(
        'Plain English: "National percentile for Ozone (ppb)"',
        'for analysis: "pctile.o3"',
        'from API: "N_E_O3_PER"'
      ),
      choiceValues = list(
        'long',
        'friendly',
        'original'
      ),
      selected = 'long'
    )
  })
  
  ### Button to Download table -show only if results ready and no new upload yet ####
  output$downloadButton_ui <- renderUI({
    req(results_table())
    shiny::downloadButton('downloadbutton', 'Download these results')
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
  
        # fix URLs to work in csv pulled into Excel or in Excel files (as opposed to datatable shown in browser)
        table_as_displayed$`EJScreen Report` <- gsub('.*(http.*)\", target=.*', '\\1', table_as_displayed$`EJScreen Report`) 
        table_as_displayed$EJScreenMAP <- gsub('.*(http.*)\", target=.*', '\\1', table_as_displayed$EJScreenMAP)
        
        if (!asExcel) {
          table_as_displayed$`EJScreen Report` <- paste0('=HYPERLINK("', table_as_displayed$`EJScreen Report`,'", "EJScreen Report ', rownames(table_as_displayed), '")')
          table_as_displayed$EJScreenMAP <- paste0('=HYPERLINK("', table_as_displayed$EJScreenMAP,'", "EJScreen Map ',    rownames(table_as_displayed), '")')
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
  #   shiny::actionButton('tabletips_button', 'Tip on using this table')  ## tips on using table
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
  
  ### Which Popups & Table are ready? uploaded pts vs. results  ####
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
  
  ### is_clustered (where one may double-count people):  ####
  
  is_clustered <- reactive({
    # i.e., which sites have residents that might also be near others sites?
    # circles overlap if 2 facilities are twice the radius apart  # in miles
    # check either pts() or results_table() depending on value of most_recently_changed() being 'upload' or 'results'
    # Do not limit by cap on mapping yet, since this info gets used in table download also which has a larger cap or no cap. 
    if (most_recently_changed() == 'results') {x <-  distance_near_eachother(lon = results_table()$lon, lat = results_table()$lat, distance = 2 * radius_miles()) }
    if (most_recently_changed() == 'upload')  {x <-  distance_near_eachother(lon = pts()$lon,           lat = pts()$lat,           distance = 2 * radius_miles()) }
    x
  })
  
  ### Data for popups for map #############################################
  
  popup_to_show <- reactive({
    # Whether to display popups on map using uploaded point data or results of EJ stats run.
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
                     popupOptions = list(maxHeight = 400, maxWidth = 850),
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
                   popupOptions = list(maxHeight = 400, maxWidth = 850),
                   popup = popup_to_show() #  already have RESTRICTED NUMBER OF POPUPS BASED ON CAP input$max_pts_map
        )
      ### Button to print map #### 
      leaflet.extras2::addEasyprint(map = mylivemap, options = leaflet.extras2::easyprintOptions(exportOnly = TRUE, title = 'Save Map Snapshot'))
      
      ### color points if clustered  ####
      #
      if (input$cluster_highlighting_on) {
        some <- mypoints[is_clustered()[  1:min(input$max_pts_map, NROW(is_clustered()))  ], ]
        addCircles(lng = some$longitude, lat = some$latitude, 
                   map = mylivemap, radius = radius_meters,
                   color = cluster_color(), fillColor = cluster_color(), fill = TRUE, weight = input$circleweight_in ,
                   # opacity = input$opaquecircles, fillOpacity = max(0, opacityratio * input$opaquecircles - opacitystep),
                   popupOptions = list(maxHeight = 400, maxWidth = 850),
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
          map = mylivemap, lng = highlighted$lon , lat = highlighted$lat , radius = radius_meters,
          # color = input$highlight_color_in, fillColor = input$highlight_color_in,  # enable in ui if want to use this option
          color = highlight_color(), fillColor = highlight_color(),
          opacity = 0.9, fill = TRUE, weight = input$circleweight_in, 
          # opacity = input$opaquecircles, fillOpacity = max(0, opacityratio * input$opaquecircles - opacitystep),
          popupOptions = list(maxHeight = 400, maxWidth = 850),
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
  
  # # mapview::mapView() - Another option might be to use mapView function in mapview package, which is easier in general but a bit trickier in shiny apps it seems
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
    names(out) <- fixnames(names(out), oldtype = 'api', newtype = 'r', mapping_for_names = map_headernames)
    us.ratios    <- calc_ratios_to_avg(out)
    #state.ratios <- calc_ratios_to_avg(out = out, zone.prefix = 'state')
    ## boxplots_ratios(us.ratios$ratios_d)
    outplot <- boxplots_ratios(us.ratios$ratios_d, 'pctlowinc', '% low income', wheretext = paste0("Within ", out$radius.miles[1]," miles of"))
    outplot
    # plot(us.ratios)
  } )
  # }, cache = "session", cacheKeyExpr = {list(out)})
}

## Use Alt-O in RStudio to fold code, then expand top level to see sections.
## Use Ctrl-Shift-O in RStudio to view the document Outline panel 
