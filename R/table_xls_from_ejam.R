

#' Format the results of ejamit() for excel and optionally save .xlsx file
#' 
#' Almost identical to [ejam2excel()]
#' 
#' @inheritParams ejam2excel   
#' 
#' @examples \dontrun{
#'   table_xls_from_ejam(testoutput_ejamit_10pts_1miles)
#'   }
#' @return returns a workbook object for use by openxlsx::saveWorkbook(wb_out, pathname)
#'   or returns just the full path/file name of where it was saved if save_now = TRUE
#'   
#' @keywords internal
#'
table_xls_from_ejam <- function(ejamitout, 
                                fname = NULL, # full path and name, or just name of .xlsx file 
                                save_now = TRUE, overwrite = TRUE, launchexcel = FALSE,
                                interactive_console = TRUE, 
                                ok2plot = TRUE,
                                in.testing = FALSE,
                                in.analysis_title =  "EJAM analysis",
                                react.v1_summary_plot = NULL,
                                radius_or_buffer_in_miles = NULL,  #  input$bt_rad_buff
                                buffer_desc = "Selected Locations", 
                                radius_or_buffer_description = 'Miles radius of circular buffer (or distance used if buffering around polygons)', 
                                # radius_or_buffer_description =   "Distance from each site (radius of each circular buffer around a point)",
                                hyperlink_colnames = c("EJScreen Report", "EJScreen Map", "ECHO report"),
                                site_method = "",
                                ...
) {
  
  npts <- NROW(ejamitout$results_bysite)
  if (missing(radius_or_buffer_in_miles)) {
    radius_or_buffer_in_miles  <- ejamitout$results_overall$radius.miles
  } 
  
  #changed the way the filename path was generated
  default_pathname <- create_filename(file_desc = "results_table",
                                      title = in.analysis_title,
                                      buffer_dist = radius_or_buffer_in_miles,
                                      site_method = site_method,
                                      with_datetime = TRUE, 
                                      ext = ".xlsx")
  if (is.null(fname)) {
    fname_was_provided <- FALSE
    pathname <- default_pathname
  } else {
    fname_was_provided <- TRUE
    pathname <- fname
  } 
  
  # server does something like this:
  ## note analysis type or overview to 'notes' tab
  # if (submitted_upload_method() == "SHP") {
  #   radius_or_buffer_description <- 'Distance from each shape (buffering around each polygon)'
  # } else {
  # radius_or_buffer_description <- 'Distance from each site (radius of each circular buffer around a point)'
  # }
  
  # keepcols <- rep(TRUE, NCOL(ejamitout$results_overall))
  
  # defaults in table_xls_format : 
  #
  # overall, eachsite, longnames=NULL, bybg=NULL, formatted=NULL,
  # summary_plot = NULL, 
  # plotlatest = TRUE, 
  # plotfilename = NULL, 
  # 
  # analysis_title = "EJAM analysis",
  # buffer_desc = "Selected Locations", 
  # radius_or_buffer_in_miles = NULL,
  # radius_or_buffer_description='Miles radius of circular buffer (or distance used if buffering around polygons)',
  # notes=NULL,
  # 
  # heatmap_colnames=NULL, heatmap_cuts = c(80, 90, 95), heatmap_colors = c("yellow", "orange", "red"),
  # hyperlink_colnames = c("EJScreen Report","EJScreen Map","ECHO report"), 
  # graycolnames=NULL, narrowcolnames=NULL, graycolor='gray', narrow6=6,
  # 
  # testing=FALSE, launchexcel = FALSE, saveas = NULL,
  
  # in server code:
  #hyperlink_colnames = c("EJScreen Report", "EJScreen Map" ),
  # summary_plot   = v1_summary_plot(),
  # analysis_title = input$analysis_title,
  # buffer_desc    = "Selected Locations", 
  # radius_or_buffer_in_miles = input$bt_rad_buff,
  # radius_or_buffer_description = radius_or_buffer_description,
  # # saveas = fname,
  # testing = input$testing
  
  # these should be data.tables or at least they used to be when coming from ejamit() but not within server code...
  # so does that cause a problem for table_xls_format() if they are data.table format???
  
  # table_xls_format ####
  
  wb_out <- table_xls_format(
    
    # ### if we must provide data.frame only, not data.table, here, then we may need to convert them:
    # overall   = as.data.frame(ejamitout$results_overall), # 1 row with overall results aggregated across sites
    # eachsite  = as.data.frame(ejamitout$results_bysite), # 1 row per site
    # longnames = as.data.frame(ejamitout$longnames), # 1 row, but full plain English column names
    
    overall   = ejamitout$results_overall, #  1 row with overall results aggregated across sites
    eachsite  = ejamitout$results_bysite,  #  1 row per site
    longnames = ejamitout$longnames,       #  1 row, but full plain English column names
    bybg      = ejamitout$results_bybg_people, # not entirely sure should provide bybg tab? it is huge and only for expert users but enables a plot
    formatted = ejamitout$formatted,  
    
    custom_tab = ejamitout$results_summarized$cols,
    custom_tab_name = "thresholds",
    
    hyperlink_colnames = hyperlink_colnames,  # need to ensure these get formatted right to work as links in Excel
    # heatmap_colnames=names(table_as_displayed)[pctile_colnums], # can use defaults
    # heatmap_cuts=c(80, 90, 95), # can use defaults
    # heatmap_colors=c('yellow', 'orange', 'red') # can use defaults
    ## optional, shiny-specific arguments to go in 'Plot' and 'Notes' sheets
    summary_plot   = react.v1_summary_plot, # NULL is fine
    analysis_title = in.analysis_title,
    ok2plot = ok2plot,
    buffer_desc = buffer_desc,
    radius_or_buffer_in_miles    = radius_or_buffer_in_miles,
    radius_or_buffer_description = radius_or_buffer_description,
    # saveas = pathname, # could do it this way but then need to condition it on save_now and cannot offer interactive picking of pathname in RStudio
    testing = in.testing,
    launchexcel = launchexcel,
    ...
  )
  
  if (save_now) {
    if (interactive_console & interactive()) {
      if (!fname_was_provided) {
        repeat {
          pathname <- rstudioapi::showPrompt(
            "Save spreadsheet file",
            "Confirm folder and name of file to save",
            default = pathname
          )

          if (is.null(pathname) || pathname ==  "") {
            cat('Invalid path/file, please provide a valid path.\n')
            next
          }
          if (grepl("[<>:\"/\\?*]", pathname)) {
            stop("Filename contains invalid characters: <>:\"/\\|?*. Please provide a valid name. \n")
            next
          }
          break
        }
      }
    }
    # if (is.null(pathname) || pathname == "" || grepl("[<>:\"/\\?*]", pathname)) { #perform a more robust check of the pathname here. 
    if (is.null(pathname) || pathname == "" || !dir.exists(dirname(pathname))) { #perform a more robust check of the pathname here. 
      
      cat('Invalid path/file,', pathname, ',so using default instead: ', default_pathname, '\n')
      pathname <- default_pathname
    }

    cat("Saving as ", pathname, "\n")
    ## save file and return for downloading - or do this within table_xls_format( , saveas=fname) ?
    openxlsx::saveWorkbook(wb_out, pathname, overwrite = overwrite)
    return(pathname)
  } else {
    invisible(wb_out)
  }
}
