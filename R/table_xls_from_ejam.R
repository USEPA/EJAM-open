

#' helper to pick text phrase to use in excel report notes tab, for Locations analyzed: _____
#'
#' @param sitetype character string, one of "shp", "latlon", "fips" and to describe locations
#' @param site_method string used in filename for saved report and to describe locations
#'   site_method can be SHP, latlon, FIPS, NAICS, FRS, EPA_PROGRAM, SIC, or MACT
#' @returns text string
#' @keywords internal
#'
buffer_desc_from_sitetype <- function(sitetype, site_method) {

  # see also the closely related function report_residents_within_xyz()

  ## old way in server:
  # if (submitted_upload_method() %in% c("SHP", "FIPS", "FIPS_PLACE")) {
  #   radius_or_buffer_description <- 'Distance from each shape (buffering around each polygon)'
  # } else {
  #   radius_or_buffer_description <- 'Distance from each site (radius of circle around a point/site)'
  # }

  ######### #
  site_method2text <- function(site_method) {
    if (missing(site_method) || is.null(site_method)) {
      return("")
    }
    if (site_method == "SHP") {
      return("shapefile")
    }
    if (site_method == "latlon") {
      return("coordinates")
    }
    if (site_method == "FIPS") {
      return("FIPS codes")
    }
    if (site_method == "FIPS_PLACES") {
      return("names of places")
    }
    if (site_method == "NAICS") {
      return("EPA-regulated facilities by NAICS code (industry type)")
    }
    if (site_method == "FRS") {
      return("EPA-regulated facilities by Registry ID")
    }
    if (site_method == "EPA_PROGRAM") {
      return("EPA-regulated Facilities by EPA program")
    }
    if (site_method == "SIC") {
      return("EPA-regulated facilities by SIC code (industry type)")
    }
    if (site_method == "MACT") {
      return("EPA-regulated facilities by MACT category (air toxics emissions source type)")
    }
    return("")
  }
  ######### #

  if (missing(sitetype) || is.null(sitetype)) {
    buffer_desc <- "Selected Locations"
  } else {
    if (sitetype == "shp") {
      buffer_desc <- "Polygons defined by shapefile"
    } else {
      if (sitetype == 'latlon') {
        buffer_desc <- "Locations defined by latitude, longitude and radius"
      } else {
        if (sitetype == "fips") {
          buffer_desc <- 'Census Units defined by FIPS code'
        } else {
          buffer_desc <- "Selected locations"
        }}}}
  if (buffer_desc == "") {
    buffer_desc <- paste0(buffer_desc, ", based on ", site_method2text(site_method))
  }
  return(buffer_desc)
}
##################################################################################### #


#' Format the results of ejamit() for excel and optionally save .xlsx file
#'
#' Almost identical to [ejam2excel()]
#'
#' @inheritParams ejam2excel
#'
#' @examples \donttest{
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
                                radius_or_buffer_description = NULL, # 'Miles radius of circular buffer (or distance used if buffering around polygons)',
                                # radius_or_buffer_description =   "Distance from each site (radius of each circular buffer around a point)",
                                hyperlink_colnames = "ECHO Report",#c("EJScreen Report", "EJScreen Map","ACS Report","ECHO Report"),
                                site_method = "",

                                mapadd = FALSE,
                                report_map = NULL,
                                community_reportadd = TRUE,
                                community_html = NULL,
                                shp = NULL,
                                ...
) {

  npts <- NROW(ejamitout$results_bysite)

  if (missing(radius_or_buffer_in_miles) || is.null(radius_or_buffer_in_miles)) {
    radius_or_buffer_in_miles  <- ejamitout$results_overall$radius.miles
  }

  #   Note `ejamitout$sitetype` is not quite the same as the `site_method` parameter used in building reports.
  #   sitetype    can be shp, latlon, fips
  #   site_method can be SHP, latlon, FIPS, NAICS, FRS, EPA_PROGRAM, SIC, or MACT
  sitetype <- ejamit_sitetype_from_output(ejamitout)
  if (missing(site_method) || is.null(site_method)) {
    site_method <- sitetype
    if (site_method == 'shp' ) site_method <- 'SHP'
    if (site_method == 'fips') site_method <- 'FIPS'
  }

  ## try to sort out what to say and do in these cases:
  ## when shp param is essential  (sitetype == "shp" AND want report or map) but shp missing/null,
  #  when shp param is irrelevant (sitetype == "latlon" OR need neither report nor map) but shp provided,
  #  when shp param is redundant (sitetype == "shp" AND want report or map BUT ALREADY PROVIDED report/map) but shp provided,
  ## when shp param is nonessential but useful (sitetype == "fips" AND want report or map) to avoid a redundant download of FIPS bounds
#
  ### TO BE CONTINUED:
  #
  # if (!is.null(shp) && !community_reportadd && !mapadd) {
  #   message("ignoring shp since mapadd and community_reportadd are both FALSE")
  #   shp <- NULL
  # }
  # if (!is.null(shp) && community_reportadd && !is.null(community_html)) {
  #   message("ignoring shp for community report map since community_html was provided")
  # }
  # # if (sitetype %in% c("shp") && !is.null(shp) && mapadd && !is.null(report_map)) {
  # #   message("using shp for community report map even though report_map was provided")
  # # }
  # if (sitetype %in% c("shp") && mapadd && is.null(report_map) && is.null(shp)) {
  #   warning("cannot add map tab - requires either shp or report_map parameter if sitetype is shp")
  #   mapadd <- FALSE
  # }
  # if (sitetype %in% c("shp") && community_reportadd && is.null(community_html) && is.null(shp)) {
  #   warning("cannot add map in summary report tab - requires either shp or report_map parameter if sitetype is shp")
  # }
  # shp_for_report <- shp
  #
# create report if requested but not provided

  if (community_reportadd && is.null(community_html)) {
    # not provided so try to create it here, noting ejam2report() still requires shp to have FIPS or polygon map in report.
    community_html <- ejam2report(
      ejamitout = ejamitout,
      return_html = FALSE,
      launch_browser = FALSE,
      shp = shp, # that will try to download if shp is null and type is fips or shp
      fileextension = ".html",
      submitted_upload_method = site_method

### may need to pass more params here to build report just like server would have?
### ***



      )
  }

  # for the notes tab of spreadsheet
  if (missing(buffer_desc) || is.null(buffer_desc)) {
    buffer_desc <- buffer_desc_from_sitetype(sitetype = sitetype, site_method = site_method)
  }

  ## for the notes tab of spreadsheet
  if (missing(radius_or_buffer_description) || is.null(radius_or_buffer_description)) {
    radius_or_buffer_description <- report_residents_within_xyz(radius = radius_or_buffer_in_miles,
                                                                nsites = npts,
                                                                sitetype = sitetype)
  }

  # changed the way the filename path was generated
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

  # table_xls_format ####

  # also see the defaults in ejamit() and in table_xls_format()

  # also see the params as used in app_server.R code

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

    mapadd = mapadd,
    report_map = report_map,
    community_reportadd = community_reportadd,
    community_html = community_html,
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
