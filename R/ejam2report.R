
#' View HTML Report on EJAM Results (Overall or at 1 Site)
#'
#' @description Get URL for and view in browser a 2-page summary report similar to the 
#' EJScreen Community Report
#' 
#' @details This relies on [build_community_report()] as used in web app
#'   for viewing report on 1 site from a list of sites.
#' @param ejamitout output as from [ejamit()], list with a data.table called `results_bysite`
#'   if sitenumber parameter is used, or a data.table called `results_overall` otherwise
#' @param sitenumber If a number is provided, the report is about 
#'   `ejamout$results_bysite[sitenumber, ]` and if no number is provided (param is NULL)
#'   then the report is about `ejamout$results_overall`
#' @param analysis_title optional title of analysis
#' @param submitted_upload_method `"latlon"` is the default and other options are not implemented yet.
#' @param data_up_shp not implemented yet, for upload method SHP
#' @param launch_browser set TRUE to have it launch browser and show report.
#' @param return_html set TRUE to have function return HTML object instead of URL of local file
#' @return URL of temp html file or object depending on return_html,
#'    and has side effect of launching browser to view it depending on return_html
#'
#' @examples
#' #out <- ejamit(testpoints_10, radius = 3, include_ejindexes = T)
#' out <- testoutput_ejamit_10pts_1miles
#'
#' ejam2report(out)
#' table_gt_from_ejamit_overall(out$results_overall)
#' table_tall_from_overall(out$results_overall)
#'
#' x <- ejam2report(out, sitenumber = 1)
#' table_gt_from_ejamit_1site(out$results_bysite[1, ])
#' browseURL(x)
#' 
#' @export
#' 
ejam2report <- function(ejamitout = testoutput_ejamit_10pts_1miles, 
                        sitenumber = NULL,  
                        analysis_title = "EJAM Report", 
                        submitted_upload_method = c("latlon", "SHP", "FIPS")[1],
                        data_up_shp = NA,
                        return_html = FALSE, 
                        launch_browser = TRUE) {
  
  if (missing(submitted_upload_method)) {
    if (all(is.na(ejamitout$results_bysite$radius.miles)) &&  # radius from ejamit() is NA in FIPS case, not zero 
        all(is.na(ejamitout$results_bysite$lat)) &&
        all(fips_valid(ejamitout$results_bysite$ejam_uniq_id))) {
      submitted_upload_method <- "FIPS"
    } else {
      if (all(is.na(ejamitout$results_bysite$lat))) {   #    to be finished later
        submitted_upload_method <- "SHP"
      }
    }
  }
  
  if (!interactive()) {launch_browser <- FALSE} # but that means other functions cannot override this while not interactive.
  if (is.null(sitenumber)) {
    ejamout1 <- ejamitout$results_overall
  } else {
    ejamout1 <- ejamitout$results_bysite[sitenumber, ]
  }
  include_ejindexes <- any(names_ej_pctile %in% colnames(ejamout1))
  
  if (!("valid" %in% names(ejamout1))) {ejamout1$valid <- TRUE}
  
  x <- as.numeric(sitenumber)
  if (ejamout1$valid == T) {
    #!(submitted_upload_method %in% c('FIPS')) &
    popstr <- prettyNum(round(ejamout1$pop, table_rounding_info("pop")), big.mark = ',')
    
    if (submitted_upload_method == 'SHP') {
      # locationstr <- paste0('Polygon ', data_up_shp[x,]$OBJECTID_1)
      if (is.null(sitenumber)) {
        locationstr <- "Areas in Aggregate (defined by shapefile)"
      } else {
        locationstr <- paste0("Site ", sitenumber, " (ejam_uniq_id ", ejamout1$ejam_uniq_id, ")")
      }
      
      if (ejamout1$radius.miles > 0) {
        locationstr <- paste0(locationstr, '<br>with ', ejamout1$radius.miles, ' mile buffer')
      }
    } else if (submitted_upload_method == 'FIPS') {
      locationstr <- paste0('FIPS Code ', ejamout1$ejam_uniq_id)
    } else {
      locationstr <- paste0(ejamout1$radius.miles, ' Mile Ring Centered at ',
                            ejamout1$lat, ', ',
                            ejamout1$lon, '<br>', 'Area in Square Miles: ',
                            round(pi * ejamout1$radius.miles^2, table_rounding_info("radius.miles"))
      )
    }
    
    if (!('main.css' %in% list.files(tempdir()))) {
      file.copy(from = EJAM:::app_sys('report/community_report/main.css'),
                to = file.path(tempdir(), 'main.css'), overwrite = TRUE)          
    }
    if (!('communityreport.css' %in% list.files(tempdir()))) {
      file.copy(from = EJAM:::app_sys('report/community_report/communityreport.css'),
                to = file.path(tempdir(), 'communityreport.css'), overwrite = TRUE)          
    }
    
    if (!('EPA_logo_white_2.png') %in% list.files(file.path(tempdir(), 'www'))) {
      dir.create(file.path(tempdir(), 'www'))
      file.copy(from = EJAM:::app_sys('report/community_report/EPA_logo_white_2.png'),
                to = file.path(tempdir(), 'www', 'EPA_logo_white_2.png'), overwrite = TRUE)
    }
    
    temp_comm_report <- file.path(tempdir(), paste0("comm_report",x,".html"))
    
    tempReport <- file.path(tempdir(), 'community_report_template.Rmd')
    
    ## copy Rmd from inst/report to temp folder  (note there had been a similar but not identical .Rmd in EJAM/www/)
    file.copy(from = EJAM:::app_sys('report/community_report/community_report_template.Rmd'),  # treats EJAM/inst/ as root
              to = tempReport, overwrite = TRUE)
    if (return_html) {
      temp_comm_report_or_null <- NULL
    } else {
    temp_comm_report_or_null <- temp_comm_report
    }
    
    ## as used in community_report_template.Rmd it is this:
    # build_community_report(
    #   output_df = params$output_df,
    #   analysis_title = params$analysis_title,
    #   totalpop = params$totalpop,
    #   locationstr = params$locationstr,
    #   include_ejindexes = params$include_ejindexes,
    #   in_shiny = params$in_shiny,
    #   filename = params$filename
    # )
    
    x <- build_community_report(
      output_df = ejamout1,
      analysis_title = analysis_title,
      totalpop = popstr,
      locationstr = locationstr,
      include_ejindexes = include_ejindexes,
      in_shiny = FALSE,
      filename = temp_comm_report_or_null # passing NULL should make it return the html object
    )
    cat(x, file = temp_comm_report)
    if (launch_browser) {
      browseURL(temp_comm_report)
    }
    if (return_html) {
      return(x)
    } else {
      return(temp_comm_report)
    }
    ########################################################################################### #
    ## can also generate reports through knitting Rmd template
    ## this is easier to add in maps and plots but is slower to appear
    
    # isolate({  # need someone to confirm this is needed/helpful and not a problem, to isolate this.
    #   ## pass params to customize .Rmd doc  # ###
    #   # params <- list(html_content = full_html_reactive(),
    #   #                map         = report_map(),
    #   #                summary_plot = v1_summary_plot())
    #   rad <- ejamout$results_bysite[x,]$radius.miles # input$radius can be changed by user and would alter the report text but should just show what was run not what slider currently says
    #   popstr <- prettyNum(round(ejamout$results_bysite$pop[x]), big.mark=',')
    #   locationstr <- paste0(ejamout$results_bysite[x,]$radius.miles, ' Mile Ring Centered at ',
    #                         ejamout$results_bysite[x,]$lat, ', ',
    #                         ejamout$results_bysite[x,]$lon, '<br>', 'Area in Square Miles: ',
    #                         round(pi* rad^2,2)
    #   )
    #   params <- list(
    #     output_df = ejamout$results_bysite[x,],
    #     analysis_title = analysis_title,
    #     totalpop = popstr,
    #     locationstr = locationstr,
    #     include_ejindexes = T,
    #     in_shiny = F,
    #     filename = NULL,#temp_comm_report,
    #     map = report_map(),
    #     summary_plot = v1_summary_plot()
    #   )
    #   
    # })
    
    # rmarkdown::render(tempReport, 
    #                   output_format ='html_document',
    #                   output_file = temp_comm_report,
    #                   params = params,
    #                   envir = new.env(parent = globalenv()),
    #                   intermediates_dir = tempdir()
    # )
    
    #browseURL(temp_comm_report)
    
  } else {
    rstudioapi::showDialog(title = 'Report not available',
                           'Individual site reports not yet available.')
    return(NA)
  }
}
########################################################################################### #
