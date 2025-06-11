
#' View HTML Report on EJAM Results (Overall or at 1 Site)
#'
#' @description Get URL for and view in browser a 2-page summary report similar to the
#' EJScreen Community Report
#'
#' @details This relies on [build_community_report()] as used in web app
#'   for viewing report on 1 site from a list of sites (or overall).
#'   You can customize the report somewhat by using parameters like extratable_list_of_sections
#'
#' @param ejamitout output as from [ejamit()], list with a data.table called `results_bysite`
#'   if sitenumber parameter is used, or a data.table called `results_overall` otherwise
#' @param sitenumber If a number is provided, the report is about
#'   `ejamitout$results_bysite[sitenumber, ]` and if no number is provided (param is NULL)
#'   then the report is about `ejamitout$results_overall`
#' @param analysis_title optional title of analysis
#' @param submitted_upload_method something like "latlon", "SHP", "FIPS", etc. (just used as-is as part of the filename)
#' @param shp provide the sf spatial data.frame of polygons that were analyzed so you can map them since
#'   they are not in ejamitout
#' @param launch_browser set TRUE to have it launch browser and show report.
#' @param return_html set TRUE to have function return HTML object instead of URL of local file
#' @param fileextension html or .html or pdf or .pdf (assuming pdf option has been implemented).
#'   Creating PDF output from R Markdown requires that LaTeX be installed.
#' @param filename optional path and name for report file, used by web app
#' @param show_ratios_in_report logical, whether to add columns with ratios to US and State overall values, in main table of envt/demog. info.
#' @param extratable_show_ratios_in_report logical, whether to add columns with ratios to US and State overall values, in extra table
#'
#' @param extratable_title Text of overall title ABOVE the extra table
#' @param extratable_title_top_row Text INSIDE top left cell of extra table
#'
#' @param extratable_list_of_sections This defines what extra indicators are shown.
#'   It is a named list of vectors,
#'   where each name is text phrase that is title of a section of the table,
#'   and each vector is the vector of colnames of output_df that are indicators
#'   to show in that section, in extra table of demog. subgroups, etc.
#'
#' @param extratable_hide_missing_rows_for only for the indicators named in this vector,
#'   leave out rows in table where raw value is NA,
#'   as with many of names_d_language, in extra table of demog. subgroups, etc.
#'
#' @param report_title optional generic name of this type of report, to be shown at top, like "EJAM Multisite Report"
#' @param logo_path optional relative path to a logo for the upper right of the overall header.
#'   Ignored if logo_html is specified and not NULL, but otherwise uses default or param set in run_app()
#' @param logo_html optional HTML for img of logo for the upper right of the overall header.
#'   If specified, it overrides logo_path. If omitted, gets created based on logo_path.
#'
#' @return URL of temp file or object depending on return_html,
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
                        analysis_title = 'Summary of Analysis',
                        submitted_upload_method = c("latlon", "SHP", "FIPS")[1],
                        shp = NULL,
                        return_html = FALSE,
                        fileextension = c("html", "pdf")[1],
                        filename = NULL,
                        launch_browser = TRUE,
                        show_ratios_in_report = TRUE,
                        extratable_show_ratios_in_report = TRUE,
                        extratable_title = '', #'Additional Information',
                        extratable_title_top_row = 'ADDITIONAL INFORMATION',
                        extratable_list_of_sections = list(
                          # see build_community_report defaults and see global_defaults_*.R
                          `Breakdown by Population Group` = names_d_subgroups,
                          `Language Spoken at Home` = names_d_language,
                          `Language in Limited English Speaking Households` = names_d_languageli,
                          `Breakdown by Sex` = c('pctmale','pctfemale'),
                          `Health` = names_health,
                          `Age` = c('pctunder5', 'pctunder18', 'pctover64'),
                          `Community` = names_community[!(names_community %in% c( 'pctmale', 'pctfemale', 'pctownedunits_dupe'))],
                          `Poverty` = names_d_extra,
                          `Features and Location Information` = c(
                            names_e_other,
                            names_sitesinarea,
                            names_featuresinarea,
                            names_flag
                          ),
                          `Climate` = names_climate,
                          `Critical Services` = names_criticalservice,
                          `Other` = names_d_other_count
                          # , `Count above threshold` = names_countabove  # need to fix map_headernames longname and calctype and weight and drop 2 of the 6
                        ),
                        ## all the indicators that are in extratable_list_of_sections:
                        extratable_hide_missing_rows_for = as.vector(unlist(extratable_list_of_sections)),
                        report_title = NULL,
                        logo_path = NULL,
                        logo_html = NULL
                        ## Rmd_name and Rmd_folder could be made params to pass to report_setup_temp_files()
) {

  if (!interactive()) {launch_browser <- FALSE} # but that means other functions cannot override this while not interactive.

  # adjust this once .pdf option is implemented/working
  fileextension <- paste0(".", gsub("^\\.", "", fileextension)) # add leading dot if not present
  fileextensions_implemented <- ".html" # c(".html", ".pdf")
  if (!(fileextension %in% fileextensions_implemented)) {
    warning("fileextension must be one of", fileextensions_implemented)
    fileextension <- ".html"
  }

  # header info ####

  if (missing(submitted_upload_method)) {
    # as used in server, this could be SHP, FIPS, latlon, MACT, FRS, EPA_PROGRAM_up, etc. etc.
    # create_filename() did use that version in server.
    # but here we just want to know if it is sitetype shp, fips, or latlon.
    if (!("sitetype" %in% names(ejamitout))) {
      ejamitout$sitetype <- ejamit_sitetype_from_output(ejamitout)
    }
    if (ejamitout$sitetype == 'shp') {
      submitted_upload_method <- 'SHP'
    } else {
      if (ejamitout$sitetype == 'fips') {
        submitted_upload_method <- 'FIPS'
      } else {
        if (ejamitout$sitetype == 'latlon') {
          submitted_upload_method <- 'latlon'
        }
      }
    }
  }

  sitenumber <- as.numeric(sitenumber)

  if (is.null(sitenumber) || length(sitenumber) == 0) {
    ejamout1 <- ejamitout$results_overall # one row
    ejamout1$valid <- TRUE
    # but shp is all rows, remember, and popup can still be like for site by site
    nsites <- NROW(ejamitout$results_bysite[ejamitout$results_bysite$valid %in% TRUE, ]) # might differ from ejamout1$sitecount_unique
    # # Get the name of the selected location
    selected_location_name_react <- NULL
  } else {
    ejamout1 <- ejamitout$results_bysite[sitenumber, ]
    if (!is.null(shp)) {
      shp <- shp[sitenumber, ]
    }
    nsites <- 1
    # # Get the name of the selected location
    selected_location_name_react <- ejamout1[sitenumber, "statename"]
  }
  include_ejindexes <- any(names_ej_pctile %in% colnames(ejamout1))

  if (!("valid" %in% names(ejamout1))) {ejamout1$valid <- TRUE}

  if (isTRUE(ejamout1$valid)) {

    popstr <- prettyNum(round(ejamout1$pop, table_rounding_info("pop")), big.mark = ',')

    ##################### #
    # the way adapted from app_server

    sitetype <- ejamitout$sitetype
    # sitetype <- tolower(submitted_upload_method) # did not work here
    if (sitetype == "shp" && is.null(shp)) {
      warning("Cannot map polygons based on just output of ejamit() -- The sf class shapefile / spatial data.frame that was used should be provided as the shp parameter to ejam2report()")
    }
    rad <- ejamout1$radius.miles

    residents_within_xyz <- report_residents_within_xyz(
      sitetype = sitetype,
      radius = rad,
      nsites = nsites,  # but should note these are only the ones where $results_bysite$valid %in% TRUE
      sitenumber = sitenumber,
      ejam_uniq_id = ejamout1$ejam_uniq_id
    )
    locationstr <- residents_within_xyz
    ## and could also add here ?
    # addlatlon = TRUE
    # if (addlatlon && sitetype == "latlon" && nsites == 1) {
    #   locationstr <- paste0(locationstr, ' Centered at ', ejamout1$lat, ', ', ejamout1$lon)
    # }
    ##################### #

    # > copy .Rmd (template), .png (logo), .css from Rmd_folder to a temp dir subfolder for rendering
    # report_setup_temp_files() copies files to where they need to be for rendering ####
    ## returns path to .Rmd template copied to a temp folder:


    tempReport <- report_setup_temp_files(
      # Rmd_name = 'community_report_template.Rmd', # default, for summary report
      # # Rmd_name = 'barplot_report_template.Rmd' # for single site barplot report
      # Rmd_folder = 'report/community_report/'
    )

    # use create_filename() here like server does:
    if (!is.null(selected_location_name_react)) {
      location_suffix <- paste0(" - ", selected_location_name_react) # the statename, if just 1 site not overall results
    } else {
      location_suffix <-  ""
    }
    if (is.null(filename)) {
      filename <- create_filename(
        file_desc = paste0('community report', location_suffix),
        title =  analysis_title,
        buffer_dist = rad,
        site_method = submitted_upload_method, # can be latlon, shp, SHP, fips, FIPS, MACT, etc. (just used as-is in filename)
        with_datetime = FALSE,
        ext = fileextension # in server,  ifelse(input$format1pager == 'pdf', '.pdf', '.html')
      )
      temp_comm_report <- file.path(tempdir(), filename)

    } else {
      temp_comm_report <- filename
    }

    output_file      <- temp_comm_report

    if (return_html) {
      temp_comm_report_or_null <- NULL
    } else {
      temp_comm_report_or_null <- temp_comm_report
    }
    ####################################################### #

    # build_community_report() does most of this work ####

    ## note build_community_report() is also used in community_report_template.Rmd and in server

    x <- build_community_report(

      output_df = ejamout1,
      analysis_title = analysis_title,
      totalpop = popstr,
      locationstr = locationstr,
      include_ejindexes = include_ejindexes,

      show_ratios_in_report = show_ratios_in_report,

      extratable_title = extratable_title,
      extratable_title_top_row = extratable_title_top_row,
      extratable_list_of_sections = extratable_list_of_sections,
      extratable_show_ratios_in_report = extratable_show_ratios_in_report,
      extratable_hide_missing_rows_for = extratable_hide_missing_rows_for,

      in_shiny = FALSE,
      filename = temp_comm_report_or_null, # passing NULL should make it return the html object

      report_title = report_title,
      logo_path = logo_path,
      logo_html = logo_html
    )

    ## seems like using cat() was a simpler approach tried initially: ***
    ##  that would write just the basics of it to the temp location, not needing render()
    ##  but the render() approach also add the map and plot !!
    # cat(x, file = temp_comm_report)

    rmd_template <- system.file("report/community_report/combine_after_build_community_report.Rmd", package = "EJAM")

    #Barplot from community report
    plot <- plot_barplot_ratios_ez(ejamitout) + ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2))

    # This presumes shp was provided in both FIPS and SHP cases - otherwise only maps them as points! (does not try to obtain polygon(s) from FIPS when shp not provided here)
    if (is.null(sitenumber) || length(sitenumber) == 0) {
      # Map from community report should be ALL the sites that were passed here, UNLESS sitenumber param was used to pick 1
      if (sitetype %in% c("fips", "shp") && !is.null(shp)) {
        map <- ejam2map(ejamitout = ejamitout$results_bysite, shp = shp, launch_browser = FALSE) # it figures out radius if used
      } else {
        map <- mapfastej(ejamitout)
      }
    } else {
      # just 1 site specified by sitenumber so map should show just that 1 site! shp and ejamout1 both 1 row already in this case
      if (sitetype %in% c("fips", "shp") && !is.null(shp)) {
        map <- ejam2map(ejamitout = ejamout1, shp = shp, launch_browser = FALSE) # it figures out radius if used
      } else {
        map <- mapfastej(ejamout1)
      }
    }
    if (is.null(map)) {
      report_params <- list(
        community_html = x,
        plot = plot

      )
    } else {
      report_params <- list(
        community_html = x,
        plot = plot,
        map = map
      )
    }
    # >>>>>>> issue303AddMapAndBarPlot
    if (return_html) {
      rendered_path <- rmarkdown::render(
        input = rmd_template,
        output_format = "html_document",   #   pdf option not relevant here
        output_file = tempfile(fileext = ".html"),  #  not output_file since here you do not care about or see the filename
        params = report_params,
        envir = new.env(parent = globalenv()),
        quiet = TRUE
      )

      return(paste(readLines(rendered_path, warn = FALSE), collapse = "\n"))

    } else {

      rmarkdown::render(
        input = rmd_template,
        output_format = ifelse(fileextension == ".pdf", "pdf_document", "html_document"), # add pdf option here
        output_file = output_file,
        params = report_params,
        envir = new.env(parent = globalenv()),
        quiet = TRUE
      )
      output_file <- normalizePath(output_file) # allows it to work on MacOS, e.g.
      if (interactive() & !shiny::isRunning()) {
        cat("file saved at", output_file, '\n')
        cat("To open that folder from R, you could copy/paste this into the RStudio console:\n")
        cat(paste0("browseURL('", dirname(output_file),"')"), '\n')
      }

      if (launch_browser && !shiny::isRunning()) {
        browseURL(output_file)
      }

      return(output_file)
    }

    ########################################################################################### #
    ## can also generate reports through knitting Rmd template
    ## this is easier to add in maps and plots but is slower to appear

    #   ## pass params to customize .Rmd doc  # ###

    #browseURL(temp_comm_report)

  } else {
    rstudioapi::showDialog(title = 'Report not available',
                           'Individual site reports not yet available.')
    return(NA)
  }
}
########################################################################################### #
