
#' helper - copies template, css to tempdir for render of summary report
#' helper - copies .Rmd (template), .css from Rmd_folder to a temp dir subfolder for rendering
#' @details used by `ejam2report()` only now? was also copying  logo .png ? but not now?
#' @param Rmd_name .Rmd filename the package uses
#' @param Rmd_folder folder the package stores the template in
#'
#' @keywords internal
#'
report_setup_temp_files <- function(Rmd_name = 'community_report_template.Rmd',
                                    # or Rmd_name = 'barplot_report_template.Rmd' for single site barplot report
                                    Rmd_folder = 'report/community_report/') {
  tempReport <- file.path(tempdir( ), Rmd_name)
  if (!file.exists(app_sys(paste0(Rmd_folder, Rmd_name))) ||
      !file.exists(app_sys(paste0(Rmd_folder, 'communityreport.css'))) ||
      !file.exists(app_sys(file.path(Rmd_folder, 'main.css')))
      ## does not check for or handle logo .png
  ) {
    stop(paste0("Necessary files missing from ", app_sys(paste0(Rmd_folder))))
  }
  # ------------------------------------  maybe it still needs the logo file?
  # file.copy(from = app_sys(paste0(Rmd_folder,
  #### ???? ### EJAM:::global_or_param(".community_report_logo_file"))),
  #           to = tempReport, overwrite = TRUE)
  # ------------------------------------ .Rmd template file ----------------------------------------- -
  file.copy(from = app_sys(paste0(Rmd_folder, Rmd_name)),
            to = tempReport, overwrite = TRUE)
  # ------------------------------------ css  ----------------------------------------- -
  if (!('main.css' %in% list.files(tempdir()))) {
    file.copy(from = app_sys(file.path(Rmd_folder, 'main.css')),
              to = file.path(tempdir(), 'main.css'), overwrite = TRUE)
  }
  if (!file.exists(file.path(tempdir( ),        'communityreport.css'))) {
    file.copy(from = app_sys(paste0(Rmd_folder, 'communityreport.css')), # app_sys() is unexported by EJAM pkg
              to = file.path(tempdir( ),        'communityreport.css'), overwrite = TRUE)
  }
  return(tempReport)
}
########################################################### ############################################################ #
########################################################### ############################################################ #


#' Generate Single-site or Multi-site Summary Report (e.g., .html)
#'
#' Creates a short summary report with tables, map, and plot of indicators
#'
#' @details This is used by the shiny app server. For use in RStudio,
#' see [ejam2report()] (which relies on this).
#'
#' This function gets called by
#'  app_server but also by [ejam2report()],
#'  and also is used by the community_report_template.Rmd used to generate a report
#'
#'  It uses functions in community_report_helper_funs.R, etc.
#'
#' @param output_df single row of results table from doaggregate - either results_overall or one row of bysite
#' @param analysis_title title to use in header of report
#' @param totalpop total population included in location(s) analyzed
#' @param locationstr description of the location(s) analyzed
#'
#' @param include_ejindexes whether to build tables for summary indexes and supp. summary indexes
#' @param show_ratios_in_report logical, whether to add columns with ratios to US and State overall values, in main table of envt/demog. info.
#'
#' @param extratable_show_ratios_in_report logical, whether to add columns with ratios to US and State overall values, in an extra info table
#' @param extratable_title Text of overall title ABOVE the extra info table
#' @param extratable_title_top_row Text INSIDE the extra info table, top left cell
#' @param extratable_list_of_sections This defines what extra indicators are shown.
#'   It is a named list of vectors,
#'   where each name is text phrase that is title of a section of the table,
#'   and each vector is the vector of colnames of output_df that are indicators
#'   to show in that section, in extra table of demog. subgroups, etc.
#' @param extratable_hide_missing_rows_for only for the indicators named in this vector,
#'   leave out rows in table where raw value is NA,
#'   as with many of names_d_language, in extra table of demog. subgroups, etc.'
#'
#' @param in_shiny whether the function is being called in or outside of shiny - affects location of header
#' @param filename path to file to save HTML content to; if null, returns as string (used in Shiny app)
#' @param report_title generic name of this type of report, to be shown at top, like "EJAM Multisite Report"
#' @param logo_path optional relative path to a logo for the upper right of the overall header.
#'   Ignored if logo_html is specified and not NULL, but otherwise uses default or param set in run_app()
#' @param logo_html optional HTML for img of logo for the upper right of the overall header.
#'   If specified, it overrides logo_path. If omitted, gets created based on logo_path.
#'
#' @seealso [ejam2report()]
#'
#' @keywords internal
#' @export
#'
build_community_report <- function(output_df,
                                   analysis_title = "Report", totalpop, locationstr,
                                   include_ejindexes = FALSE,
                                   show_ratios_in_report = FALSE,
                                   extratable_show_ratios_in_report = FALSE,
                                   extratable_title = '',   # 'Additional Information', # above the table
                                   extratable_title_top_row = 'ADDITIONAL INFORMATION', # inside the table, top left cell
                                   extratable_list_of_sections = list(
                                     # see ejam2report defaults and see global_defaults_*.R
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
                                     # , `Count above threshold` = names_countabove # need to fix map_headernames longname and calctype and weight and drop 2 of the 6
                                   ),
                                   ## all the indicators that are in extratable_list_of_sections:
                                   extratable_hide_missing_rows_for = as.vector(unlist(extratable_list_of_sections)),

                                   in_shiny = FALSE,
                                   filename = NULL,
                                   report_title = NULL,
                                   logo_path = NULL,
                                   logo_html = NULL
) {

  ## check that analysis was run with EJ columns; if not, don't add them
  if (include_ejindexes) {
    ejcols <- c(names_ej,      names_ej_state,
                names_ej_supp, names_ej_supp_state)
    if (!(all(ejcols %in% names(output_df)))) {
      include_ejindexes <- FALSE
    }
  }

  output_df_rounded <-   as.data.frame(output_df)
  output_df_rounded <- format_ejamit_columns(output_df_rounded, names(output_df_rounded))
  if (missing(locationstr)) {
    warning('locationstr parameter missing')
    locationstr <- ""
  }
  if (missing(totalpop)) {
    if ("pop" %in% names(output_df_rounded)) {
      totalpop <- prettyNum(round(output_df_rounded$pop, 0), big.mark = ',')
    } else {
      warning('totalpop parameter or output_df_rounded$pop is required')
      totalpop <- "NA"
    }
  }

  full_page <- paste0(

    ############################################################# #

    # 1. Report/analysis overall header ####

    generate_html_header(analysis_title = analysis_title,
                         totalpop = totalpop, locationstr = locationstr,
                         in_shiny = in_shiny,
                         report_title = report_title,
                         logo_path = logo_path,
                         logo_html = logo_html
    ), #   report_title if in shiny is .community_report_title or outside shiny is eg "EJAM Multisite Report"

    ############################################################# #

    # 2. Envt & Demog table ####

    generate_env_demog_header(), # title = 'Environmental and Residential Population Indicators'

    fill_tbl_full(output_df = output_df_rounded,
                  show_ratios_in_report = show_ratios_in_report
    ),
    collapse = ''
  )

  ############################################################# #

  # 3. "Summary Index" table ####

  ## add Summary index and Supp Summary index tables
  ## only if those columns are available
  if (include_ejindexes) {
    full_page <- paste0(full_page,
                        generate_ej_header(),
                        fill_tbl_full_ej(output_df_rounded),
                        #generate_ej_supp_header(),
                        #fill_tbl_full_ej_supp(output_df_rounded),
                        collapse = '')
  }
  ############################################################# #

  # 4. Subgroups and Additional info table ####

  full_page <- paste0(
    full_page,
    fill_tbl_full_subgroups(output_df = output_df_rounded,
                            extratable_title         = extratable_title,         # above table
                            extratable_title_top_row = extratable_title_top_row, # inside table, e.g.,  'Additional Information' or 'Additional Indicators'
                            extratable_show_ratios_in_report = extratable_show_ratios_in_report,
                            list_of_sections      = extratable_list_of_sections,
                            hide_missing_rows_for = extratable_hide_missing_rows_for
    ),
    ############################################################# #

    # 5. footnote ####

    generate_report_footnotes(
      # ejscreen_vs_ejam_caveat = "Note: Some numbers as shown on the EJScreen report for a single location will in some cases appear very slightly different than in EJScreen's multisite reports. All numbers shown in both types of reports are estimates, and any differences are well within the range of uncertainty inherent in the American Community Survey data as used in EJScreen. Slight differences are inherent in very quickly calculating results for multiple locations.",
      diesel_caveat = paste0("Note: Diesel particulate matter index is from the EPA's Air Toxics Data Update, which is the Agency's ongoing, comprehensive evaluation of air toxics in the United States. This effort aims to prioritize air toxics, emission sources, and locations of interest for further study. It is important to remember that the air toxics data presented here provide broad estimates of health risks over geographic areas of the country, not definitive risks to specific individuals or locations. More information on the Air Toxics Data Update can be found at: ",
                             url_linkify("https://www.epa.gov/haps/air-toxics-data-update", "https://www.epa.gov/haps/air-toxics-data-update"))
    ),
    collapse = ''
  )
  ############################################################# #
  if (is.null(filename)) {
    return(HTML(full_page))
  } else {
    junk <- capture.output({
      cat(HTML(full_page))
    })
    # DO WE NEED TO RENDER  HERE? ***
    # OR CAN WE WRITE junk TO A .html FILE - WILL THAT WORK?
  }
}
