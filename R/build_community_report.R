

#' Generate EJScreen Multisite (EJAM) Summary Report in HTML within shiny app
#' 
#' Creates a 2 page report on overall results or for one site, with demographic and environmental indicators, and EJ Indexes if needed.
#' 
#' @details For the same function for use in RStudio, 
#' see [ejam2report()] which relies on `build_community_report()`
#' 
#' This function gets called by 
#'  app_server,
#'  ejam2report(), and
#'  community_report_template.Rmd
#' 
#' @param output_df, single row of results table from doaggregate - either results_overall or one row of bysite
#' @param analysis_title, title to use in header of report
#' @param totalpop, total population included in location(s) analyzed
#' @param locationstr, description of the location(s) analyzed
#' @param include_ejindexes, whether to build tables for EJ and EJ supp. indexes
#' @param in_shiny, whether the function is being called in or outside of shiny - affects location of header
#' @param filename, path to file to save HTML content to; if null, returns as string (used in Shiny app)
#' 
#' @keywords internal
#' @export
#' 
build_community_report <- function(output_df, analysis_title, totalpop, locationstr, 
                                   include_ejindexes = FALSE, in_shiny = FALSE, 
                                   # ejscreen_vs_ejam_caveat = "Note: Some numbers as shown on the EJScreen report for a single location will in some cases appear very slightly different than in multisite reports. All numbers shown in both types of reports are estimates, and any differences are well within the range of uncertainty inherent in the American Community Survey data as used in EJScreen. Slight differences are inherent in very quickly calculating results for multiple locations.",
                                   # diesel_caveat = paste0("Note: Diesel particulate matter index is from the EPA's Air Toxics Data Update, which is the Agency's ongoing, comprehensive evaluation of air toxics in the United States. This effort aims to prioritize air toxics, emission sources, and locations of interest for further study. It is important to remember that the air toxics data presented here provide broad estimates of health risks over geographic areas of the country, not definitive risks to specific individuals or locations. More information on the Air Toxics Data Update can be found at: ",
                                   # url_linkify("https://www.epa.gov/haps/air-toxics-data-update", "https://www.epa.gov/haps/air-toxics-data-update")),
                                   filename = NULL
) {
  
  ## check that analysis was run with EJ columns; if not, don't add them
  if (include_ejindexes) {
    ejcols <- c(names_ej,names_ej_state, names_ej_supp,names_ej_supp_state)
    if (!(all(ejcols %in% names(output_df)))) {
      include_ejindexes <- FALSE
    } 
  }
  
  output_df_rounded <-   as.data.frame(output_df) 
  
  names_demog_index <- c(names_d[1:2], names_d_avg[1:2], names_d_state_avg[1:2])
  
  varlist <- union(unique(map_headernames$varlist), names_demog_index)
  
  ## iterate through variable lists in map_headernames to get column names
  expandedVarlist <- c()
  for (var in varlist) {
    if (exists(var) & !(var %in% expandedVarlist)) {
      expandedVarlist <- c(expandedVarlist, get(var))
    }
  }
  
  ## drop duplicates of variables found in multiple lists
  expandedVarlist <- unique(expandedVarlist)
  
  output_df_rounded <- format_ejamit_columns(output_df_rounded, expandedVarlist)
  
  full_page <- paste0(
    generate_html_header(analysis_title, totalpop, locationstr, in_shiny = in_shiny),
    generate_demog_header(),
    fill_tbl_full(output_df_rounded),
    collapse = ''
  )
  ## add EJ index and Supp EJ index tables 
  ## only if those columns are available
  if (include_ejindexes) {
    full_page <- paste0(full_page,
                        generate_ej_header(),
                        fill_tbl_full_ej(output_df_rounded), 
                        #generate_ej_supp_header(),
                        #fill_tbl_full_ej_supp(output_df_rounded),
                        collapse = '') 
  }
  
  full_page <- paste0(
    full_page,
    fill_tbl_full_subgroups(output_df_rounded),
    generate_report_footnotes(
      # ejscreen_vs_ejam_caveat = "Note: Some numbers as shown on the EJScreen report for a single location will in some cases appear very slightly different than in EJScreen's multisite reports. All numbers shown in both types of reports are estimates, and any differences are well within the range of uncertainty inherent in the American Community Survey data as used in EJScreen. Slight differences are inherent in very quickly calculating results for multiple locations.",
      diesel_caveat = paste0("Note: Diesel particulate matter index is from the EPA's Air Toxics Data Update, which is the Agency's ongoing, comprehensive evaluation of air toxics in the United States. This effort aims to prioritize air toxics, emission sources, and locations of interest for further study. It is important to remember that the air toxics data presented here provide broad estimates of health risks over geographic areas of the country, not definitive risks to specific individuals or locations. More information on the Air Toxics Data Update can be found at: ",
                             url_linkify("https://www.epa.gov/haps/air-toxics-data-update", "https://www.epa.gov/haps/air-toxics-data-update"))
    ),
    collapse = ''
  )
    
  if (is.null(filename)) {
    return(HTML(full_page))
  } else {
    junk <- capture.output({
      cat(HTML(full_page))
    })
  }
}
