

#' Generate HTML Page for Summary Barplot Report in shiny app
#' 
#' Creates header and footer of 1 page report to include a barplot on results for one site (to supplement the EJScreen Community Report)
#' 
#' @details For a related function for use in RStudio, 
#' see [ejam2report()] which relies on `build_community_report()`
#' 
#' 
#' @param analysis_title, title to use in header of report
#' @param totalpop, total population included in location(s) analyzed
#' @param locationstr, description of the location(s) analyzed
#' 
#' @param in_shiny, whether the function is being called in or outside of shiny - affects location of header
#' @param filename, path to file to save HTML content to; if null, returns as string (used in Shiny app)
#' 
#' @keywords internal
#' @export
#' 
#' 
build_barplot_report <- function(analysis_title, totalpop, locationstr, 
                                   in_shiny = FALSE, filename = NULL
) {
  
  full_page <- paste0(
    generate_html_header(analysis_title, totalpop, locationstr, in_shiny = in_shiny),
    # generate_demog_header(),
    # generate_report_footnotes(), # the footnotes about dpm and estimates are not really relevant here
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
