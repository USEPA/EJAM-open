

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
#' @param report_title generic name of this type of report, to be shown at top, like "EJAM Multisite Report"
#' @param logo_path optional relative path to a logo for the upper right of the overall header.
#'   Ignored if logo_html is specified and not NULL, but otherwise uses default or param set in run_app()
#' @param logo_html optional HTML for img of logo for the upper right of the overall header.
#'   If specified, it overrides logo_path. If omitted, gets created based on logo_path.
#' @return can return HTML if filename not specified, but otherwise NULL
#'
#' @keywords internal
#' @export
#'
build_barplot_report <- function(analysis_title, totalpop, locationstr,
                                   in_shiny = FALSE, filename = NULL,
                                 report_title = NULL,
                                 logo_path = NULL,
                                 logo_html = NULL
) {

  full_page <- paste0(
    generate_html_header(analysis_title = analysis_title,
                         totalpop = totalpop, locationstr = locationstr,
                         in_shiny = in_shiny,
                         report_title = report_title,
                         logo_path = logo_path,
                         logo_html = logo_html),
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
