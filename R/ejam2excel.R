
#' Save EJAM results in a spreadsheet
#'
#' @description ejam2excel() takes the output of something like ejamit() and
#' creates a spreadsheet with an overall summary tab, a site by site table tab,
#' as well as other tabs such as map, plot, notes, etc.
#' 
#' @return returns a workbook object for use by openxlsx::saveWorkbook(wb_out, pathname)
#'   or returns just the full path/file name of where it was saved if save_now = TRUE
#' 
#' @param ejamitout output of [ejamit()] 
#' @param mapadd Logical option for including a map of the points
#' @param fname optional name or full path and name of file to save locally, like "out.xlsx" 
#' @param save_now optional logical, whether to save as a .xlsx file locally or just return workbook object
#'   that can later be written to .xlsx file using [openxlsx::saveWorkbook()]
#' @param overwrite optional logical, passed to [openxlsx::saveWorkbook()]
#' @param launchexcel optional logical, passed to [table_xls_format()], whether to launch browser to see spreadsheet immediately
#' @param interactive_console optional - should set to FALSE when used in code or server. If TRUE,
#'   prompts RStudio user interactively asking where to save the downloaded file
#' @param ok2plot optional logical, passed to  [table_xls_format()], whether safe to try and plot or set FALSE if debugging plot problems
#' @param in.testing optional logical
#' @param radius_or_buffer_in_miles optional radius in miles
#' @param buffer_desc description of location to use in labels, like "Selected Locations"
#' @param in.analysis_title optional title as character string
#' @param react.v1_summary_plot optional - a plot object
#' @param radius_or_buffer_description optional text phrase describing places analyzed
#' @param hyperlink_colnames optional names of columns with URLs
#' @param site_method site selection method, such as SHP, latlon, FIPS, NAICS, FRS, EPA_PROGRAM, SIC, MACT
#'   optional site method parameter used to create a more specific title with create_filename.
#'   Note `ejamitout$sitetype` is not quite the same as the `site_method` parameter used in building reports.
#'   sitetype can be latlon, fips, or shp
#'   site_method can be one of these: SHP, latlon, FIPS, NAICS, FRS, EPA_PROGRAM, SIC, MACT
#' @param mapadd logical option for including a map of the points 
#' @param ... optional additional parameters passed to [table_xls_format()], such as 
#'   heatmap_colnames, heatmap_cuts, heatmap_colors, etc.
#' @examples
#' \donttest{
#' # Add purple to flag indicators at 99th percentile
#' ejam2excel(testoutput_ejamit_10pts_1miles, 
#'   # View spreadsheet 1st without saving it as a file
#'   launchexcel = T, save_now = F, 
#'   heatmap_cuts = c(80, 90, 95, 99), 
#'   heatmap_colors  = c("yellow", "orange", "red", "purple"), 
#'   # Apply heatmap to only a few of the ratio columns
#'   heatmap2_colnames = names_d_ratio_to_state_avg)
#' }
#' 
#' @export
#'
ejam2excel <- function(ejamitout,
                       fname = NULL, # full path and name, or just name of .xlsx file
                       save_now = TRUE, 
                       overwrite = TRUE, 
                       launchexcel = FALSE,
                       interactive_console = TRUE,
                       ok2plot = TRUE,
                       in.testing = FALSE,
                       in.analysis_title =  "EJAM analysis",
                       react.v1_summary_plot = NULL,
                       radius_or_buffer_in_miles = NULL,  #  input$bt_rad_buff
                       buffer_desc = "Selected Locations",
                       radius_or_buffer_description = 'Miles radius of circular buffer (or distance used if buffering around polygons)',
                       # radius_or_buffer_description =   "Distance from each site (radius of each circular buffer around a point)",
                       hyperlink_colnames = "ECHO Report",#c("EJScreen Report", "EJScreen Map","ACS Report", "ECHO Report"),
                       site_method = "",
                       mapadd = FALSE,
                       community_reportadd = TRUE,
                       community_html = NULL,
                       ...
) {

  if (mapadd == T) {
    report_map <- ejam2map(ejamitout, radius = radius_or_buffer_in_miles)
  } else {
    report_map <- NULL
  }
  x <- table_xls_from_ejam(
    ejamitout = ejamitout,
    fname = fname,
    save_now = save_now,
    overwrite = overwrite,
    launchexcel = launchexcel,
    interactive_console = interactive_console,
    ok2plot = ok2plot,
    in.testing = in.testing,
    in.analysis_title = in.analysis_title,
    react.v1_summary_plot = react.v1_summary_plot,
    radius_or_buffer_in_miles = radius_or_buffer_in_miles,
    buffer_desc = buffer_desc,
    radius_or_buffer_description = radius_or_buffer_description,
    hyperlink_colnames = hyperlink_colnames,
    site_method = site_method,
    mapadd = mapadd,
    report_map = report_map,
    community_reportadd = community_reportadd,
    community_html = community_html,
    ...
  )
  invisible(x)
}
