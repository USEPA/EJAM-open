

#' Quick view of summary stats by type of stat, but lacks rounding specific to each type, etc.
#'
#' @param ejamitout list as from ejamit() that includes results_overall
#' @param sitenumber if NULL, uses overall results. If an integer, uses that site, 
#'   based on just one row from ejamitout$results_bysite
#' @param decimals optional number of decimal places to round to
#' @return prints to console and returns a simple data.frame
#' @examples 
#'  ejam2barplot(testoutput_doaggregate_100pts_1miles)
#'  ejam2ratios(testoutput_ejamit_100pts_1miles)
#'
#' @export
#'
ejam2ratios <- function(ejamitout,
                         sitenumber = NULL,
                          # vartypes = c("ratio", "pctile", "pct", "all")[1],
                          # stats = c("Average site", "Average person", "Median site", "Median person", "Min", "Max"),
                          decimals = 1) {

  ## quick view of summary stats by type of stat, but lacks rounding specific to each type, etc.
  ## table_ratios_from_ejamit(testoutput_ejamit_100pts_1miles)

  if (!is.null(sitenumber)) {
    df = ejamitout$results_bysite[sitenumber, ]
  } else {
    df = ejamitout$results_overall
  }
  if (!all(c(names_these_ratio_to_avg) %in% names(df))) {stop("some of the ratios were not found as column names in the table")}
  
  x <-data.frame(
    Ratio_to_US_avg    = round(unlist(df[, ..names_these_ratio_to_avg]), table_rounding_info(names_these_ratio_to_avg)),
    Ratio_to_State_avg = round(unlist(df[, ..names_these_ratio_to_state_avg]), table_rounding_info(names_these_ratio_to_state_avg))
    )

  rownames(x) <- fixcolnames(names_these, "r", "shortlabel")
  cat("\n\nAverage Resident in Place(s) Analyzed vs US or State\n\n")
  return(x)

}
####################################################################### #


#' Quick view of summary stats by type of stat, but lacks rounding specific to each type, etc.
#'
#' @param ejamitout list as from ejamit() that includes results_overall
#' @param sitenumber if NULL, uses overall results. If an integer, uses that site, 
#'   based on just one row from ejamitout$results_bysite
#' @param decimals optional number of decimal places to round to
#'
#' @return prints to console and returns a simple data.frame
#'
#' @export
#'
table_ratios_from_ejamit <- function(ejamitout,
                                     sitenumber = NULL,
                                     # vartypes = c("ratio", "pctile", "pct", "all")[1],
                                     # stats = c("Average site", "Average person", "Median site", "Median person", "Min", "Max"),
                                     decimals = 1) {
  if (is.null(sitenumber)) {
    ejam2ratios(ejamitout = ejamitout,                    decimals = decimals)
  } else {
    ejam2ratios(ejamitout = ejamitout, sitenumber = sitenumber, decimals = decimals)
  }
}
####################################################################### #
