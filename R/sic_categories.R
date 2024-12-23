#' See the names of SIC industrial categories and their codes
#'
#' Easy way to view, in RStudio console, the SIC categories.
#'   SIC all are 4-digit codes, like 7218 - Industrial launderers
#' @return matrix of 1 column, with rownames like
#'   "7353 - Heavy construction equipment rental (411 sites)"
#'   and values like "7353"
#' @seealso   [SIC] [naics_categories]
#'
#' @export
#'
sic_categories <- function() {
  cbind(EJAM::SIC)
}
