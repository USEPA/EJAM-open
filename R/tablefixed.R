#' Table of counts of integer values zero through maxbin
#' 
#' @description Like tabulate or table, sort of, but includes zero unlike tabulate,
#'   and lets you ensure results include every integer 0 through maxbin,
#'   so you can, for example, easily combine tables of counts where some
#'   did not include all integers.
#' @details There is likely a more efficient way to do this in some existing package,
#'   but this is useful and fast enough.
#'
#'   When using a dataset like EJScreen with 13 indicators of interest,
#'   and counting how many of the 13 are above various cutpoints,
#'   there may be zero rows that have exactly 8 above some cutoff, for example.
#'
#'   This function makes it easier to combine those tables into a summary
#'   where 0-13 are in each table while table() would only return integers
#'   that came up in a given case (for one cutoff).
#' @param x vector of integers, like counts, that can include 0
#' @param maxbin highest integer among x, or number of bins
#' @seealso colcounter_summary()
#' @return summary table
#' @export
#'
tablefixed <- function(x, maxbin=NULL) {
  # returns a table of counts of integer values zero through maxbin
  maxvalue_or_1 <- max(1, x, na.rm = TRUE)
  if (is.null(maxbin)) {maxbin <- maxvalue_or_1}
  if (maxbin < maxvalue_or_1){
    warning('invalid maxbin - some values are larger')
    return(NULL)
  }
  mycounts <- tabulate(x, nbins = maxvalue_or_1)
  
  zerocount <- sum(x == 0, na.rm = T)
  abovemaxcounts <- rep(0, maxbin - max(1, x, na.rm = T))
  x <- c(zerocount, mycounts, abovemaxcounts)
  
  names(x) <- 0:(maxbin)
  return(x)
}
######################################## #
