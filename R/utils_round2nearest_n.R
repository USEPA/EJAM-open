


#' Round numbers to the nearest N (e.g., to the nearest 100)
#' round2nearest_n(x, n = 100) is easier to understand than equivalent round(x, digits = -2
#' @param x vector of numbers to round. NA returns as NA.
#' @param n Single number. To nearest what? e.g., 100.
#'   Or vector as long as x, but it may be confusing to round each x to a different degree,
#'   or a vector that gets recycled but with warning.
#'   NA would return NA.
#' @return vector as long as x
#' @examples
#' # For rounding to nearest n when n > 1, using round() is a little confusing:
#' # round to nearest 100
#' round2nearest_n(1534, 100); round2nearest_n(9161, 100)
#' round(1534, -2); round(9161, -2)
#' 
#' # round to nearest 10
#' round2nearest_n(1534, 10); round2nearest_n(9161, 10)
#' round(1534, -1); round(9161, -1)
#' 
#' # When n < 1, it easy to just use round()
#' # round to 0 decimals
#' round2nearest_n(15.3, 1); round2nearest_n(91.61, 1)
#' round(15.3, 0); round(91.61, 0)
#' 
#' # round to 1 decimal
#' round2nearest_n(0.153, 0.1); round2nearest_n(0.9161, 0.1)
#' round(0.153, 1); round(0.9161, 1)
#' 
#' @keywords internal
#' @noRd
#' 
round2nearest_n = function(x, n = 100) {
  
  r1 = round(x / n, 0) * n
  
  r2 = round(x, digits = -1 * log10(n))  # same thing.
  if (!all.equal(r1, r2)) {stop("unexpected results")}
  cat("Note that 'round2nearest_n(x, n)' is the same as 'round(x, digits = -log10(n)' \n")
  
  return(r1)
}
