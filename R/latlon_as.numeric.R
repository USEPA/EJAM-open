#' Strip non-numeric characters from a vector
#' 
#' @description Remove all characters other than minus signs, decimal points, and numeric digits
#' @details Useful if latitude or longitude vector has spaces, tabs, etc. 
#'   CAUTION - Assumes stripping those out and making it numeric will fix whatever problem there was 
#'   and end result is a valid set of numbers. Inf etc. are turned into NA values.
#'   Empty zero length string is turned into NA without warning. NA is left as NA.
#'   If anything other than empty or NA could not be interpreted as a number, it 
#'   returns NA for those and offers a warning.
#' @param x vector of something that is supposed to be numbers like latitude or longitude
#'   and may be a character vector because there were some other characters like tab or space or percent sign or dollar sign 
#' @seealso latlon_df_clean() latlon_infer() latlon_is.valid() latlon_as.numeric()
#' @return numeric vector same length as x
#'
#' @examples   
#'   latlon_as.numeric(c("-97.179167000000007", " -94.0533", "-95.152083000000005"))
#'   latlon_as.numeric(-3:3)
#'   latlon_as.numeric(c(1:3, NA))
#'   latlon_as.numeric(c(1, 'asdf'))
#'   latlon_as.numeric(c(1, ''))
#'   latlon_as.numeric(c(1, '', NA))
#'   latlon_as.numeric(c('aword', '$b'))
#'   latlon_as.numeric(c('-10.5%', '<5', '$100'))
#'   latlon_as.numeric(c(Inf, 1))
#'
#' @keywords internal
#'
latlon_as.numeric <- function(x) {

  if (missing(x)) {
    warning('No value provided for argument "x".')
    return(NULL)
  }

  if (!is.null(dim(x)) || !is.atomic(x) || is.null(x) || length(x) == 0) {
    if (shiny::isRunning()) {
      warning('latlon_as.numeric(x) expects x to be a vector like 1:10 or df$mycol, not a data.frame, list, or anything else.')
      return(NA)
    } else {
      stop('latlon_as.numeric(x) expects x to be a vector like 1:10 or df$mycol, not a data.frame, list, length zero, or anything else.')
    }
  }

  oldx <- x
  x <- (gsub('[^012345678.9-]', '',  x))
  # NOTE THIS does not warn if NA was an input but does warn if other stuff was input (presumably intended as numbers) that gets turned into NA:
  if (any(x[!is.na(oldx)] == '' & oldx[!is.na(oldx)] != '')) {
    warning('After stripping non numeric characters via latlon_as.numeric(x), no actual number was left in at least some elements of x, and returning NA in those cases.')
  }
  return(as.numeric(x))
  
  # removes anything other than numeric characters used in lat or lon, so retain only minus signs, decimal points, and numeric digits.
  # may want to fix crash when read lat or lon that has any characters in it besides minus, decimal, and number, like not just whitespace but also tab or other strange characters...
  # as.numeric(trimws(x)) # would do only part of this.
}
