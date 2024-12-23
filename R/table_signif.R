

#' Round numbers in a table, each column to appropriate number of significant digits
#'
#' @param dat data.frame or data.table of numbers
#' @param digits vector as long as number of columns in dat,
#'   or use default which is to get the number of significant digits from
#'   varinfo(colnames(dat), 'sigfigs')$sigfigs which gets it from
#'   map_headernames dataset of metadata on EJAM/EJScreen indicators.
#' @seealso [table_signif_round_x100()] [table_signif()] [table_round()] [table_x100()]
#' @return table same size as dat
#' @examples
#' out <- testoutput_ejamit_10pts_1miles
#' mytable <- out$results_bysite[1:2, ..names_these]
#' table_signif_round_x100(mytable)
#' # same as this:
#' table_signif(
#'   table_round(
#'     table_x100(
#'       mytable, names_pct_as_fraction_ejamit
#'     )
#'   )
#' )
#' 
#' 
#' @keywords internal
#'
table_signif <- function(dat, digits = NULL) {
  
  if (missing(digits) || is.null(digits) || any(digits %in% 'ejscreen')) {
    suppressWarnings({
      # digits <- as.numeric(esigfigs.api[match(colnames(dat), esigfigs.api$evar), 'sigfigs'])
      digits <- as.numeric(varinfo(colnames(dat), 'sigfigs')$sigfigs)
    })
  }
  
  signifarray.api <- function(dat, digits = NULL) {
    if (!(is.data.frame(dat))) {dat <- as.data.frame(dat)}
    if (length(digits) != NCOL(dat)) {
      if (length(digits) == 1) {
        digits <- rep(digits, NCOL(dat))
      } else {
        warning("length of digits parameter does not match number of columns in dat. cannot set significant digits.")
        return(dat)
      }
    }
    # digits must be NA for any non-numeric columns for this to work
    digits[!is.numericish(dat)] <- NA
    dat[,!is.na(digits)] <- mapply(FUN = signif, x = dat[,!is.na(digits)], digits = digits[!is.na(digits)])
    # dat <- mapply(FUN = signif, x = dat, digits = digits)
    return(dat)
  }
  
  return(
    signifarray.api(dat = dat, digits = digits)
  )
}
############################################################################# #  
