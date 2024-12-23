

#' Clean table of EJAM numbers: signif digits, rounding, scaling as 0-100%
#' Does table_signif() and table_round() and fix_pctcols_x100() in one call.
#' @param x data.frame or data.table
#' @param cnames use default when formatting output like ejamit()$results_bysite
#' @seealso [table_signif_round_x100()] [table_signif()] [table_round()] [table_x100()]
#' @return table of same shape as x
#' @examples
#' out <- testoutput_ejamit_10pts_1miles$results_bysite
#' table_signif_round_x100(
#'   out[1:2, ..names_these]
#' )
#' 
#' @keywords internal
#'
table_signif_round_x100 <- function(x, cnames = names_pct_as_fraction_ejamit) {
  
  table_signif(
    table_round(
      table_x100(
        x, cnames = cnames
      )
    )
  )
}
############################################################################# #
