############################################################################# #

#' Round numbers in a table, each column to appropriate number of decimal places
#' @details Percentages stored as 0 to 1 rather than 0 to 100 will not be shown correctly unless adjusted, 
#' because rounding info says 0 digits when the intent is to show 0 digits after the 0-100 percent number.
#' @param x data.frame, data.table, or vector with at least some numerical columns, like the results
#'   of ejamit()$results_bysite
#' @param var optional, but assumed to be names(x) by default, specifies colnames of table
#'   or names of vector elements, within x
#' @param varnametype optional, name of column in map_headernames that is looked in for var
#' @param ... passed to [is.numericish()] 
#' @seealso [is.numericish()] [table_rounding_info()]
#' @return Returns the original x but with appropriate cells rounded off.
#' @examples  
#'   table_round(c(12.123456, 9, NA ), 'pm')
#' 
#'  x <- testoutput_ejamit_10pts_1miles$results_bysite[
#'    1:2, c('lat','lon', 'pop', names_these, names_these_ratio_to_avg, names_e_pctile), 
#'    with = FALSE
#'  ]
#' 
#'  table_rounding_info(names(x))
#' 
#'  table_round(x)
#' 
#' @keywords internal
#'
table_round <- function(x, var = names(x), varnametype="rname", ...) {
  
  # See the internal helper function  round2nearest_n()  which lets you explicitly round to nearest 100, e.g.
  
  # warning("Percentages stored as 0 to 1 rather than 0 to 100 will not be shown correctly unless adjusted, 
  #         because rounding info says 0 digits when the intent is to show 0 digits after the 0-100 percent number.")
  
  # treat a vector differently than a matrix/data.frame/data.table
  # even if those nonvectors are just 1 row (multiple indicators) like results_overall,
  # or just 1 column (single indicator) of a table (e.g., subset of df where drop=F)
  # For a vector we might want to round each element differently and maybe only some are even roundable.
  # For a table, each column is treated as an indicator where it is roundable and rounded just 1 way for all rows of the column.
  
  dig <- table_rounding_info(var = var, varnametype = varnametype)
  roundable <- is.numericish(x, ...) 
  roundable[is.na(dig)] <- FALSE # if NA was returned as the number of digits to round to, dont try to round that one
  if (!any(roundable)) {
    warning('none of the columns of x = ', deparse1(substitute(x)),' appear to be roundable, so it is being returned unchanged')
    return(x)
  }
  
  if (is.vector(x)) {
    #  names were provided using var parameter
    x[roundable] <- round(
      x[roundable],
      dig)
    return(x)
    
  } else {
    # table, not  a vector
    if (data.table::is.data.table(x)) {data.table::setDF(x); wasdt <- TRUE} else {wasdt <- FALSE}
    
    for (i in 1:sum(roundable)) {
      x[ , roundable][ , i] <- round(x[ , roundable][ , i], dig[roundable][i])
    }
    if (wasdt) {data.table::setDT(x)}
    return(x)
  }
}
############################################################################# #
