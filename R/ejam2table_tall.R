
#' Simple quick look at results of ejamit() in RStudio console
#'
#' @param ejamitout like from ejamit() or doaggregate()
#' @param sitenumber if omitted, shows results_overall, and 
#'   if an integer, identifies which site (which row) to show from results_bysite
#' @return data.frame with one indicator per row
#' 
#' @export
#'
ejam2table_tall <- function(ejamitout, sitenumber) {
  if (missing(sitenumber)) {
    table_tall_from_overall(ejamitout)
  } else {
    table_tall_from_bysite(ejamitout$results_bysite, sitenumber = sitenumber)
  }
}
############################################### #


#' Format the results_overall part of the output of ejamit() or doaggregate()
#' 
#' Take a quick look at results in the RStudio console
#' 
#' @param results_overall data.table of 1 row, from output of ejamit() or doaggregate()
#' @param longnames vector of names of variables in results_overall, 
#'   from output of ejamit() or doaggregate()
#'
#' @return data.frame with one indicator per row
#' @examples 
#'  table_tall_from_overall(testoutput_ejamit_10pts_1miles$results_overall)
#'  table_tall_from_overall(x$results_bysite[1, ])
#'  
#' @keywords internal
#'
table_tall_from_overall <- function(results_overall, longnames = NULL) {
  # just a slightly easier to read view of the results
  
  if (is.null(longnames)) {longnames <- fixcolnames(names(results_overall), "r", "long")}
  
  if (is.list(results_overall) & "results_overall" %in% names(results_overall)) {
    if (is.data.frame(results_overall$results_overall)) {
      # looks like the entire output of ejamit() or doaggregate() was passed as 1st param, not just the 1 table needed
      if ("results_overall" %in% names(results_overall) & "longnames" %in% names(results_overall)) {
        longnames <- results_overall$longnames
        results_overall <- results_overall$results_overall
      } else {
        warning("requires results_overall and longnames, such as from output of ejamit() or doaggregate() ")
        return(NULL)
      }
    }
  } else {
    if (missing(longnames)) {
      warning("requires longnames")
    }
  }
  
  x <- copy(results_overall)
  x <- table_signif_round_x100(x) 
  # x <- table_x100(x, cnames = names_pct_as_fraction_ejamit)
  # x <- table_round(x)
  
  x <- as.vector(unlist(x))
  x[!is.numericish(x)] <- NA # easier to just drop the info like state name/abbrev.
  # a vector has to be numeric or not, cannot have just some elements numeric some char.
  # x[is.numericish(x)] <- as.numeric(x[is.numericish(x)] )
  x  <- as.numeric(x  )
  x <- cbind(
    value = x, 
    indicator = longnames
  )
  x <- data.frame(x)
  return(x)
  # data.frame(table_tall_from_overall( table_round( ejamit_a$results_overall) ))
  
}
############################################### #

#' @keywords internal
#' 
table_tall_from_bysite <- function(results_bysite, sitenumber = 1) {
  table_tall_from_overall(results_bysite[sitenumber, ])
}
############################################### #
