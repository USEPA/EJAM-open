#' utility to multiply certain percentage columns by 100 to convert 0-1.00 into 0-100
#' 
#' multiplies some data to rescale percentages stored as 0 to 1, into 0-100
#' 
#' @aliases fix_pctcols_x100
#' 
#' @param df data.frame but can be data.table
#' @param cnames colnames in df of indicators to multiply by 100, like those in
#'
#'   names_pct_as_fraction_ejamit,
#'
#'   names_pct_as_fraction_blockgroupstats, or
#'
#'   names_pct_as_fraction_ejscreenit
#'
#' @seealso [table_signif_round_x100()] [table_signif()] [table_round()] [table_x100()]
#' @return df with data in specified columns multiplied by 100
#'
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
#'  y = data.frame(pctlowinc = 1:2, pctpre1960 = 1:2, avg.pctunemployed = 1:2, avg.pctpre1960 = 1:2)
#'  
#'  table_x100(y, names_pct_as_fraction_ejscreenit)
#'  table_x100(y, names_pct_as_fraction_blockgroupstats)
#'  table_x100(y, names_pct_as_fraction_ejamit)
#'  cat("\n\n")
#'  names_pct_as_fraction_ejscreenit
#'  names_pct_as_fraction_blockgroupstats
#'  names_pct_as_fraction_ejamit
#'  cat("\n\n")
#'  ytable = data.table(pctlowinc = 1:2, pctpre1960 = 1:2, avg.pctunemployed = 1:2, avg.pctpre1960 = 1:2)
#'  
#'  table_x100(ytable, names_pct_as_fraction_blockgroupstats) 
#'  table_x100(ytable, names_pct_as_fraction_ejamit)
#'  cat("\n\n")
#'  y
#'  ytable
#'  
#' @keywords internal
#'
table_x100 <- function(df, cnames = names_pct_as_fraction_ejamit
                       # c(names_pct_as_fraction_blockgroupstats, 
                       #                names_pct_as_fraction_ejamit,
                       #                names_pct_as_fraction_ejscreenit) 
) {
  
  ## which percentage indicators are stored as 0-1.00 not 0-100 ?
  ## This will correct for different scaling in blockgroupstats and ejamit()$results_bysite, etc.
  
  # inefficient to pass the whole df here but should work
  
  if (missing(cnames)) {
    message("missing cnames parameter so assuming defaults should be used")
  }
  tofix <- names(df)[names(df) %in% cnames]
  
  if (is.data.table(df)) {
    
    ## This way would be only slightly faster, using data.table approach, 
    ## saving about  0.01 seconds for 1,000 points dataset with cnames = names_pct_as_fraction_ejamit
    ## but would update the data.table in the calling envt by reference 
    ## rather than just returning an updated copy, which may be unexpected.
    #
    # df[ , (tofix) := lapply(.SD, function(z) z * 100), .SDcols = tofix] 
    
    setDF(df)
    df[ , tofix] <- df[ , tofix] * 100
    setDT(df)
    return(df)
    
  } else {
    df[ , tofix] <- df[ , tofix] * 100
    return(df)
  }
}
############################################################################# #  


fix_pctcols_x100 <- function(df, cnames = names_pct_as_fraction_ejamit
                             # c(names_pct_as_fraction_blockgroupstats, 
                             #                names_pct_as_fraction_ejamit,
                             #                names_pct_as_fraction_ejscreenit) 
) {
  # just an alias for, and the prior name of, table_x100()
  
  table_x100(df = df, cnames = cnames)

}
############################################################################# #  
