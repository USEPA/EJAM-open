

#' see which columns seem numeric and could be rounded, e.g. - DRAFT NOT FULLY TESTED
#' @details
#' Does not report "08" as numeric-ish, so cannot check fips with leading zero this way, e.g.
#' To do that you could check if is.na(as.numeric(x)) 
#' but "08" is not roundable.
#' @seealso [is.numeric.text()] 
#' @param x data.table, data.frame, or vector
#' @param only.if.already.numeric logical, if TRUE, only reports TRUE for a column (or element) if  is.numeric() is TRUE for that one 
#' @param strip.characters.before.coerce logical, if TRUE, tries to remove spaces and percentage signs before trying to coerce to numeric
#' @seealso [table_round()] 
#' @return logical vector as long as NCOL(x) i.e., is length(x), if x is table, or length(x) if vector
#' 
#' @keywords internal
#' 
is.numericish <- function(
    
  x, only.if.already.numeric=FALSE, strip.characters.before.coerce=FALSE) {
  # treat a vector differently than a matrix/data.frame/data.table
  # even if those nonvectors are just 1 row (multiple indicators) like results_overall,
  # or just 1 column (single indicator) of a table (e.g., subset of df where drop=F)
  # For a vector we might want to round each element differently and maybe only some are even roundable.
  # For a table, each column is treated as an indicator where it is roundable and rounded just 1 way for all rows of the column.
  
  roundable_by_element_of_vector <- function(vect) {
    function(x) { !inherits(try(round(x), silent = TRUE), "try-error")}
  }
  
  
  if (is.vector(x)) {
    #  names were provided using var parameter
    
    if (only.if.already.numeric) {
      return(is.numeric(x))
    } else {
      
      if (strip.characters.before.coerce) {
        warning('strip.characters.before.coerce=T is not implemented yet')
        ### try to remove spaces, percent signs, etc. like latlon_as.numeric() does?
        # x <- latlon_as.numeric(x) # not tested at all ***
      }
      
      # note as.numeric() will convert integer vector to numeric vector, which are
      # not identical() but are all.equal()
      
      # if it is just NA, treat it as potentially a number.
      # and then if as.numeric() results in an NA when it was NOT initially NA, it is truly not a potential number
      x[is.na(x)] <- 0
      answer <-  suppressWarnings(x == as.numeric(x))
      answer[is.na(answer)] <- FALSE # will not report NA as roundable, possibly numeric-
      # it is NA when failed to do as.numeric, and also if it started as NA which arguably can be made numeric
      
      return(answer)
    }
    
  } else {
    # table not vector    *** could optimize / vectorize for speed here:
    
    if (data.table::is.data.table(x)) {data.table::setDF(x); wasdt <- TRUE} else {wasdt <- FALSE}
    
    answer <- vector(length = NCOL(x))
    x[is.na(x)] <- 0 # this works cell by cell in a table, faster than in loop
    for (i in 1:NCOL(x)) {
      
      answer[i] <-  all(suppressWarnings(x[ , i] == as.numeric(x[ , i])))
      answer[i][is.na(answer[i])] <- FALSE  # will not report a vector of NAs as roundable, possibly numeric
    }
    
    if (wasdt) {data.table::setDT(x)}
    return(answer)
  }
  
}
############################################################################# #

