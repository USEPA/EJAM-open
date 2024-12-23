
# DRAFT utility to check if vector is positive whole numbers stored as text like "01"

is.numerals.only =  function(x, trim_ = FALSE, decimalsok = FALSE, negativeok = FALSE) {
  
  # DRAFT
  
  # IGNORES NA values so can be TRUE even if some are NA
  
  if (trim_) {x = trimws(x)}
  
  if (decimalsok) {
    warning("not implemented yet")
    # cannot simply remove all decimals and treat  "3.3.3" as valid number stored as text
  }
  
  if (negativeok) {
    warning("not implemented yet")
    # NOTE this would be complicated to implement because of the - sign being the same as subtraction 
    # so as.numeric(3-1) or as.numeric(-3) are T, but "3-1" is not a negative number stored as text and unclear how you would want that reported here.
  }
  
  all(!grepl("[^0123456789]", x))
}

####################################################### # 

#' DRAFT utility to check if vector is numbers stored as text like "01"
#'
#' @param x character vector (or it reports FALSE)
#'
#' @return TRUE or FALSE (not a vector as long as x... just length 1).
#'   Always ignores any or all NA values.
#' 
#' @keywords internal
#'
is.numeric.text = function(x, trim_ = FALSE) {
  
  # DRAFT
  
  if (!is.atomic(x))    {return(FALSE)}
  if (!is.character(x)) {return(FALSE)}
  
  x <- x[!is.na(x)]
  
  # if (length(x) == 1 && is.na(x)) {return(NA)} 
  
  return( 
    is.numerals.only(x, trim_ = trim_)
  )
  
  #      ################## 
  
  # or maybe this....
  
  suppressWarnings({
    all(!is.na(as.numeric(x)))
  })
  
}
####################################################### # 
