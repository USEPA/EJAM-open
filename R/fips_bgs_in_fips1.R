

#' @keywords internal
#' 
fips_bgs_in_fips1 <- function(fips) {
  
  # SLOWER THAN fips_bgs_in_fips() ALONE BUT FASTER WHEN IN sapply( )  ???
  
  x <- fips_lead_zero(fips)
  
  if (anyNA(x)) {
    howmanyna = sum(is.na(x))
    warning("NA returned for ", howmanyna," values that failed to match")
  }
  fips <- x[!is.na(x)]
  
  # if smaller than bg (i.e., block fips), return just the parent bgs
  fips <- unique(substr(fips,1,12))
  
  # if bigger than bg, return all the child bgs
  all_us_bgfips <- blockgroupstats$bgfips
  
  # if nchar==2, state, so get all bg starting with that
  # if nchar is N, get all bg starting with those N characters
  
  len <- nchar(fips)
  bgfips <- fips[len == 12]
  nonbg <- fips[len !=  12]
  
  extrabgs <- sapply(nonbg, FUN = function(z) all_us_bgfips[startsWith(all_us_bgfips, z)])
  # extrabgs <- list(rep(NA, length(nonbg)))
  # for (thisone in nonbg) {
  #   extrabgs[[i]] <-   all_us_bgfips[startsWith(all_us_bgfips, thisone)]
  # }
  # extrabgs <- do.call(c,extrabgs)
  
  return(unlist(union(bgfips, extrabgs)))
}
############################################################################# #
