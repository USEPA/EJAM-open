
#' Find all blocks within each of the FIPS codes provided
#' 
#' Allows EJAM to analyze and compare Counties, for example
#' 
#' @param fips vector of FIPS codes identifying blockgroups, tracts, counties, or states.
#'   This is useful if -- instead of gettings stats on and comparing circular buffers or polygons --
#'    one will be getting stats on one or more tracts, 
#'   or analyzing and comparing blockgroups in a county, 
#'   or comparing whole counties to each other, within a State.
#' @param inshiny used by shiny app server code to handle errors via validate() instead of stop()
#' @param need_blockwt ignored now
#' @return same as for [getblocksnearby] but one row per FIPS, and the distance column is irrelevant
#'
#' @examples
#'   x <- getblocksnearby_from_fips(fips_counties_from_state_abbrev("DE"))
#'   counties_ej <- doaggregate(x)
#'   #cannot use mapfast(counties_ej$results_bysite) since no lat lon.  mapfastej_counties() should work...
#'   y =  ejamit(fips=fips_counties_from_statename("Delaware"))
#'   
#'   # x=getblocksnearby_from_fips("482011000011") # one blockgroup only
#'   # y=doaggregate(x)
#' @seealso [getblocksnearby()] [fips_bgs_in_fips()] [fips_lead_zero()] [getblocksnearby_from_fips()] [fips_from_table()]
#' 
#' @keywords internal
#'
getblocksnearby_from_fips2 <- function(fips, inshiny = FALSE, need_blockwt = TRUE) {
  
  # unfortunately fips_lead_zero() gets done twice - once here to check all are same type, and later by fips_bgs_in_fips()
  #  see   fipstype() function
  fips.char <- fips_lead_zero( fips)  # adds leading zeroes and returns as character, 5 characters if seems like countyfips, etc. 
  fipslengths <- nchar(fips.char)
  if (!(length(unique(fipslengths)) == 1)) {
    if (inshiny) {
      validate('fips must all be same number of characters, like all are 5-digit county fips with leading zeroes counted')
    } else {
      stop('fips must all be same number of characters, like all are 5-digit county fips with leading zeroes counted')
    }}
  
  # Get all bgfips in each state, county, tract, or other kind of fips. (or just compare blockgroups themselves) (*** may not work right if specified fips are blocks)
  # We only get bgfips as a way to get bgid and then all blockids. After that we do not need bgfips.
  # Note that ejam_uniq_id in this case actually is the fips provided, like a state fips or county fips vector
  fips_blockpoints <- data.table(ejam_uniq_id = fips)
  
  ## this oddly fips_bgs_in_fips() is faster than fips_bgs_in_fips1() on its own, but
  ## when used in getblocksnearby_from_fips2() here, it makes getblocksnearby_from_fips2() much faster than any other combo.
  fips_blockpoints <- fips_blockpoints[ , fips_bgs_in_fips(ejam_uniq_id), by = "ejam_uniq_id"] # newer faster func makes this overall go slower??
  # fips_blockpoints <- fips_blockpoints[ , fips_bgs_in_fips1(ejam_uniq_id), by = "ejam_uniq_id"]
  
  setnames(fips_blockpoints, "V1", "bgfips")
  
  # stop if none found
  if (NROW(fips_blockpoints) == 0) {
    if (inshiny) {shiny::validate('No blockgroups found for these FIP codes.')} else {stop('No blockgroups found for these FIP codes.')}
  } else {
    
    # Get bgid from blockgroupstats (use join to blockgroupstats on bgfips, to get all bgid values)
    fips_blockpoints[blockgroupstats, bgid := bgid, on = "bgfips"]
    
    # Get blockid from blockwts (use blockwts table to get all the blockid values for each bgid value in fips_blockpoints)
    fips_blockpoints <- merge(fips_blockpoints, blockwts[ , .(bgid, blockid, blockwt)], by = "bgid")
    
    ## Get all blocks in each bg. Use merge(). A join would only get one block per bg, e.g.,  fips_blockpoints[blockwts, blockid := blockid, on = "bgid"]
    # Include the blockwt to be consistent with getblocksnearby(), and so doaggregate() understands it, and if you want to use it after this,
    #  even though we know the resulting bgwt (not blockwt) will be 1 in every case assuming we started with FIPS codes bigger than blocks (blockgroups, tracts, counties, states, whatever)
    
    ## Remove any invalid blocks
    fips_blockpoints <- na.omit(fips_blockpoints)
    
    # Emulate the normal output of getblocksnearby() which is a data.table with these columns:
    #  ejam_uniq_id, blockid, distance, blockwt, bgid
    #  but do not really need to return bgfips, blockfips, lat, lon here.
    #  Distance for fips analysis is zero. or do I want distance to be null, or missing or NA or 0.001, or what? note approximated block_radius_miles is sometimes zero, in blockwts
    fips_blockpoints[, distance := 0]
    fips_blockpoints[ , bgfips := NULL]
    suppressWarnings({
      fips_blockpoints[ , blockfips := NULL] # should not be there anyway
      fips_blockpoints[ , lat := NULL] # should not be there anyway
      fips_blockpoints[ , lon := NULL] # should not be there anyway
    })
    setcolorder(fips_blockpoints, c('ejam_uniq_id', 'blockid', 'distance', 'blockwt', 'bgid'))
  }
  
  return(fips_blockpoints[])
}
