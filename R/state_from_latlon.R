
# state_from_sitetable()  using # latlon_from_s2b() can use ST,FIPS, or latlon


# state_from_s2b_bysite()   -     compare this to state_from_blockid_table() ***  -  formerly called  ST _by_site_from_sites2blocks() 

### in this source file:

# state_from_blockid_table()   - compare this to state_from_s2b_bysite()   *** 
# state_from_blockid()
# state_from_latlon()
# state_from_fips_bybg()    # confusing name
# state_from_nearest_block_bysite()

# fips2state_abbrev() *** # this is the one to use for a single state per FIPS

# fips_state_from_state_abbrev()
# fips2state_fips()

## and see elsewhere states_as_sites()


########################################### #


state_from_nearest_block_bysite <- function(s2b) {
  
  # simplistic quick way to get state of nearest block to each site - 
  # and if FIPS, works ok. 
  # but  if a polygon covering 2+ states, it just picks one block which might not be from the state accounting for most of the polygon.
  if (any(s2b$distance > 0)) {sitetype <- "latlon"} else {
    sitetype <- "shp or fips"
  }
  if (sitetype == "latlon") {
    s2b[, .(ST = state_from_blockid(blockid[which.min(distance)])), keyby = ejam_uniq_id]
  } else {
    if ("ST" %in% names(s2b)) {
      # already there sometimes? 
      s2b
    } else {
      s2b[, .(ST = state_from_blockid(blockid[1])), keyby = ejam_uniq_id]
    }
  }
}
########################################### #


#' Find what state is where each point is located
#'
#' Takes 3 seconds to find state for 1k points, so a faster alternative would be useful
#' @param lon longitudes vector
#' @param lat latitudes vector
#' @seealso [states_shapefile] [get_blockpoints_in_shape()] [state_from_sitetable()]
#' @return Returns data.frame: ST, statename, FIPS.ST, REGION, n
#'   as many rows as elements in lat or lon
#' @examples
#'  myprogram <- "CAMDBS" # 739 sites
#'  pts <- frs_from_program(myprogram)[ , .(lat, lon, REGISTRY_ID,  PRIMARY_NAME)]
#'  # add a column with State abbreviation
#'  pts[, ST := state_from_latlon(lat=lat, lon = lon)$ST]
#'  #map these points
#'  mapfast(pts[ST == 'TX',], radius = 1) # 1 miles radius circles
#'
#' @export
#'
state_from_latlon <- function(lat, lon) {
  
 #  states_shapefile   <- EJAM::states_shapefile
  
  # if just a table was provided try to accept that- could use latlon_from_anything() but that may be slower and overkill
  if (missing(lon) && !missing(lat) && is.data.frame(lat) && "lon" %in% names(lat) && "lat" %in% names(lat)) {
    lon <- lat$lon
    lat <- lat$lat
  }
  
  if (suppressWarnings({
    any(is.na(as.numeric(lat)) & is.na(as.numeric(lon))) }) ) {
    warning("Some Latitude and Longitude could not be coerced to a number.")
  } else {
    if (suppressWarnings({
      any(is.na(as.numeric(lat))) })) {
      warning("Some Latitude could not be coerced to a number")
    }
    if (suppressWarnings({
      any(is.na(as.numeric(lon))) }) ) {
      warning("Some Longitude could not be coerced to a number.")
    }
  }
  lat[is.na(as.numeric(lat))] <- NA
  lon[is.na(as.numeric(lon))] <- NA
  
  lat[is.na(lat)] <- 0
  lon[is.na(lon)] <- 0 # will ensure NA is returned by the join for those points with missing coordinates
  pts <- data.frame(lat = lat, lon = lon) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(states_shapefile))  # st_as_sf wants lon,lat not lat,lon
  pts <- pts |> sf::st_join(states_shapefile)
  # note setdiff(stateinfo2$ST, sort(unique(states_shapefile$STUSPS)))
  # [1] "UM" "US"  but has PR,GU,AS,VI,MP
  
  pts <- as.data.frame(pts)[,c("STUSPS", "NAME", "STATEFP")]
  colnames(pts) <- c("ST", "statename", "FIPS.ST")
  pts$REGION <- fips_st2eparegion(pts$FIPS.ST) # this is not sensitive to exact spelling of statename and can handle Island Areas 
  # EJAM::stateinfo$REGION[match(pts$statename, stateinfo$statename)] 
  pts$n <- 1:NROW(pts)
  
  if (suppressWarnings({
    any(is.na(pts$statename ))})
  ) {warning("Some latitude / longitude were provided that are not found in any state")}
  return(pts)
}
##################################################################################################### #


#' state_from_blockid_table was used in some special cases e.g., in testpoints_n()
#'
#' given data.table with blockid column, get state abbreviation of each - not used?
#' @param dt_with_blockid
#'
#' @return vector of ST info like AK, CA, DE, etc.
#'
#' @examples
#' x = sample(blockpoints$blockid, 3)
#' state_from_blockid_table(blockpoints[blockid %in% x, ])[]
#' mapfast(blockpoints[blockid %in% x, ])
#' 
#' table(state_from_blockid_table(testoutput_getblocksnearby_10pts_1miles))
#' # unique(state_from_latlon(testpoints_10)$ST) # slow
#' 
#' all.equal(state_from_blockid(x), state_from_blockid_table(blockpoints[blockid %in% x, ]))
#' 
#' @keywords internal
#'
state_from_blockid_table <- function(dt_with_blockid) {
  
  ## TEMPORARILY UPDATED BUT FURTHER EDITS IN BRANCH TO BE MERGED SOON
  
  if ("bgid" %in% names(dt_with_blockid)) {
    
    return(blockgroupstats[dt_with_blockid, ST, on = "bgid"])
    
  } else {
    
    # all in one step, 
    # use blockid to get bgid from blockwts table, 
    # then use bgid to get ST from blockgroupstats table
    
    return(blockgroupstats[blockwts[dt_with_blockid, .(bgid, blockid), on = "blockid"], ST, on = "bgid"])
    
  }
  
}
##################################################################################################### #


#' given vector of blockids, get bgid of each (the parent block group)
#'
#' @param blockid vector of block ids like in blockwts data.table or blockpoints
#'
#' @return vector of bgid values
#' 
#' @keywords internal
#'
bgid_from_blockid = function(blockid) {
  blockwts[data.table(blockid = blockid), .(bgid, blockid), on = "blockid"]$bgid
  }
##################################################################################################### #


#' given vector of blockids, get state abbreviation of each
#' unused. Not needed if you have sites2blocks table that includes a bgid column
#' 
#' @param blockid vector of blockid values as from EJAM in a table called blockpoints
#' @seealso unexported state_from_blockid_table() 
#' @return vector of ST info like AK, CA, DE, etc.
#' @examples
#' x = sample(blockpoints$blockid, 3)
#' state_from_blockid(x)[]
#' mapfast(blockpoints[blockid %in% x, ])
#' 
#' all.equal(state_from_blockid(x), state_from_blockid_table(blockpoints[blockid %in% x, ]))
#' 
#' @keywords internal
#'
state_from_blockid <- function(blockid) {
  
  dt_with_blockid <- data.table(blockid = blockid)
  return(state_from_blockid_table(dt_with_blockid))
}
##################################################################################################### #


#' Get FIPS of ALL BLOCKGROUPS in the States or Counties specified
#'
#' Get the State abbreviations of ALL blockgroups WITHIN the input FIPS
#'
#' @details Unlike [fips2state_abbrev()], this returns a vector of 2-letter State abbreviations that is
#'   one per blockgroup that matches the input FIPS,
#'   not necessarily a vector as long as the input vector of FIPS codes!,
#'   and not just a short list of unique states!
#' @param fips Census FIPS codes vector, numeric or char, 2-digit, 5-digit, etc. OK
#' @param uniqueonly If set to TRUE, returns only unique results.
#'   This parameter is here mostly to remind user that default is not uniques only.
#' @seealso [fips2state_abbrev()] to get just one state per FIPS
#' @return vector of 2-character state abbreviations like CA,CA,CA,MD,MD,TX
#'
#' @export
#'
state_from_fips_bybg <- function(fips, uniqueonly=FALSE) {
  message("This function provides the states of ALL blockgroups within the FIPS, not just one state per fips. see also fips2state_abbrev() ")
  fips <- fips_bgs_in_fips(fips) # returns all the blockgroups fips codes that match, such as all bg in the state or county
  x <- stateinfo$ST[match(substr(fips,1,2), stateinfo$FIPS.ST)]
  if (uniqueonly) {return(unique(x))} else {return(x)}
}
##################################################################################################### #

