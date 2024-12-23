
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
#' @param need_blockwt set to FALSE to speed it up if you do not need blockwt
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
#' @export
#'
getblocksnearby_from_fips <- function(fips, inshiny = FALSE, need_blockwt = TRUE) {

  ######################################## #
  # Handle special case where FIPS are for City/CDP ####
  ftype = fipstype(fips)
  if ('city' %in% ftype) {
    if (!all(ftype[!is.na(ftype)] == 'city')) {
      warning("Ignoring the City/CDP FIPS because getblocksnearby_from_fips cannot handle a combination of FIPS where some are city/Census Designated Places (6-7 digit FIPS) and others are not (e.g., Counties)")
      fips[ftype == 'city'] <- NA
    } else {
      cat("note that fips for cities/cdps are handled as shapefiles for analysis\n")
      # must use a separate function to handle City/CDP FIPS since they do not map onto block groups bounds or by FIPS digits
      
      # example:
      # fips = fips_place_from_placename('chelsea city, MA', exact = T)
      # 2513205
      # mapview(  shapes_places_from_placefips(fips_place_from_placename('chelsea city, MA', exact = T) ))
      # mapview(  shapes_places_from_placefips(fips_place_from_placename('chelsea,MA', exact = FALSE) ))
      # mapview(shapes_places_from_placefips(  fips_place_from_placename('white plains, ny', exact = F)   ))
      
      polys = shapes_places_from_placefips(fips)
      polys
      s2b_pts_polys <- get_blockpoints_in_shape(
        polys = polys
      )  
      
      ## convert ejam_uniq_id 1:N to the fips here - to emulate what is done by getblocksnearby_from_fips()
      ## *** try/ test this:  order of rows is not quite right yet
      s2b_pts_polys$pts$ejam_uniq_id  <- as.character(fips[s2b_pts_polys$pts$ejam_uniq_id ])
      
      # drop the shapefile here - just return the blocks in each polygon
      return(s2b_pts_polys$pts)
    }
  }
  ######################################## #
  
  if (!exists('blockid2fips')) {
    dataload_from_pins(varnames = 'blockid2fips')
  }
  if (!exists('bgid2fips')) {
    dataload_from_pins(varnames = 'bgid2fips')
  }
  
  fips.char <- fips_lead_zero( fips)  # adds leading zeroes and returns as character, 5 characters if seems like countyfips, etc. 
  fipslengths <- nchar(fips.char)
  if (!(length(unique(fipslengths)) == 1)) {    # might recode to allow that but it is complicated
    if (inshiny) {
      validate('fips must all be same number of characters, like all are 5-digit county fips with leading zeroes counted')
    } else {
      stop('fips must all be same number of characters, like all are 5-digit county fips with leading zeroes counted')
    }}
  #  see   fipstype() function.
  # 
  # > length(unique(substr(blockid2fips$blockfips,1,12)))
  # [1] 242335
  # > length(unique(substr(blockid2fips$blockfips,1,11)))
  # [1] 85395
  # > length(unique(substr(blockid2fips$blockfips,1,5)))
  # [1] 3221
  
  fips_vec <- fips  
  names(fips_vec) <- fips.char # as.character(fips_vec)
  if (length(fips_vec) == 1) {
    fips_vec <- c(fips_vec, na = NA) # quick workaround since code below failed if only 1 FIPS provided, like 1 state or only 1 county
  }
  suppressWarnings({ # because if length was 1 and added NA at end, this reports irrelevant warning
  ## create two-column dataframe with bgs (values) and original fips (ind)
  # fips_bgs_in_fips1() returns all blockgroup fips codes contained within each fips provided
  # fips_bgs_in_fips() replaces fips_bgs_in_fips1()
    # all_bgs <- stack(sapply(fips_vec, fips_bgs_in_fips)) # newer - fast alone but slow in sapply?
  all_bgs <- stack(sapply(fips_vec, fips_bgs_in_fips1)) # Slow:  1.4 seconds for all counties in region 6, e.g.
  })
  names(all_bgs) <- c('bgfips', 'ejam_uniq_id')
  
  # *** It actually could be more efficient to replace the above fips_bgs_in_fips1() 
  # or make a new func to provide bgid_from_anyfips() 
  # instead of 1st getting bgfips and then needing to look up bgid by bgfips - 
  #
  #    Can we just change to this?... 
  #      use fips_bgs_in_fips() to get all bg fips values
  #      use join to blockgroupstats on bgfips, to get all bgid values
  #      use join to blockwts on bgid, to get all the blockid values.
  #
  # Get bgid:
  all_bgs$bgid <- bgid2fips[match(all_bgs$bgfips, bgfips), bgid]

  #### IS THIS RIGHT OR DID IT GET MESSED UP :  ???  bgfips vs site id vs ejam_uniq_id might have gotten mixed up: ***
    all_bgs$ejam_uniq_id <- as.character(all_bgs$ejam_uniq_id) # because stack() always creates a factor column. data.table might have a faster reshaping approach? ***
  # Note that site id in this case actually is the fips provided, like a state fips or county fips vector
 
  ## only process blockgroups exist for uploaded data
  if (nrow(all_bgs) > 0) {
    # WOULD data.table join or merge be faster than dplyr here? ***
    fips_blockpoints <- dplyr::left_join(all_bgs, 
                                         ## create 12-digit column inline (original table not altered)  ## do not actually need blockfips here except to join on its first 12 chars  
                                         blockid2fips[, .(blockid, blockfips, blockfips12 = substr(blockfips,1,12))], 
                                         by = c('bgfips' = 'blockfips12'), multiple = 'all') |> 
      dplyr::left_join(blockpoints) |>  
      dplyr::mutate(distance = 0) |>      # or do I want distance to be null, or missing or NA or 0.001, or what? note approximated block_radius_miles is sometimes zero, in blockwts
      data.table::as.data.table()
    
    if (need_blockwt) {
      # provide blockwt to be consistent with getblocksnearby() and doaggregate() understands it if you want to use it after this.
      #fips_blockpoints[,blockwt := 1] # since doaggregate() uses blockwt even though we know the resulting bgwt will be 1 in every case if used FIPS codes bigger than blocks (blockgroups, tracts, counties, states, whatever)
      fips_blockpoints <- merge(fips_blockpoints, blockwts[,.(blockid, blockwt)], by = "blockid")
    }
    
    ## remove any invalid  values 
    fips_blockpoints <- na.omit(fips_blockpoints)
    
    # Emulate the normal output of  getblocksnearby() which is a data.table with  
    #  ejam_uniq_id, blockid, distance, blockwt, bgid
    # but do not really need to return bgfips, blockfips, lat, lon here.
    setcolorder(fips_blockpoints, c('ejam_uniq_id', 'blockid', 'distance', 'blockwt', 'bgid'))
    fips_blockpoints[ , bgfips := NULL]
    fips_blockpoints[ , blockfips := NULL]
    fips_blockpoints[ , lat := NULL]
    fips_blockpoints[ , lon := NULL]
    
    return(fips_blockpoints[])
    
  } else {
    if (inshiny) {
      ## if not matched, return this message
      shiny::validate('No blockgroups found for these FIP codes.') # A list of tests. Each test should equal NULL for success, FALSE for silent failure, or a string for failure with an error message.
    } else {
      stop('No blockgroups found for these FIP codes.')
    }
    
  }
}
