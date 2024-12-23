
#' identify the State each site is in, for doaggregate()
#'
#' @param s2b like testoutput_getblocksnearby_100pts_1miles, or output of getblocksnearby()
#' @param s2st like testpoints_10, like input to ejamit() or to getblocksnearby()
#' @return data.table
#' @examples \dontrun{
#' 
#'  # cannot quickly id ST if a site spans 2+ states
#'  # not this is an unexported function:
#'  tail(state_from_s2b_bysite(testoutput_getblocksnearby_100pts_1miles))
#'  
#'  # using the closest block can id the wrong state:
#'  tail(state_from_nearest_block_bysite(testoutput_getblocksnearby_100pts_1miles))
#'  
#'  # getting the true state is slow if some sites span 2+ states:
#'  tail(
#'    state_per_site_for_doaggregate(
#'      testoutput_getblocksnearby_100pts_1miles, 
#'      testpoints_100
#'    ))
#' }
#' 
#' @keywords internal
#'
state_per_site_for_doaggregate <- function(s2b, s2st) {
  
  # This is only a bit slower than the more complicated way tried for doaggregate() that was intended to speed it up.
  # Approx 2-4 seconds for 1000 points either way:
  # 
  # > system.time({x = state_per_site_for_doaggregate(
  #    s2b = testoutput_getblocksnearby_1000pts_1miles,
  #    s2st = testpoints_1000)})
  #    user  system elapsed
  #    1.95    0.06    2.34
  # 
  # > system.time({x = state_from_latlon(testpoints_1000)})
  # user  system elapsed
  # 1.95    0.11    2.71
  
  
  # # compare methods of identifying ST of each site:
  # 
  # st_exact            <- state_per_site_for_doaggregate(s2b = testoutput_getblocksnearby_100pts_1miles, s2st = testpoints_100)
  # st_from_id_if_1state <- state_from_s2b_bysite(testoutput_getblocksnearby_100pts_1miles)
  # st_of_closestblock  <- state_from_nearest_block_bysite(testoutput_getblocksnearby_100pts_1miles)
  # st_bylatlon         <- state_from_latlon(testpoints_100)
  # 
  # tail( cbind(
  #   st_exact = st_exact,
  #   st_from_id_if_1state = st_from_id_if_1state$ST,
  #   st_of_closestblock = st_of_closestblock$ST,
  #   st_bylatlon = st_bylatlon$ST
  # ), 20)
  
  
  ########################################################################################################### # 
  # This first case is typical if using shiny app or ejamit() !!
  # Just in case, check for and use ST or fips if available,
  # but normally it is not yet here, if input was just something like testpoints_10  ***
  suppressWarnings({ suppressMessages({
    sites2states <- state_from_sitetable(s2st, ignorelatlon = TRUE) # check only ST and FIPS, not latlon
  })})
  setDT(sites2states)
  setkey(sites2states, ejam_uniq_id)
  setDT(s2st)
  # setDT(s2b) # not needed- already a data.table
  if (!("ejam_uniq_id" %in% names(s2st))) {
    s2st[, ejam_uniq_id := .I]
  }
  setkey(s2st, ejam_uniq_id)
  
  # Ignored latlon for now even if it is there, since we want to 
  # more quickly infer ST from blockpoints (via bgid join to blockgroupstats) for single-state sites.
  #  see which sites cover ONLY ONE STATE, based on ids/fips of all their blockpoints nearby.
  #
  # FOR SINGLE-STATE SITES  ####
  #     quickly get ST (and it returns NA for multistate ones)  
  
  ids_needing_st <- sites2states$ejam_uniq_id[is.na(sites2states$ST)]
  
  idst <- state_from_s2b_bysite(s2b[ejam_uniq_id %in% ids_needing_st, ]) #  - should be fast but is SLOW and gives results for only single-state sites
  setDT(idst)
  setkey(idst, ejam_uniq_id)
  id_gotten <- idst$ejam_uniq_id[!is.na(idst$ST)]
  
  sites2states$ST[sites2states$ejam_uniq_id %in% id_gotten] <- idst[ejam_uniq_id %in% id_gotten, ST]  # to be safer, should use a join on id?
  sites2states[idst, in_how_many_states := in_how_many_states, on = "ejam_uniq_id"]
  
  if (!all(idst$ejam_uniq_id %in% id_gotten)) {
    # FOR MULTISTATE SITES ONLY ####
    #     go back to slow method -- using sitepoints lat lon to identify ST:
    #    
    #     Try to use latlon of each site to infer ST (for the multisite ones)
    #        using latlon of points assuming they are already available from sitepoints table
    suppressWarnings({ suppressMessages({
      idst <- state_from_sitetable(s2st[!(ejam_uniq_id %in% id_gotten), ], ignorelatlon = FALSE)   # uses state_from_latlon() - SLOW
      setDT(idst)
      setkey(idst, ejam_uniq_id)
      id_gotten <- idst$ejam_uniq_id[!is.na(idst$ST)] # ids just added, not ever added
      
      # setDF(sites2states)
      sites2states$ST[sites2states$ejam_uniq_id %in% id_gotten] <- idst[ejam_uniq_id %in% id_gotten, ST]  # to be safer, should use a join on id?
    })
    })
  }
  
  ################################## #
  #
  # still not debugged?
  #
  #    RARE EDGE CASE #### 
  ###      Handle any remaining sites, e.g., we did NOT even have latlon of site, so must estimate it via blockpoints
  ###      e.g., doaggregate() called directly without providing sitepoints
  ###      but this case does NOT arise if using shiny app or ejamit()
  
  # id_needlatlon <- sites2states$ejam_uniq_id[ is.na(sites2states$ST)]
  # idlatlon <- latlon_from_s2b(s2b[ejam_uniq_id %in% id_needlatlon, ]) # data.table  - VERY SLOW and not stable
  # setDT(idlatlon)
  # setkey(idlatlon, ejam_uniq_id)
  # s2st <- merge(s2st, idlatlon, by = "ejam_uniq_id", all.x = TRUE) # merge in latlon, but keep all rows of s2st
  
  # ###      Now that you have any previously-missing latlon of site, get ST for those
  # suppressWarnings({ suppressMessages({ 
  #   idst <- state_from_sitetable(s2st[is.na(s2st$ST), ], ignorelatlon = FALSE)   # uses state_from_latlon() - SLOW
  #   setDT(idst)
  #   setkey(idst, ejam_uniq_id)
  #   id_gotten <- idst$ejam_uniq_id[!is.na(idst$ST)] # ids just added, not ever added
  #   sites2states$ST[ejam_uniq_id %in% id_gotten] <- idst[ejam_uniq_id %in% id_gotten, ST] 
  # })})
  
  ################################## #
  # QA/QC? ####
  # Also, may want to check these / qa/qc:
  # if ids in s2st are exactly 1 site ejam_uniq_id row for each of the ids in s2b, none missing (and no extras?),  
  # if already have 1 and only 1 ST for every site? (that was in s2b out of getblocksnearby) 
  # if no NA values for ST ?? ***
  # 
  #   #   "ST" %in% names(s2st) && 
  #   #   "ejam_uniq_id" %in% names(s2st) &&
  #   #   # no dupes in s2st
  #   #   !anyDuplicated(s2st$ejam_uniq_id) &&
  #   #   setequal(s2st$ejam_uniq_id, s2b$ejam_uniq_id) && # setequal ignores duplicates
  #   #   # all((s2b$ejam_uniq_id) %in% s2st$ejam_uniq_id) &&
  ################################## #
  
  # lat,lon also returned, if had been available by site ####
  ##   (but not if they were guessed/approximated in rare edge case above?)
  if (all(c("ejam_uniq_id", "lat", "lon") %in% names(s2st))) {
    sites2states <- merge(sites2states, s2st[, .(ejam_uniq_id, lat, lon)], by = "ejam_uniq_id", all.x = TRUE)
  }
  setorder(sites2states, ejam_uniq_id)
  return(sites2states)
}
########################################################################################################### # 
