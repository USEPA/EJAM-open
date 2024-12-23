#' Get State each site is entirely within, quickly, from table of blockid, ejam_uniq_id
#' 
#' @description  Find the 2-character State abbreviation, but only for sites entirely in 1 state.
#' 
#' @details This only works for sites entirely in 1 state. 
#'   For multistate sites, need lat/lon of site and that is slower.
#'   This returns NA for any site whose nearby blocks are in multiple states. 
#'   For multistate sites, you need to know the lat/lon of the site to be sure of its state.
#'   
#'   This function is for when you need to quickly find out the state each site is in,
#'   to be able to report state percentiles, This can identify the State
#'   each site is located in, based on the states of the nearby blocks (and parent blockgroups).
#'   In many analyses, all the sites will be single-state sites, and this function will be sufficient.
#'   
#'   However, for the rare edge case where you did not save the lat,lon of sites you analyzed,
#'   you would need to approximate those from the lat/lon of the blocks and their distances, 
#'   via latlon_from_s2b(), separately.
#'   
#' @param sites2blocks data.table or data.frame, like [testoutput_getblocksnearby_10pts_1miles],
#'   from [getblocksnearby()] that has columns ejam_uniq_id and blockid and distance
#' @seealso [state_from_blockid_table()] [state_per_site_for_doaggregate()]
#' @return data.table with columns  ejam_uniq_id, ST 
#' @details
#' These two functions differ -- one gets the state info for each unique site, 
#' and the other gets the state abbreviation of each unique block:
#' Note: unexported function
#' xx = state_from_s2b_bysite(testoutput_getblocksnearby_10pts_1miles)[]
#' NROW(xx)
#' [1] 10
#' length(unique(testoutput_getblocksnearby_10pts_1miles$ejam_uniq_id))
#' [1] 10
#' 
#' length(EJAM:::state_from_blockid_table(testoutput_getblocksnearby_10pts_1miles))
#' [1] 1914
#' NROW(testoutput_getblocksnearby_10pts_1miles)
#' [1] 1914
#'   
#' @examples \dontrun{
#' # unexported function
#' table(state_from_blockid_table(testoutput_getblocksnearby_10pts_1miles))
#' state_from_s2b_bysite(testoutput_getblocksnearby_10pts_1miles)[]
#' 
#'   x = getblocksnearby(pts, radius = 30)
#'   y = state_from_s2b_bysite(x)
#'   table(y$in_how_many_states)
#'   y
#' 
#'   fname = './inst/testdata/testpoints_207_sites_with_signif_violations_NAICS_326_ECHO.csv'
#'   x = state_from_s2b_bysite(
#'     getblocksnearby( latlon_from_anything(fname), quadtree = localtree))
#'   y = read_csv_or_xl(fname)
#'   x$ST == y$FacState
#'   }
#'   state_from_s2b_bysite(testoutput_getblocksnearby_10pts_1miles) 
#' 
#' @keywords internal
#'
state_from_s2b_bysite <- function(sites2blocks) {

  setDT(sites2blocks)
  if (!all(c('ejam_uniq_id', 'blockid' ) %in% names(sites2blocks) )) {
    warning("column names must include ejam_uniq_id and blockid, as in output of getblocksnearby() - see ?testoutput_getblocksnearby_10pts_1miles")
    return(NULL)
  }
  # sites2blocks <- getblocksnearby(testpoints_1000, radius = 3.1)
  
  
  s2st <- blockgroupstats[sites2blocks, .(ejam_uniq_id, ST), on = "bgid"][ , .(st1 = ST[1], in_how_many_states = length(unique(ST))), by = "ejam_uniq_id"]
  setorder(s2st, ejam_uniq_id)
  
  # s2st[, multistate := in_how_many_states > 1]
  # multistate_ids  <- s2st[in_how_many_states > 1, ejam_uniq_id]
  # s2st is like this:
  #    ejam_uniq_id    st1 in_how_many_states
  #           <int> <char>              <int>
  # 1:          365     AL                  1
  # 2:          806     AL                  1
  # 3:          890     AL                  1
  
  s2st[in_how_many_states == 1, ST := st1] # done. same state for 1st bgid as for all bgids
  s2st[, st1 := NULL]
  
  s2st[in_how_many_states != 1, ST := NA]  # will need sitepoints lat lon and shapefile of states for these
  
  return(s2st) 
}
################################################################### # 

## example of 2 ways to add statename columns using ST
# results_bysite = copy(testoutput_ejamit_100pts_1miles$results_bysite)[,4:9]
#   results_bysite[, statename2 := fips2statename(fips_state_from_state_abbrev(ST))] # like  
#   results_bysite[, statename3 := stateinfo$statename[match(ST, stateinfo$ST)]]

