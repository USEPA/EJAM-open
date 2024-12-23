
#' state_from_sitetable - Identify US State that each site is in (given ST, FIPS, or lat/lon)
#' 
#' Identify US State that each site is in (given ST, lat/lon, or FIPS)
#' @param sites data.frame or data.table, with one row per site, and
#'   column(s) that are either "ST" (2-letter abbreviation of State),
#'   "lat" and "lon", or "fips" or "bgfips" 
#'   and optionally a column like  "ejam_uniq_id" or "n"
#' @param ignorelatlon set to TRUE to skip the slowest step of inferring ST from latlon
#'   in case you want to do that via sites2blocks info on blocks nearby
#' @seealso [state_from_blockid_table()] [state_per_site_for_doaggregate()] 
#'   [state_from_latlon()] [state_from_fips_bybg()]
#' @return the input table as a data.frame, but with these 
#'   new columns if ST was not already a column:
#'   ejam_uniq_id, ST, statename, FIPS.ST, REGION, n
#' @examples
#'   state_from_sitetable(testpoints_10)
#'   state_from_sitetable(testoutput_ejamit_10pts_1miles$results_bysite[, .(ejam_uniq_id, ST, pop)])
#'   state_from_sitetable(testoutput_ejamit_10pts_1miles$results_bysite[, .(ST, pop)])
#'   state_from_sitetable(testoutput_ejamit_10pts_1miles$results_bysite[, .(ST, lat, lon, pop)])
#'   
#' @export
#'
state_from_sitetable <- function(sites, ignorelatlon = FALSE) {
  
  if (NROW(sites) == 0) {
    warning('no sites')
    return(NULL)
  }
  
  bad_sites2states <- FALSE
  if (missing(sites)) {stop("requires sites, a table with columns that can be ST, fips or bgfips, or lat and lon (and ejam_uniq_id kept if present) or vector of fips or ST values")}
  sites2states <- sites # sites2states_or_latlon # could rename param or variable in code to avoid later doing something by reference and changing the input table by reference using data.table ? ***
  
  # if vector not table was provided, try to interpret as fips or ST values
  if (is.atomic(sites2states) && is.character(sites2states) && is.vector(sites2states)) {
    if (all(nchar(sites2states) == 2)) {
      if (sum(fips_valid(sites2states), na.rm = TRUE) > sum(sites2states %in% stateinfo$ST, na.rm = TRUE)) {
        message("interpreting vector as FIPS values")
        sites2states <- data.frame(fips = sites2states)
      } else {
        message("interpreting vector as ST values")
        sites2states <- data.frame(ST = sites2states)
      }
    } else {
      if (sum(fips_valid(sites2states), na.rm = TRUE) > 0 ) {
        message("interpreting vector as FIPS values")
        sites2states <- data.frame(fips = sites2states)
      }
    }
  }
  if (is.atomic(sites2states) && is.numeric(sites2states) && is.vector(sites2states)) {
    sites2states <- data.frame(fips = substr(sites2states,1,2))
  }
  
  if (data.table::is.data.table(sites2states)) data.table::setDF(sites2states)
  # error handling
  ### overly inflexible to require identical lists of ejam_uniq_id values -- 
  # might want to get state pctiles for all where possible even if 1 site lacks ST, 
  # and ignore extra info too not found in sites2blocks
  # if (!all(unique(sites2blocks$ejam_uniq_id) %in% sites2states$ejam_uniq_id)) {
  #   warning("cannot provide state percentiles unless all ejam_uniq_id values in sites2blocks are also in sites2stats")
  #   bad_sites2states <- TRUE
  # }
  # if (!all(unique(sites2states$ejam_uniq_id) %in% sites2blocks$ejam_uniq_id)) {
  #   warning("cannot provide state percentiles unless all ejam_uniq_id values in sites2stats are also in sites2blocks")
  #   bad_sites2states <- TRUE
  # }
  
  # create ejam_uniq_id column only if cannot find one (are we sure we want to do that?) ***
  if (!("ejam_uniq_id" %in% names(sites2states))) {
    if ("n" %in% names(sites2states)) {  # use n or rownumber as ejam_uniq_id if not explicitly provided as ejam_uniq_id column
      sites2states$ejam_uniq_id <- sites2states$n
    } else {
      sites2states$ejam_uniq_id <- 1:NROW(sites2states)
    }
  }
  # note that if this was called by ejamit_compare_types_of_places() and just looking at one of those types, 
  # an overall ejam_uniq_id column will have been added to the entire sitepoints table and the subset (type) passed here can have
  # ejam_uniq_id values that are just some subset, not 1:N ! so that needs to be retained and returned by this function.
  ################################## #
  
  # order of best to slowest preference for finding ST info:
  
  # 1. ST: if already there
  
  # 2. FIPS like if fips or some shp analyses:   stfips, bgfips, blockfips, then fips  etc.:   ST <- fips2state_abbrev(fips)
  
  # 3. latlon of sites, points analyzed (not of blocks nearby)       state_from_latlon(lat = , lon = )  # but this is slowest
  
  # 4.  shapefile of zones - NOT IMPLEMENTED 
  #
  #     Ideally do not want to infer based on just the polygons, but would need to do work from the s2b type table that came out of  get_blockpoints_in_shape() assuming it has been done.
  #     and that will correctly handle case of polygon entirely in 1 state.
  #     But unclear what to do if a polygon spans 2+ states (including any buffer it may have).
  #     if working from the s2b table it would normally try to use distances of blocks to infer some site point but there are no distances in the shape/polygon case output of get_blockpoints_in_shape()
  #     Maybe just we should find which state has the largest share of population from the whole polygon. 
  #     OR, maybe a weighted avg of the scores from various states...? 
  
  
  # 1. if ST was already available,  just leave table as-is ####
  
  if ("ST" %in% names(sites2states)) {
    # done
  } else {
    
    # 2. is any FIPS there?
    
    suppressWarnings({x = fips_from_table(sites2states)}) # returns NULL if no column could be interpreted as fips column name, and returns vector of values otherwise
    if (!is.null(x) && (sum(fips_valid(x), na.rm = TRUE) > 0)) {
      sites2states$ST <- fips2state_abbrev(fips2state_fips(x))       # fips2state_abbrev() is NOT the same as state_from_fips_bybg(sites2states$fips)
    } else {
      
      if (!ignorelatlon) {
        
        # 3. lat lon of site points (not blocks) ####
        # IF ALREADY THERE OR HAD BEEN ESTIMATED VIA latlon_from_s2b() 
        
        # *** Note this MIGHT also occur in shapefile case now?
        
        sites2states <- latlon_from_anything(sites2states, interactiveprompt = FALSE) # ejam_uniq_id if already was there is preserved here
        if (("lat" %in% names(sites2states) ) && ("lon" %in% names(sites2states))) {
          # use lat lon to get ST, but it takes a few seconds to use the shapefile to do this:
          sites2states <- cbind(
            sites2states,   # ejam_uniq_id if already was there is preserved here
            state_from_latlon(lat = sites2states$lat, lon = sites2states$lon)  ## VERY SLOW STEP ***  
          )
        } else {
          
          # 4. shapefiles case ####
          # assuming no latlon was found ***
          
          # if("sf" %in% class(sites2states)) {}  # but only the non-geometry part, a data.frame, would get passed here from 
          
          
          
          bad_sites2states <- TRUE
        }
      } else {
        bad_sites2states <- TRUE
      }
    }
  }
  
  # if nothing found to tell us the ST info, fill in NA values
  if (bad_sites2states) {
    if ("ejam_uniq_id" %in% names(sites2states)) {
      # preserve id if already there, (which it always is at this point since it was checked and created if missing, above)
      #   in case using ejamit_compare_types_of_places() e.g.
      sites2states$ST <- NA
      sites2states <- sites2states[, c("ejam_uniq_id", "ST")] # get rid of extra columns (even if latlon were there, if ignorelatlon=T)
      sites2states <- unique(sites2states) # get rid of redundant rows
    } else {
      ## this case should not occur now, because of code above. It used to do this:
      # sites2states <- data.frame(ejam_uniq_id = 1:length(unique(sites2states$ejam_uniq_id)), ST = NA)
      
      ## get rid of extra columns and redundant rows, to retain only 1 row per id and only ST and ejam_uniq_id cols)
      sites2states <- data.frame(ejam_uniq_id = 1:NROW(sites2states), ST = NA)
    }

  }
  # confirm quality of ST info found or looked up
  # note that 
  # > setdiff(stateinfo2$ST,  stateinfo$ST)
  # [1] "AS" "GU" "MP" "UM" "VI" "US"
  if (!all(sites2states$ST %in% stateinfo$ST)) {
    if (any(sites2states$ST %in% c("AS", "GU", "MP", "UM", "VI"))) {
      warning("Some ST values appear to be referring to Island Areas, which are not counted as States here")
    }
    sites2states$ST[(!(sites2states$ST %in% stateinfo$ST))] <- NA
    if (all(is.na(sites2states$ST))) {
      message("no valid states could be determined by state_from_sitetable() -- based on ST, fips (or lat,lon if ignorelatlon is FALSE) -- so we cannot lookup state percentiles, etc.")
    } else {
      message("some of the states could not be determined by state_from_sitetable() -- based on ST, fips  (or lat,lon if ignorelatlon is FALSE) -- so for those we cannot lookup state percentiles, etc.")
    }
  }
  
  return(sites2states)
}
