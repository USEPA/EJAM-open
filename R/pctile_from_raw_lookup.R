
#' Find approx percentiles in lookup table that is in memory
#'
#' @description This is used with a lookup table to
#'   convert a raw indicator vector to percentiles in US or States.
#' @details
#'
#'   This could be recoded to be more efficient - could use data.table.
#'
#'
#'   The data.frame lookup table must have a field called "PCTILE" that has quantiles/percentiles
#'   and other column(s) with values that fall at those percentiles.
#'   [usastats] and [statestats] are such lookup tables.
#'   This function uses a lookup table and
#'   finds the number in the PCTILE column that corresponds to where a specified value
#'   (in myvector) appears in the column called varname.in.lookup.table.
#'   The function just looks for where the specified value fits between values in the lookup table
#'    and returns the approximate percentile as found in the PCTILE column.
#'   If the value is between the cutpoints listed as
#'   percentiles 89 and 90, it returns 89, for example.
#'   If the value is exactly equal to the cutpoint listed as percentile 90,
#'   it returns percentile 90.
#'   If the value is exactly the same as the minimum in the lookup table and multiple percentiles
#'   in that lookup are listed as tied for having the same threshold value defining the percentile
#'    (i.e., a large percent of places have the same score and it is the minimum score),
#'    then the percentile gets reported as 0,
#'   not the percent of places tied for that minimum score. Note this is true whether they are
#'   tied at a value of 0 or are tied at some other minimum value than 0.
#'   If the value is less than the cutpoint listed as percentile 0,
#'   which should be the minimum value in the dataset,
#'   it still returns 0 as the percentile, but with a warning that
#'   the value checked was less than the minimum in the dataset.
#'
#'   It also handles other odd cases, like where a large percent of all raw scores are tied at
#'   the minimum value, in which case it reports 0 as percentile, not that large percent.
#'
#' @param myvector Numeric vector, required. Values to look for in the lookup table.
#' @param varname.in.lookup.table Character element, required.
#'   Name of column in lookup table to look in
#'   to find interval where a given element of myvector values is.
#'
#'   *** If vector is provided, then must be same length as myvector,
#'
#'   but only 1 value for zone can be provided.
#'
#' @param lookup Either lookup must be provided, not quoted,
#'   or a lookup table called [usastats] must already be in memory. This is the lookup table
#'   data.frame with a PCTILE column, REGION column, and column whose name is the value of varname.in.lookup.table
#'   To use state lookups set lookup=statestats
#' @param zone Character element (or vector as long as myvector), optional.
#'   If specified, must appear in a column called REGION within the lookup table,
#'    or NA returned for each item looked up and warning given.
#'   For example, it could be "NY" for New York State, "USA" for national percentiles.
#' @param quiet set to FALSE to see details on where certain scores were all NA values like in 1 state
#' @aliases lookup_pctile
#' @return By default, returns numeric vector length of myvector.
#' @examples \dontrun{
#'
#' eg <- dput(round(as.vector(unlist(testoutput_ejamit_10pts_1miles$results_overall[ , ..names_d] )),3))
#'
#' data.frame(value = eg, pctile = t(testoutput_ejamit_10pts_1miles$results_overall[ , ..names_d_pctile]))
#'
#' data.frame(value = eg, pctile = lookup_pctile(eg, names_d))
#'
#'
#'   # compare ejscreen API output percentiles to those from this function:
#'   for (vname in c(names_d[c(1,3:6,8:10)] )) {
#'      print(pctile_from_raw_lookup(testoutput_ejscreenapi_plus_100[,vname] / 100, vname,
#'        lookup = usastats)
#'        - testoutput_ejscreenapi_plus_100[,paste0("pctile.",vname)] )
#'   }
#'   for (vname in c(names_e )) {
#'      print(pctile_from_raw_lookup(testoutput_ejscreenapi_plus_100[,vname], vname,
#'        lookup = usastats)
#'          - testoutput_ejscreenapi_plus_100[,paste0("pctile.",vname)] )
#'   }
#' }
#'
#' @export
#'
pctile_from_raw_lookup <- function(myvector, varname.in.lookup.table, lookup=usastats, zone = "USA", quiet=TRUE) {

  #  similar code in ejanalysis package file was lookup.pctiles()

  # CHECK FOR FATAL PROBLEMS  ####

  if (missing(lookup) & !exists("usastats")) {
    if (shiny::isRunning()) {
      warning("lookup default usastats was not found, but should be available as usastats")
      return(rep(NA, length(myvector)))
    } else {
      stop("lookup default usastats was not found, but should be available as usastats")
    }
    }
  if (!is.data.frame(lookup)) {
    if (shiny::isRunning()) {

      warning("lookup must be a data.frame like usastats or statestats, with columns PCTILE, REGION, and the names of indicators like pctlowinc")
      return(rep(NA, length(myvector)))

    } else {
      stop("lookup must be a data.frame like usastats or statestats, with columns PCTILE, REGION, and the names of indicators like pctlowinc")
    }
  }
  if (!('PCTILE' %in% names(lookup))) {
    if (shiny::isRunning()) {
      warning('lookup must have a field called "PCTILE" that contains quantiles/percentiles')
      return(rep(NA, length(myvector)))
    } else {
      stop('lookup must have a field called "PCTILE" that contains quantiles/percentiles')

    }
    }
  if (missing(zone) & any(lookup$REGION != 'USA')) {
    if (shiny::isRunning()) {
      warning('If no zone (like "NY") is specified, lookup table must have column called REGION that has "USA" in every row.')
      return(rep(NA, length(myvector)))

    } else {
      stop('If no zone (like "NY") is specified, lookup table must have column called REGION that has "USA" in every row.')

    }
    }

  if (length(varname.in.lookup.table) > 1 )  {
    if (length(zone) == 1 & length(varname.in.lookup.table) == length(myvector) ) {
      ##   allow multiple indicators at once, for a vector of corresponding values, but only 1 zone, like this:
      message("checking each value for its corresponding indicator, such as c(12,40)  for  ('pm','o3')  ")
      return( mapply(FUN = pctile_from_raw_lookup, myvector = myvector, varname.in.lookup.table = varname.in.lookup.table, MoreArgs = list(lookup = lookup, zone = zone)) )
    } else {
      ## check if being used in a running shiny app
      if (shiny::isRunning()) {
        warning("Can provide vectors of values and of zones like states, but then must specify only one variable (column name) as varname.in.lookup.table, like 'pctlowinc' -
           or can provide vectors of values and corresponding variables but only in 1 zone")

        ## return vector of all NAs, match length of input vector
        return(rep(NA, length(myvector)))

      } else{
        stop("Can provide vectors of values and of zones like states, but then must specify only one variable (column name) as varname.in.lookup.table, like 'pctlowinc' -
           or can provide vectors of values and corresponding variables but only in 1 zone")
      }

    }
  }

  if (length(zone) != length(myvector)) {
    if (length(zone) == 1) {
      # Assume they meant the one zone (e.g. a State) to apply to all the indicator values provided as myvector
      zone <- rep(zone, length(myvector))
    } else {
      if (shiny::isRunning()) {
        warning('Number of raw score values and number of zone values provided must be the same (except if only one zone value is provided, like a single state, it is assumed to apply for all)')

        ## return vector of all NAs, match length of input vector
        return(rep(NA, length(myvector)))

      } else {
        stop('Number of raw score values and number of zone values provided must be the same (except if only one zone value is provided, like a single state, it is assumed to apply for all)')
      }
    }
  }

  # remove mean - too slow? *** ####
  lookup <- lookup[lookup$PCTILE != "mean", ]

  # CHECK FOR WARNINGS overall ####

  # WARN if indicator name as provided is not a column in the lookup table # ------------------------------------------------
  if (!(varname.in.lookup.table %in% colnames(lookup))) {
    warning(paste0(varname.in.lookup.table, " must be a column in lookup table - returning NA values"))
    return(rep(NA, length(myvector)))
  }
  # warn if looks like maybe units mismatch ####
  if (any(!is.na(myvector)) && max(myvector, na.rm = TRUE) > 1 & max(lookup[,varname.in.lookup.table], na.rm = TRUE) <= 1) {
    warning("Raw scores are > 1, but lookup table values are not. Check if percentages should be expressed as fractions (0 to 1.00) instead of as integers 0-100, for ", varname.in.lookup.table)
  }

  ######################################################################### #
  # loop over zones  ####
  # (States or just USA works too, but this code is a bit inefficient now for that case)
  ######################################################################### #

  whichinterval        <- vector(length = NROW(myvector)) # empty, will store results zone by zone
  match        <- vector(length = NROW(myvector)) # empty, will store results zone by zone
  percentiles_reported <- vector(length = NROW(myvector)) # empty, will store results zone by zone

  for (z in unique(zone)) {
    ## WARN if zone does not exist in lookup table ####
    if (!(z %in% lookup$REGION)) {
      warning("zone = ", z, "was not found in the percentile lookup table column called REGION, so percentiles will be reported as NA, in zone = ", z, " for all indicators.") # but this msg will appear every single time you do this function on 1 indicator!!
      percentiles_reported[zone == z] <- NA
      next # go to next zone (for this one indicator)
    }

    myvector_selection <- myvector[zone == z]        # sort(myvector)
    myvector_lookup <-   lookup[lookup$REGION == z, varname.in.lookup.table]  # this is probably slower than could be, if just USA or large number of zones (and many indicators)

    # WARN if all or just some values in myvector_selection are NA,  ####
    #  findInterval(x=myvector_selection, vec=myvector_lookup) returns NA for each is.na(x)
    if (all(is.na(myvector_selection))) {
      if (!quiet) {
      message("Among these results, all raw scores were NA (so percentiles will be reported as NA) in zone = ", z, " for ", varname.in.lookup.table, ".")
      }
        percentiles_reported[zone == z] <- NA
      next # go to next zone (for this one indicator)
    }

    #  WARN if all or even just some entries in lookup table are NA ####
    #    findInterval crashes unless that case is handled
    # Just assume the lookup is useless for this indicator in this zone, probably because no blockgroupstats data at all for that indicator in that zone,
    # rather than figuring out why some percentiles in lookup are NA in this zone.
    if (any(is.na(myvector_lookup))) {
      # whichinterval[zone == z] <- rep(NA, length(myvector_selection))
      message("No percentile info is available in the percentile lookup table (all or at least some values here are NA, which is not allowed in lookup table), so percentile will be reported as NA, in zone = ", z, " for ", varname.in.lookup.table, ".")
      percentiles_reported[zone == z] <- NA
      next  # go to next zone (for this one indicator)
    }

    # findInterval ####
    
    # 1.) Uses findInterval to bin each percentile vector value into unique percentile vectors; Results are a list of bin values rather than acutal percentiles
    # 2.) Percentile indices are calculated based on the first nonduplicate values (indices are based on 1-100 percentile location)
    # 3.) Percentile indices are applied to the bin values vector in step 1 to assign the appropriate percentile value to vector selection
    unique_vlookup <- c(unique(myvector_lookup),Inf) #add Inf to coerce N-1 to N
    nondupe_interval <- findInterval(myvector_selection, unique_vlookup, all.inside = TRUE)

    #nondupvec <- which(!duplicated(myvector_lookup,fromLast = FALSE))
    nondupvec <- which(!duplicated(myvector_lookup,fromLast = TRUE))
    
    ## get list of duplicated values (ties)
    dupvals <- unique(myvector_lookup[which(duplicated(myvector_lookup,fromLast = TRUE))])
   
    whichinterval[zone == z] <- nondupvec[nondupe_interval]
    
    ## check if any inputted values match tied
    if(any(dupvals %in% myvector_selection)){
     
      for(d in dupvals){
        ## if they match a tied value, assign lowest of tied percentiles
        whichinterval[zone == z][myvector_selection == d] <- min(which(myvector_lookup == d))
      }
    }
   
    # WARN if raw score < PCTILE 0, in lookup ! ####
    # WARN if a raw value < minimum raw value listed in lookup table (which should be percentile zero). Why would that table lack the actual minimum? when created it should have recorded the min of each indic in each zone as the 0 pctile for that indic in that zone.
    # *** COULD IT BE THAT UNITS ARE MISMATCHED?  e.g., QUERY IS FOR RAW VALUE OF 0.35 (FRACTION OF 1) BUT LOOKUP TABLE USES RAW VALUES LIKE 35 (PERCENT. FRACTION OF 100) ?
    belowmin <- (myvector_selection < min(myvector_lookup)) 
    if (any(belowmin, na.rm = TRUE)) {
      whichinterval[zone == z][!is.na(belowmin) & belowmin]  <- 1 # which means 0th percentile
      warning("Some raw values were < min (0th PCTILE) seen in the percentile lookup table (you should confirm myvector and lookup are in same units, like percents reported as 0.00 to 1.00 versus as 0 to 100!), so percentile will be reported as 0, in zone = ", z, " for ", varname.in.lookup.table, ".")
    }
    #print(zone)
    #print(z)
    whichinterval[zone == z][is.na(belowmin)] <- 1 # will not be used but wont cause error. pctile reported will be NA in this case.
    percentiles_reported[zone == z] <- as.numeric(lookup$PCTILE[lookup$REGION == z][whichinterval[zone == z]]) # this is just in case each zone has a different number of or set of PCTILE values.
    # returns NA if belowmin is NA
    percentiles_reported[zone == z][is.na(belowmin)] <- NA
    
    
    #set percentile to zero if myvector_selection <= 0
    percentiles_reported[zone == z][myvector_selection <= 0] <- 0
    # set first nonzero percentile to second value

   #percentiles_reported[zone == z][(myvector_selection > 0 & myvector_selection < unique_vlookup[1])] <- nondupvec[2]-1
    #percentiles_reported[zone == z][(myvector_selection > 0 & myvector_selection < unique_vlookup[2])] <- high_pctiles_tied_with_min[[z]][[varname.in.lookup.table]]#nondupvec[2]-1
  } # end of loop over zones ####

  return(percentiles_reported)

  ############################################################################################################# #
  #
  ## notes on findInterval ####
  #
  # findInterval returns the gap (interval) number, 0 through length(myvector), and
  # 0 if < min of lookup, and counts it as in the gap if tied with lower edge and less than upper edge.
  # If lookup was for pctiles 1-100, the whichinterval would be the same as the percentile, including for findInterval = pctile = 0.
  # The lookup table has pctiles 0-100, where 0 is min and 0-1 is 1st 1%, so
  # the pctile should be reported as lookup[whichinterval, varname.in.lookup.table] ....
  #  unless whichinterval = 0 (i.e., place has score smaller than min of all places used in creating the lookup table) in which case report 0 for pctile.

  # ***   fix case where multiple percentiles are tied in lookup table... as with pctlingiso
  # findInterval will return the last interval, not the first, that it matches when there are duplicates,
  # which does not mesh with how the lookup table should be interpreted.
  # -- We want to report the first not last when the same value is shown as being at multiple percentiles,
  # which happens if a large percent of places are tied at some given value, such as when 30% of places have a score of zero, e.g.
  # One workaround is to check the lookup tables for cases of ties at min value, and add a very tiny amount to each of those,
  #  which will force it to report 0 percentile for that value that hasn't had the tiny amount added.
  # that seems like an awkward hack but would work.
  # We could alter the statestats table itself, and usastats. but,
  # solution: created   high_pctiles_tied_with_min  dataset
  # Rule for using it here:
  # if reported pctile per lookup function is <= these high_pctiles_tied_with_min,
  # then report instead zero as the percentile.
  #
  # To see what variables in what states have a bunch of tied values at minimum that is NOT zero, and then ties at zero:
  # datacols <- setdiff(names(statestats), c('PCTILE', 'REGION')); states <- unique(statestats$REGION);  for (myvar in datacols) {for (mystate in states) {z = statestats[mystate == statestats$REGION, myvar]
  #   if ((z[1] == z[2]) & (z[1] != 0)) {cat("in ",mystate, " for ", myvar, " = ", z[1], '\n')}}}
  # datacols <- setdiff(names(statestats), c('PCTILE', 'REGION')); states <- unique(statestats$REGION);  for (myvar in datacols) {for (mystate in states) {z = statestats[mystate == statestats$REGION, myvar]
  #   if ((z[1] == z[2]) & (z[1] == 0)) {cat("in ",mystate, " for ", myvar, " = ", z[1], '\n')}}}
  # datacols <- setdiff(names(usastats), c('PCTILE', 'REGION')); states <- unique(usastats$REGION);  for (myvar in datacols) {for (mystate in states) {z = statestats[mystate == usastats$REGION, myvar]
  #   if ((z[1] == z[2]) & (z[1] != 0)) {cat("in ",mystate, " for ", myvar, " = ", z[1], '\n')}}}
  # datacols <- setdiff(names(usastats), c('PCTILE', 'REGION')); states <- unique(usastats$REGION);  for (myvar in datacols) {for (mystate in states) {z = statestats[mystate == usastats$REGION, myvar]
  #   if ((z[1] == z[2]) & (z[1] == 0)) {cat("in ",mystate, " for ", myvar, " = ", z[1], '\n')}}}

  ## also, this probably happens for any set of tied threshold values (not only ties at min value) - but would need to confirm EJScreen was coded that way.

  # ** also using data.table might make this whole function significantly faster if statestats is a data.frame with keys REGION and PCTILE
  # pctile <- lookup[myvector >= ..varname.in.lookup.table, PCTILE[1]] # but also, if none where >= true, pctile <- lookup$PCTILE[1]

}
########################################################################### #


#' Find approx percentiles in lookup table that is in memory
#'
#' @seealso Identical to [pctile_from_raw_lookup()] [usastats] [statestats]
#' @inheritParams pctile_from_raw_lookup
#'
#' @export
#'
lookup_pctile  <- function(myvector, varname.in.lookup.table, lookup = usastats, zone = "USA") {

  pctile_from_raw_lookup(myvector = myvector, varname.in.lookup.table = varname.in.lookup.table, lookup = lookup, zone = zone)
} #  function(...) {pctile_from_raw_lookup(...)}
######################################################################### #
