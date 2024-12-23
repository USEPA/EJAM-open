

#' What percentage of this demographic group's population lives less than X miles from a site? --- *** DRAFT - NEED TO RECHECK CALCULATIONS
#'
#' @description *** DRAFT - NEED TO RECHECK CALCULATIONS
#'   This plots the cumulative share of residents found within each distance,
#'   for a single demographic group.
#' @details Also see ejamit_compare_distances() for a plot  of several indicators at several distances!
#' 
#'   This function uses the distance of each Census block from the site in conjunction with
#'   the block group demographics, to provide a relatively detailed picture of
#'   how far away residents in each group live. In contrast, the function
#'   [distance_cdf_by_group_plot()] is based on ejamit()$results_bybg_people,
#'   which provides only block group resolution information about distance.
#'
#' @param s2b output of [getblocksnearby()], or else can be
#'   a table of points with lat,lon columns and 1 row per point.
#'   If NULL (not provided as a parameter to the function),
#'   will prompt for a file to upload and use, if interactive() is TRUE, or else
#'   the function will just show an example using a random point.
#' @param sitenumber If used, one number that is the unique ID
#'   (the row number of original list of points) to look at in s2b.
#'   This should be the same as the value of s2b$ejam_uniq_id for
#'   the site to be analyzed.
#'   Will be able to omit or set to NULL to use overall aggregate of all sites.
#' 
#' @param score_colname colname in blockgroupstats for an indicator to be
#'   aggregated across blocks and blockgroups as a weighted mean
#' @param scorewts_colname colname in blockgroupstats -- like "pop" -- for the weight
#'   to use in aggregating the scores referred to by score_colname
#' @param score_label optional friendly label for the variable
#'   
#' @param radius optional radius to use as maximum analyzed or shown --
#'   if s2b was provided, this caps what is used and only shorter radii get shown
#'   (only relevant if s2b had radii larger than this radius parameter)
#'   and if s2b is not provided, interactively RStudio user is prompted
#'   to provide latlon file to analyze in getblocksnearby() and
#'   radius is used in that.
#' 
#' @return returns s2b but with more columns in it like wtdmean_within
#' @examples
#' 
#'  # Example of area where %Black is
#'  very high within 1 mile but drops by 3 miles away
#'  pts = testpoints_100[3,]
#'   plot_distance_by_pctd(
#'     getblocksnearby(pts, radius = 10, quiet = T),
#'     score_colname = "pctnhba")
#'  #browseURL(url_ejscreen_report(lat = pts$lat, lon = pts$lon, radius = 0.5))
#'  #browseURL(url_ejscreen_report(lat = pts$lat, lon = pts$lon, radius = 3))
#'  
#'  # Example of area that has higher %Hispanic as you go 
#'  # 10 to 30 miles away from this specific point
#'  pts = data.table(lat = 45.75464, lon = -94.36791)
#'  plot_distance_by_pctd(pts,
#'    sitenumber = 1, score_colname = "pcthisp")
#'  # browseURL(url_ejscreen_report(lat = pts$lat, lon = pts$lon, radius = 10))
#'  # browseURL(url_ejscreen_report(lat = pts$lat, lon = pts$lon, radius = 30))
#'  
#'  
#' @export
#'
plot_distance_by_pctd <- function(s2b = NULL, sitenumber = 1, #  NULL, 
                                  score_colname = names_these[3], 
                                  scorewts_colname = "pop",
                                  score_label = fixcolnames(score_colname, "r", "shortlabel"),
                                  radius = 30
) {
  
  if (missing(sitenumber)) {warning("aggregate of multiple sites not yet implemented - using site #1")}
  
  if (!(score_colname %in% names(blockgroupstats))) {stop(cat(score_colname, "was not found in colnames(blockgroupstats) \n"))}
  if (!(scorewts_colname %in% names(blockgroupstats))) {stop(cat(scorewts_colname, "was not found in colnames(blockgroupstats) \n"))}
  
  if (is.null(s2b)) {
    if (interactive()) {
      s2b <- getblocksnearby(radius = radius, quiet = T)
    } else {
      warning('s2b not provided, so showing random example data only')
      pts = suppressWarnings(testpoints_n(1))
      cat(paste0("site: ", pts$lat, ", ", pts$lon, "\n"))
      s2b <- getblocksnearby(pts, radius = radius, quiet = T)
      sitenumber <- 1
    }
  }
  if (!("blockid" %in% names(s2b))) {
    # maybe they provided just latlon table
    s2b <- latlon_from_anything(s2b)
    if (all("lat" %in% names(s2b), "lon" %in% names(s2b)) ) {
      if (!all(is.na(s2b$lat)) && !all(is.na(s2b$lon))) {
        s2b <- getblocksnearby(s2b, radius = radius, quiet = T)
      } else {
        warning("s2b must be results of getblocksnearby() or else a table of points with lat,lon columns and 1 row per point")
        pts = suppressWarnings(testpoints_n(1))
        cat(paste0("site: ", pts$lat, ", ", pts$lon, "\n"))
        s2b <- getblocksnearby(pts, radius = radius, quiet = T)
        sitenumber <- 1
      }
    } else {
      warning("s2b must be results of getblocksnearby() or else a table of points with lat,lon columns and 1 row per point")
      warning('s2b not provided, so showing sample data only')
      pts = suppressWarnings(testpoints_n(1))
      cat(paste0("site: ", pts$lat, ", ", pts$lon, "\n"))
      s2b <- getblocksnearby(pts, radius = radius, quiet = T)
      sitenumber <- 1
    }
  }
  if (is.null(sitenumber)) {
    stop("aggregate of multiple sites not yet implemented")
    sitenumber <- 0
    s2b$ejam_uniq_id <- 0
    # this should fool it into treating all the sites as one site 
    # except note it will not be in only 1 state, so account for that later...
  }
  
  allvarnames <- unique(c("bgid", score_colname, scorewts_colname))
  
  # confirm that sitenumber %in% s2b$ejam_uniq_id in case sitenumber provided is bad or missing
  if (!("ejam_uniq_id" %in% colnames(s2b)) || !(sitenumber %in% s2b$ejam_uniq_id)) {stop("sitenumber of ", sitenumber, " not found among s2b$ejam_uniq_id values")}
  # limit analysis to this one site and limited radius to view and drop unpopulated blocks
  s2b <- data.table::copy(s2b[ejam_uniq_id == sitenumber & blockwt > 0 & distance <= radius, ])
  # get the relevant EJScreen demog or envt indicator scores, by block group
  s2b <- merge(s2b, blockgroupstats[pop > 0, c(..allvarnames)], all.x = TRUE, all.y = FALSE, by = "bgid")
  # sort by increasing distance
  data.table::setorder(s2b, distance)  
  
  ###################################### # 
  # FORMULAS to aggregate over blocks and bgs
  # 
  # 1) Was using hard-coded variables that are calculated only as percentage of population total
  # s2b[ , blockpop  :=  pop * blockwt]
  # s2b$dpop <- s2b[ , ..countvarname] # bg count    #### ***  but this only works for a single indicator named by countvarname, not a vector of them?
  # s2b[ , blockdpop := dpop * blockwt] # block counts
  # s2b[ , cumpop  := cumsum(blockpop)]  # cum pop count within <x distance
  # s2b[ , cumdpop := cumsum(blockdpop)] # same but for countvarname group
  # s2b[ , pctdwithin := cumdpop / cumpop] # %D among everyone within X distance  **** uses ratio of sums of counts, not pop wtd mean of %s or other raw scores
  # 
  # 2) But to be more accurate one would use the exact right formula depending on each indicator.
  # That could be adjusted to use calc_ejam() for example. 
  #
  # 3)  new wtd mean method ####
  s2b$scorewts <- s2b[, ..scorewts_colname] # not as efficient but syntax is awkward otherwise
  s2b$scores  <- s2b[, ..score_colname]
  s2b[, wtdmean_within := cumsum(scorewts * blockwt * scores) / cumsum(scorewts * blockwt)]
  ###################################### # 
  
  ## STATE(S) ####
  ## need to handle State identification and State avg, 80th pctile
  ## for the case where user wants plot of all aggregated sites in >1 state!
  ## In doaggregate() we handle that by creating a simulated state that is the 
  # wtd mean of states in the analysis, weighted by share of analyzed pop that is in each state. 
  
  if (sitenumber == 0) {
    SITENUMBERTEXT <- "any one or more of the analyzed sites"
    
    # myST <- state_from_blockid(as.vector(unlist(s2b[ejam_uniq_id == sitenumber, blockid[1]])))
    # STATEMEAN = statestats[statestats$PCTILE == "mean" & statestats$REGION == myST, dpctvar]
    # STATE80 =   statestats[statestats$PCTILE == "80"   & statestats$REGION == myST, dpctvar]
    
  } else {
    SITENUMBERTEXT <- paste0("site number ", sitenumber)
    myST <- state_from_blockid(as.vector(unlist(s2b[ejam_uniq_id == sitenumber, blockid[1]]))) # EJAM:::
    STATEMEAN = statestats[statestats$PCTILE == "mean" & statestats$REGION == myST, score_colname]
    STATE80 =   statestats[statestats$PCTILE == "80"   & statestats$REGION == myST, score_colname]
  }
  USMEAN = usastats[    usastats$PCTILE == "mean"                            , score_colname]
  US80   = usastats[    usastats$PCTILE == "80"                              , score_colname]
  
  plot(
    s2b$distance,
    s2b$wtdmean_within,
    type = "b",
    xlab = "Distance (miles)", ylab = "Indicator value within X miles",
    xlim = c(0, max(s2b$distance)),
    ylim = c(0, 1.10 * max(STATE80, US80, s2b$wtdmean_within, na.rm = TRUE)),
    main = paste0(score_label, " as a function of distance from ", SITENUMBERTEXT)
  )
  
  abline( h = STATEMEAN, col = "red")
  abline( h = STATE80,   col = "red",  lty = 4, lwd = 0.5)
  
  abline( h = USMEAN, col = "blue")
  abline( h = US80, col = "blue", lty = 4, lwd = 0.5)
  legend("topright", legend = c(
    "State 80th %ile blockgroup", "State overall",
    "US 80th %ile bg", "US overall"), 
    fill = c(
      "red", "red", 
           "blue", "blue"
    ))
  return(s2b)
}
