#' Calculate a proximity score for every blockgroup - DRAFT WORK IN PROGRESS
#' 
#' Indicator of proximity of each blockgroups to some set of facilities or sites. 
#' also see getfrsnearby()
#' 
#' @details  Proximity score is sum of (1/d) where each d is distance of a given site in km, 
#'   summed over all sites within 5km, as in EJScreen.
#'   
#'   getblocksnearbyviaQuadTree.R() and maybe doaggregate()?
#'   
#'    has a bit of code in it to do some of what this function does.
#'   
#' @param pts data.table of lat lon
#' @param countradius distance within in which nearby sites are counted to create proximity score.
#'   In miles, and default is 5km (8.04672 miles)
#'   which is the EJScreen zone for proximity scores based on counts.
#' @param maxradius max distance in miles to search for nearest single facility,
#'   if none found within countradius. EJScreen seems to use 1,000 km as the max to search,
#'   since the lowest scores for proximity scores of RMP, TSDF, or NPL are around 0.001, 
#'   meaning approx. 1/1000 km and km_per_mile = 1.609344
#'   so 1000 km is 1000 / 1.609344 = 621.3712 miles
#' @param quadtree must be called localtree, an index of block locations, 
#'   built during use of EJAM package. see [quaddata]
#' @return data.table with proximityscore, bgfips, lat, lon, etc.
#' @import data.table
#' @export
#'
#' @examples 
#'  # pts <- testpoints_100
#'  # x <- proxistat2(pts = pts[1:1000,], quadtree = localtree) 
#'  #
#'  # summary(x$proximityscore)
#'  # # analyze.stuff   pctiles(x$proximityscore)
#'  # plot(x = x$lon, y = x$lat)
#'  # tops = x$proximityscore > 500 & !is.infinite(x$proximityscore) & !is.na(x$proximityscore)
#'  # points(x = x$lon[tops], y = x$lat[tops], col="red")
#'  
proxistat2 <- function(pts, countradius = 8.04672, maxradius = 621.3712, quadtree = NULL) {
  
  if (!('proxistat' %in% installed.packages())) {
    warning("this does not work without proxistat package dataset 
       OR having block area 
       or at least effective radius 
       in blockpoints or blockwts table.
       ")
    return(NULL)
  }

  if (is.null(quadtree)) {
    if (!exists(localtree)) {
      indexblocks() # will load data as needed
    }
    quadtree <- localtree
  }
  
  
  warning("dataset for most but not all blocks -
          PR and Island Area lacked block area data in source used as of 2023 for EJAM")
  
  warning("if none found within radius of 5km, this proximity score function does not yet create score based on single nearest - see source code for notes")
  
  stop('function not written yet')
  
  # excerpt from 2017 tech doc on using min dist to create proximity scores
  #  but the same idea applies if EJAM is
  #  calculating and reporting distance from each site to avg resident in each block and then bg) ---
  # 
  # Since we cannot easily find out how the
  # residents are actually distributed in those areas, we made two simplifying assumptions:
  # - residents are evenly distributed across the surface area of each block, and
  # - each block can be represented by a circle whose radius is
  
  # [Block area / Pi]^(1/2) 
  
  # We call this latter value the "Block Area Equivalent Radius."
  
  # Our investigations indicate that for any dij less than the Block Area Equivalent Radius, 0.9 times that
  # value is a reasonable representation of the average distance from the facility for all residents in the
  # block. We call this the dij corrected.
  # Our computational scheme determines the dij values as described above, tests for the comparison with
  # Block Area Equivalent Radius, and substitutes dij corrected values. We found that we needed to make
  # that correction for less than 1% of all facility/block combinations in an early testing dataset that used
  # 2005-2009 ACS data.
  # 
  ######################################## #
  # Sequence of steps in finding d value(s):
  ######################################## #
  #
  
  ######################################## #
  
  #    0) block_radius_miles is PRECALCULATEd EFFECTIVE RADIUS FOR EVERY BLOCK IN USA, in miles ####   
  #  as in EJScreen proximity scores
  # > dim( blockpoints)
  # [1] 8174955       3
  # blockwts$block_radius_miles  has the info in miles effective radius
  
  km_per_mile <- meters_per_mile / 1000  # km_per_mile = 1.609344  # meters_per_mile #   [1] 1609.344

  # block_radius_miles  is the distance at which we start to adjust
  # min.dist <- 0.9 * block_radius_miles
  
 ######################################### # 
  # 1) if not done already via getblocksnearby(), get distances that are <= radius using get.distances()
  
  # but here we want to create a score for each US block then roll up to bg scores,
  # by for each block finding all nearby sites, rather than EJAM analysis of 
  # doing at each site finding all nearby blocks 
  # unless for efficiency the other way around.
  # EFFICIENCY QUESTION: 
  #  Obvious possible algorithm at least if running it for the entire FRS of all facilities, is to 
  #  STEP 1: loop through all 8 million US blocks, and for each block count all nearby facilities (sites),
  #   but vast majority of blocks will have zero sites nearby, so 
  #   STEP 2: for every block with zero sites nearby, expand search somehow until finding nearest 1 site. HOW?
  
  #  But for users needing proxistat for a few hundred or thousand site types (e.g. CAFOs)  
  #  for all 8 mill us blocks still,  
  # another approach to check is 
  # STEP 1: loop through just the 10k, 100k, or 1.5 million sites (is it faster than step 1 above?), 
  # and for each site find all nearby blocks, maybe 1k each, say. 
  #   then do STEP 2 as above.
  
  # THE VAST MAJORITY OF BLOCKS WILL HAVE ZERO WITHIN THE 5 KM RADIUS, SO NEAREST 1 IS BASIS FOR THEIR SCORE, BUT
  #   SOME WILL EVEN HAVE ZERO WITHIN THE MAX RADIUS TO CHECK
  # if none found within radius of 5km, this func does not yet create score based on single nearest
  
  #   obvious way, blocks as sites and sites as blocks?... but getblocksnearby is not written to enable that.
  
  # other way: like EJAM but then have to invert what is near what:
  
  sites2blocks <- getblocksnearby(sitepoints = pts, radius = countradius, maxradius = maxradius, avoidorphans = TRUE)
  
  
  # we will AGGREGATE BY blockid, not by site  , to create proxistat
  
  
  
  
  
  
  # 2) where d < min.dist, set d <- min.dist to adjust it upwards (ie use the max of min.dist and distance??)
  ######################################## #
  # ADJUST THE VERY SHORT DISTANCES ####
  
  # distance gets adjusted to be the minimum possible value,  0.9 * effective radius of block_radius_miles (see EJScreen Technical Documentation discussion of proximity analysis for rationale)
  #
  # use block_radius_miles here, to correct the distances that are small relative to a block size.
  # This adjusts distance the way EJScreen does for proximity scores - so distance reflects distance of sitepoint to avg resident in block
  # (rather than sitepoint's distance to the block internal point),
  # including e.g., where distance to block internal point is so small the site is inside the block.
  # This also avoids infinitely small or zero distances.
  # 2 ways considered to do join here - may be able to optimize.
  # a) try to do join that updates sites2blocks by reference - not sure it works this way, but goal was to make join faster:
  # sites2blocks[blockwts, .(ejam_uniq_id,blockid,distance,blockwt,bgid, block_radius_miles), on = 'blockid']
  # b) try to do join that updates sites2blocks by making a copy? This does work:
  
  sites2blocks <-  blockwts[sites2blocks, .(ejam_uniq_id, blockid, distance, blockwt, bgid, block_radius_miles), on = 'blockid'] 
  
  # 2 ways considered here for how exactly to make the adjustment: 
  
  sites2blocks[distance < block_radius_miles, distance := 0.9 * block_radius_miles]  # assumes distance is in miles
  # or a more continuous adjustment for when dist is between 0.9 and 1.0 times block_radius_miles: 
  # sites2blocks_dt[ , distance  := pmax(block_radius_miles, distance, na.rm = TRUE)] # assumes distance is in miles
  
  # drop that info about area or size of block to save memory. do not need it later in sites2blocks
  sites2blocks[ , block_radius_miles := NULL]
  ######################################## #
  
  sites2blocks[ , distance.km := pmax(countradius * km_per_mile, distance * km_per_mile, na.rm = TRUE)] 
  
  # that would convert distance from miles to km not meters !!
  # collapse::fmin() is probably much faster
  
  
  # 3)     and for those, check again to see if new d is still <= radius. keep only if d<=radius now. *** 
  # 4) for each frompoints, if no distances were found, get nearest single d at any radius,
  #       originally thought perhaps by expanding outwards step by step until at least one is found (but not worth the overhead vs just finding ALL d and picking min)
  
  # steps 3 and 4 had not yet been implemented as of 1/29/23.  
  
  # 8.04672 miles is 5 km which is the EJScreen max search range for proximity scores
  # # FACILITY DENSITY INDICATOR 
  #  AS PROXIMITY SCORE FOR SITES IN FRS
  

  
  
  #     TO BE ADDED HERE 
  
  
  
  # see also older notes:
  
  #  frsdensity/SCRIPT_how_many_blocks_near_FRS.R
  # 
  # frsdensity/FACILITY_DENSITY_PROXIMITY_SCORE.R
  
  
  
  
  #  ADJUST DISTANCE USING A MINIMUM DISTANCE ####
  # see code in getblocksnearby() now
  # creating min dist and joining was temporarily done here
  
  
  
  
  
  # create score per BLOCK = sum of sites wtd by 1/d ####
  
  blockscores <- sites2blocks_dt[ , sum(1 / distance.km, na.rm = TRUE), by = blockid] 
  # result is data.table with blockid, V1
  # blockscores[is.infinite(V1), V1 := 999]
  
  
  
  x <- data.table::merge.data.table(blockwts, blockscores, on = "blockid", all.x = FALSE, all.y = TRUE)
  
  # create score per BLOCK GROUP = popwtd mean of block scores ####
  
  bgscore <- x[, sum(V1 * blockwt, na.rm = TRUE)/sum(blockwt, na.rm = TRUE), by = bgid]
  setnames(bgscore, 'V1', "proximityscore")
  bgscore = merge(bgscore, bgpts, by = "bgid", all.x = TRUE, all.y = FALSE)
  
  cat("proximity score is sum of (1/d) where each d is distance of a given site in km, summed over all sites within 5km \n")
  return(bgscore)
}
