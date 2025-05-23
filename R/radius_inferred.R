#' radius_inferred - utility to estimate original radius requested in getblocksnearby()
#'   if we only have the outputs of getblocksnearby()
#'
#' @details There are some cases where someone using EJAM functions like getblocksnearby() 
#'   might in a later separate step use the results of getblocksnearby() to summarize 
#'   indicator values using a function like doaggregate(), and the actual radius originally 
#'   requested is not known.
#'   
#'   This function tries to approximate what radius must have been requested for analysis, 
#'   looking at the sites2blocks information about distances to all nearby blocks
#'   near each of the analyzed sites. It is not as simple as using the max distance over all sites, 
#'   because at some sites getblocksnearby() reports one or two distances larger than 
#'   radius requested, even if avoidorphans is FALSE. That must be because the 
#'   reported distance is adjusted when it is small relative to the whole block, to 
#'   better estimate distance to average resident in the block rather than reporting 
#'   distance to the point that is the block internal point (centroid essentially). 
#'   As documented in the EJScreen information about creating proximity scores, 
#'   a facility exactly on top of the block internal point has distance zero to the point but
#'   that is not the actual distance to the average resident in the block, hence the adjustment. 
#'   Some blocks in low density areas are huge so a relatively small circular buffer (small radius)
#'   will require adjustments more often. If the block is 3 miles in radius but someone wants a
#'   radius of 1 mile in getblocksnearby() or ejamit() analysis overall, a site inside the block
#'   might be reported as having a distance of 2.7 miles because the average resident in the block
#'   is estimated to be 2.7 miles away from any site in the block. Almost 2% of US blocks are 
#'   affected by this issue for a selected radius of 1 mile, but only 1 in 1,000 are for a radius of 3 miles.
#'  
#'  This function is based largely on a practical algorithm that is accurate to within 0.01 miles
#'   the vast majority of the time for a radius of 1 to 3 miles.
#'  
#' @param s2b data.table of ejam_uniq_id, distance, etc. that is the output of getblocksnearby()
#' @param decimalsreported parameter to fine tune estimates - generally should not be changed
#' @param decimalsforinferring  parameter to fine tune estimates - generally should not be changed
#' @param pctile_of_sites  parameter to fine tune estimates - generally should not be changed
#' @param nth_furthest_block  parameter to fine tune estimates - generally should not be changed
#'
#' @return a single number such as 1.5 or 3 that is the estimate of the miles distance that was
#'   originally requested in getblocksnearby()
#' 
#' @examples  radius_inferred()
#'   # radius_inferred(getblocksnearby(testpoints_n(100), radius = 3.25))
#'   
#' @keywords internal
#' 
radius_inferred <- function(s2b = NULL, decimalsreported = 2, 
                            decimalsforinferring = 3, pctile_of_sites = 0.90, nth_furthest_block = 2) {
  
  #  guesstimate <- round(max(s2b$distance, na.rm = TRUE), 1)  # this was a simplified way to get a rough estimate for radius but now uses radius_inferred()
  
  if (is.null(s2b)) {s2b <- data.table::copy(testoutput_doaggregate_1000pts_1miles)}
  if (!data.table::is.data.table(s2b)) { data.table::setDT(s2b)}
  # setorder(s2b, -distance) # would alter by reference the passed data.table in the calling environment, I think
 
if (NROW(s2b) == 1) {nth_furthest_block <- 1} # or it will return NA since there is only the one row not a 2d furthest, etc.
   x <- s2b[order(-distance), ][ ,  .(distance = 
      round(
        distance[nth_furthest_block],   # use distance to 2d furthest block to avoid an outlier, e.g. 
        decimalsforinferring           # round here so that top few distances are nearly identical to requested radius, at most sites (where block is not too large)
      )
    ), by = "ejam_uniq_id"] 
    
    # nth_longest_distance = round(distance[nth_furthest_block], decimalsforinferring)
    # ), by="ejam_uniq_id"][ , nth_longest_distance],
   
   guesstimate <- quantile(x$distance, probs = pctile_of_sites, na.rm = TRUE)
                 # to avoid small fraction of sites where multiple distances are > actual requested radius

  as.vector(
    round(
      guesstimate,
      decimalsreported
    )
  )  
}
