
#' utility - estimate how large table might be that will be output of getblocksnearby()
#'
#' @param nsites count of sites
#' @param radius radius in miles
#'
#' @return a number
#'
#' @export
#' @keywords internal
#' 
getblocks_predict_blocks_per_site <- function(nsites, radius) {
  # rough estimate of how many rows sites2blocks might be after
  # sites2blocks <- getblocksnearby(radius = radius) 
  #  
  round(  nsites * (radius^1.54 * 200 - 0.15 * radius^3) / 1000 , 0)
}
######################################################################################### # 

## no documentation
##
blockcounts_table_just_counts <- function(blockcounts, cuts = c(-1, 0, 9, 29, 999999)) {
  table(cut(blockcounts, cuts))
}
######################################################################################### # 

## no documentation
##
text_in_or_within_x_miles_of <- function(radius) {
  ifelse(radius == 0,
         "in", 
         # within x mile(s) of
         paste0("within ", radius, " mile", ifelse(radius == 1, "", "s"), " of")
  )
}
######################################################################################### # 

#' utility - How many blocks (<10, <30, etc.) are near the sites (pop density affects accuracy)
#'
#' @param blockcounts vector like from 
#'   
#'   out <- testoutput_ejamit_1000pts_1miles
#'   
#'   blockcounts <- out$results_bysite$blockcount_near_site
#'
#' @param cuts optional vector defining bins
#' @param labels optional vector of text labels for bins
#' @return data.frame of counts in bins
#' @noRd
#' 
blockcounts_table <- function(blockcounts, 
                              cuts = c(-1, 0, 9, 29, 999999), 
                              labels = c("Not even 1", "some but <10", "10-29", "at least 30 blocks")) {

  justcounts <- blockcounts_table_just_counts(blockcounts = blockcounts, cuts = cuts)
  x <- cbind(sites = justcounts)
  rownames(x) <- NULL
  x <- data.frame(x)
  x$blocks_per_site <- labels
  x
}
######################################################################################### # 

#' utility - Plot How many blocks (<10, <30, etc.) are near the sites (pop density affects accuracy)
#'
#' @param blockcounts vector like from 
#'   
#'   out <- [testoutput_ejamit_1000pts_1mile]s
#'   
#'   blockcounts <- out$results_bysite$blockcount_near_site
#'
#'   or 
#'   
#'   getout <- testoutput_getblocksnearby_1000pts_1miles
#'   
#'   blockcounts <- getout[ , .N, keyby = 'ejam_uniq_id']$N
#'   
#' @param radius in miles (can use 0 if shapefiles analyzed)
#' @param n optional count of sites should be length(blockcounts)
#' @param cuts optional vector defining bins
#' @param labels optional vector of text labels for bins
#' @param ... passed to [barplot()]
#'
#' @return barplot
#' @noRd
#'
blockcounts_plot <- function(blockcounts, radius, n = length(blockcounts), 
                             cuts =  c(-1, 0, 9, 29, 999999), 
                             labels = c("Not even 1", "some but <10", "10-29", "at least 30 blocks"), 
                             ...) {
  
  justcounts <- blockcounts_table_just_counts(blockcounts = blockcounts, cuts = cuts)
  milestext <- text_in_or_within_x_miles_of(radius)
  maintext  <- paste0("How many blocks are ", milestext, " these ", n, " sites?")
  
  barplot(
    justcounts,
    names.arg = labels,
    main = maintext,
    ylab = "# of facilities (or sites or polygons)", 
    xlab = "# of blocks nearby (or in polygon)",
    ...
  )
}
######################################################################################### # 

#' utility - How many blocks are near the sites (pop density affects accuracy)
#' 
#' @description Number of blocks near avg site, how many sites have only 1 or fewer than 30 blocks nearby, etc.
#' 
#' @param x The output of [getblocksnearby()] like [testoutput_getblocksnearby_1000pts_1miles]
#' @param varname colname of variable in data.table x that is the one to summarize by
#' @return invisibly, a list of stats, and plot
#' @import data.table
#' @seealso [getblocks_diagnostics()]
#' 
#' @export
#' @keywords internal
#'
getblocks_summarize_blocks_per_site <- function(x, varname='ejam_uniq_id') {
  
  blocks_per_site_histo <- table(table(x[ , ..varname]))
  blocks_per_site_histo <- data.frame(
    blocks_nearby =  as.numeric(names(blocks_per_site_histo)), 
    freq_of_sites =  as.numeric(blocks_per_site_histo)
  )
  
  blockcounts = x[ , .N, keyby = varname]$N
  blockcounts_plot(x[ , .N, keyby = varname]$N, radius = round(max(x$distance),2))
  
  cat('Range and mean of count of blocks nearby the various sites:\n\n')
  print(summary(as.numeric(table(x[ , ..varname]))))
  cat("\n")
  
  print(blockcounts_table(blockcounts = blockcounts))
  cat("\n")
  invisible(blocks_per_site_histo)
}
######################################################################################### # 


#' utility - How many sites are near the blocks (site density near residents)
#'
#' @param x The output of [getblocksnearby()] like [testoutput_getblocksnearby_10pts_1miles]
#' @param varname colname of variable in data.table x that is the one to summarize by
#' @return invisibly, a list of stats
#' @import data.table
#' @seealso [getblocks_diagnostics()]
#'
#' @keywords internal
#'
getblocks_summarize_sites_per_block <- function(x, varname='blockid') {
  table(table(x[ , ..varname]))
}
######################################################################################### # 


#' utility - How many blocks and many other stats about blocks and sites
#'
#' @param x The output of [getblocksnearby()] like [testoutput_getblocksnearby_10pts_1miles]
#' @param detailed if TRUE, also shows in console a long table of frequencies via [getblocks_summarize_blocks_per_site()]
#' @param see_distanceplot if TRUE, also draws scatter plot of adjusted vs unadj distances
#' @param see_pctiles set to TRUE to see 20 percentiles of distance in a table
#' @return A list of stats 
#' @seealso This relies on  [getblocks_summarize_blocks_per_site()] and [getblocks_summarize_sites_per_block()]
#' @examples 
#'   getblocks_diagnostics(testoutput_getblocksnearby_10pts_1miles)
#'   # library(data.table)
#'   x <- data.table::copy(testpoints_10)
#'   setDT(x)
#'   pts <- rbind(data.table(lat = 40.3, lon = -96.23),
#'     x[ , .(lat, lon)])
#'  z <- getblocksnearbyviaQuadTree(pts, 1, quadtree = localtree, quiet = T)
#'  z[ , .(blocks = .N) , keyby = 'ejam_uniq_id']
#'  plotblocksnearby(pts, radius = 1, sites2blocks = z)
#'  zz <- getblocks_diagnostics(z, detailed = T, see_pctiles = T)
#' cbind(stats = zz)
#' 
#'   getblocks_diagostics(testoutput_getblocksnearby_1000pts_1miles, see_distanceplot = TRUE)
#'   
#' @import data.table
#'
#' @export
#'
getblocks_diagnostics <- function(x, detailed=FALSE, see_pctiles=FALSE, see_distanceplot = FALSE) {
  if (NROW(x) == 0) {warning('no blocks found nearby'); return(NA)}
  
  prit <- function(x) {prettyNum(x, big.mark = ',')}
  
  # Distances ####
  
  cat("\n   DISTANCES FROM BLOCKS (AND RESIDENTS) TO SITES (AND FOR CLOSEST SITE) \n")
  if (see_pctiles) {
    cat("\n")
    print(cbind(percentiles.of.distance = quantile(x$distance, probs = (0:20)/20)))
  }
  cat("\n")
  cat(max(x$distance_unadjusted, na.rm = TRUE), "miles is max. distance to block internal point (distance_unadjusted)  ", "\n")
  cat(max(x$distance, na.rm = TRUE), "miles is max. distance to average resident in block (distance reported)  ",  "\n")
  cat(min(x$distance_unadjusted, na.rm = TRUE), "miles is shortest distance to block internal point (distance_unadjusted)  ", "\n")
  cat(min(x$distance, na.rm = TRUE), "miles is shortest distance to average resident in block (distance reported)  ",  "\n")
  # cat(round(mean(x$distance, na.rm = TRUE), 2), "miles is mean distance to average resident", "\n")
  
  if ("distance_unadjusted" %in% names(x)) {
    blockcount_distance_adjusted_up  <- x[distance > distance_unadjusted, .N]
    blockcount_distance_adjusted_down <-  x[distance < distance_unadjusted, .N]
    blockcount_distance_adjusted <- blockcount_distance_adjusted_up + blockcount_distance_adjusted_down
    sitecount_distance_adjusted <- data.table::uniqueN(x[distance != distance_unadjusted, ejam_uniq_id])
    
    cat(paste0(blockcount_distance_adjusted,
               " block distances were adjusted (these stats may count some blocks twice if adjusted at 2+ sites)\n"))
    cat(paste0("  ", blockcount_distance_adjusted_up,
               " block distances were adjusted up (reported dist to avg resident is > dist to block internal point)\n"))
    cat(paste0("  ", blockcount_distance_adjusted_down,
               " block distances were adjusted down (reported < unadjusted)\n"))
    cat(paste0(sitecount_distance_adjusted,
               " unique sites had one or more block distances adjusted due to large block and short distance to block point\n"))
  } else {
    blockcount_distance_adjusted_up <- NA
    blockcount_distance_adjusted_down <- NA
    blockcount_distance_adjusted <- NA
    sitecount_distance_adjusted <- NA
  }
  
  cat("\n")
  
  if (detailed) {
    # Counts of blocks nearby, frequency of x nearby ####
    print(getblocks_summarize_blocks_per_site(x))
    # returns tables that gives Range and mean of count of blocks nearby the various sites,
    #   how many sites have only 1 block nearby, or <30 nearby, etc.
    #
    cat("\n\n")
  }
  
  # calculate extra stats ####
  
  sitecount_unique_out       <- data.table::uniqueN(x, by = 'ejam_uniq_id')
  blockcount_unique          <- data.table::uniqueN(x, by = 'blockid') # how many blocks are there, counting each once, not "how many blocks are unique" ie appear only once
  blockcount_incl_dupes      <- data.table::uniqueN(x)
  ratio_blocks_incl_dupes_to_unique <- blockcount_incl_dupes / blockcount_unique
  
  sites_per_block_histo <- getblocks_summarize_sites_per_block(x) #table(table(x$blockid))
  z <- sites_per_block_histo['1']  
  uniqueblocks_near_only1site  <-  ifelse(is.na(z),0,z) 
  z <- sites_per_block_histo['2']  
  uniqueblocks_near_exactly2site <- ifelse(is.na(z),0,z) 
  z <- sites_per_block_histo['3']  
  uniqueblocks_near_exactly3site <- ifelse(is.na(z),0,z) 
  uniqueblocks_near_multisite  <- blockcount_unique - uniqueblocks_near_only1site
  pct_of_unique_blocks_in_overlaps <- uniqueblocks_near_multisite / blockcount_unique
  
  count_block_site_distances <- blockcount_incl_dupes # number of rows in output table of all block-site pairs with their distance.
  blockcount_avgsite         <- blockcount_incl_dupes / sitecount_unique_out
  
  sumstats <- list(
    sitecount_unique_out = sitecount_unique_out, 
    # sites_withany_overlap = as.numeric(getblocks_summarize_sites_per_block(x)['2']),
    # that tells you how many blocks are near 2 sites, but not how many or which sites those were. 
    
    blockcount_avgsite = blockcount_avgsite, 
    
    blockcount_incl_dupes = blockcount_incl_dupes, 
    blockcount_unique = blockcount_unique, 
    
    uniqueblocks_near_only1site = uniqueblocks_near_only1site,
    uniqueblocks_near_exactly2site = uniqueblocks_near_exactly2site,
    uniqueblocks_near_exactly3site = uniqueblocks_near_exactly3site,
    ratio_blocks_incl_dupes_to_unique = ratio_blocks_incl_dupes_to_unique,
    pct_of_unique_blocks_in_overlaps = pct_of_unique_blocks_in_overlaps,
    
    count_block_site_distances = count_block_site_distances,
    uniqueblocks_near_multisite = uniqueblocks_near_multisite, 
    
    max_distance_unadjusted = max(x$distance_unadjusted, na.rm = TRUE),
    max_distance = max(x$distance, na.rm = TRUE),
    min_distance_unadjusted = min(x$distance_unadjusted, na.rm = TRUE),
    min_distance = min(x$distance, na.rm = TRUE)
  )
  
  # Print those extra stats to console ####
  
  cat("  BLOCK COUNTS PER SITE (FEWER MEANS HIGHER UNCERTAINTY AT THOSE SITES)\n\n")
  
  cat(paste0(prit(round(blockcount_avgsite, 0)), ' blocks are near the avg site or in avg buffer\n(based on their block internal point, like a centroid)\n'))
  cat("\n")
  blockcounts <- x[ , .N, keyby = "ejam_uniq_id"]$N
  print(blockcounts_table(blockcounts = blockcounts))
  
  cat("\n  BLOCK COUNTS TOTAL AND IN OVERLAPS OF AREAS (MULTIPLE SITES FOR SOME RESIDENTS) \n\n")
  
  cat(paste0(prit(blockcount_unique), " actual unique blocks total\n" ))
  cat(paste0(prit(blockcount_incl_dupes), " blocks including doublecounting in overlaps, 
             in final row count (block-to-site pairs table)\n" ))
  cat(paste0(prit(ratio_blocks_incl_dupes_to_unique), ' is ratio of blocks including multicounting / actual count of unique blocks\n'))
  cat(paste0(prit(100 * round(pct_of_unique_blocks_in_overlaps, 3)), 
             '% of unique blocks could get counted more than once 
             because those residents are near two or more sites 
             (assuming they live at the block internal point\n'))
  # cat(prit(count_block_site_distances), ' = count_block_site_distances',  '\n')
  # cat(prit(uniqueblocks_near_multisite),' = uniqueblocks_near_multisite ', '\n')
  
  cat("\n  SITE COUNTS TOTAL AND IN OVERLAPS OF AREAS (MULTIPLE SITES FOR SOME RESIDENTS)\n\n")
  
  cat(paste0(prit(sitecount_unique_out), ' unique output sites\n\n'))
  # cat(paste0(prit(sites_withany_overlap), ' sites do not overlap with any others\n'))
  
  cat(paste0(prit(uniqueblocks_near_only1site) ,    ' blocks (and their residents) have exactly 1 site nearby \n'))
  cat(paste0(prit(uniqueblocks_near_exactly2site) , ' blocks (and their residents) have exactly 2 sites nearby \n'))
  cat(paste0(prit(uniqueblocks_near_exactly3site) , ' blocks (and their residents) have exactly 3 sites nearby \n'))
  
  
  # PLOTS ####
  
  # Plot nice histogram of count of blocks near each site, by default now
  blockcounts_plot(blockcounts = blockcounts, radius = round(max(x$distance, na.rm = TRUE),2))
  
  if (see_distanceplot) {
    # show differences between unadjusted and adjusted distances
    # x = getblocksnearby(testpoints_1000, radius = 1, quiet = T)
    if (NROW(x) > 10000) {
      cat("plotting a sample of blocks since too many to easily plot them all\n")
      x <- x[sample(1:NROW(x), 10000), .(distance, distance_unadjusted)]
    }
    plot(x$distance, x$distance_unadjusted, 
         xlab = "Adjusted Distance to avoid unrealistic short distances", 
         ylab = "Distance as calculated (unadjusted for short distances, but should be < radius if avoidorphans=F ?)", 
         xlim = c(0,max(x$distance, x$distance_unadjusted, na.rm = T)), 
         ylim = c(0,max(x$distance, x$distance_unadjusted, na.rm = T)))
    # boxplot(x$distance ~ x$ejam_uniq_id)
  }
  invisible(sumstats)
}
######################################################################################### # 
