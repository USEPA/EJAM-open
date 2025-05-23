#' Fast way to find nearby points - For each frompoint, it finds distances to all nearby topoints (within radius)
#'
#' @description Given a set of frompoints (e.g., facilities or blocks) and a specified radius in miles,
#'   this function quickly finds all the topoints (e.g., blocks or facilities) near each point.
#'   If from and to are facilities and census blocks, respectively, this can be used to aggregate
#'   over block groups near a facility for an EJAM analysis. But if it is used to define from
#'   as blocks and to as facilities, it finds all facilities near each block, which is how
#'   proxistat works to create proximity indicators.
#'
#' @details
#'   The explanation below is assuming frompoints are "sites" such as facilities and
#'   topoints are Census blocks, but they can be reversed as long as the quaddata index
#'   passed is an index of the topoints.
#'
#'   For each point, this function uses the specified search radius and finds the distance to
#'   every topoint within the circle defined by the radius.
#'   Each topoint is defined by its latitude and longitude.
#'
#'   Results are the sites2points table that would be used by doaggregate(),
#'   with distance in miles as one output column of data.table.
#'
#' @param frompoints data.table with columns lat, lon giving point locations of
#'   sites or facilities or blocks around which are circular buffers defined by radius.
#'
#'  - pointid is the indexed topoints id.
#'
#'  - ejam_uniq_id is the frompoints id
#'
#' @param radius in miles, defining circular buffer around a frompoint
#' @param maxradius miles distance (max distance to check if not even 1 topoint is within radius)
#' @param min_distance miles minimum distance to use for cases where from and to points are
#'   identical or almost the same location.
#' @param avoidorphans logical If TRUE, then where not even 1 topoint
#'   is within radius of a frompoint,
#'   it keeps looking past radius, up to maxradius, to find nearest 1 topoint
#'
#'   Note that if creating a proximity score, by contrast, you instead want to find nearest 1 SITE if none within radius of this BLOCK.
#' @param quadtree (a pointer to the large quadtree object)
#'    created using indexpoints() which uses the SearchTree package.
#' @param quaddatatable data.table like quaddata passed to function
#'   - the data.table used to create quadtree, such as blockpoints or frs.
#'
#' @param report_progress_every_n Reports progress to console after every n points
#' @param quiet Optional.
#' @param retain_unadjusted_distance set to FALSE to drop it and save memory/storage. If TRUE,
#'   the distance_unadjusted column will save the actual distance of site to the topoint,
#'   which might be zero. adjusted distance uses a lower limit, min_distance
#' @param updateProgress, optional function to update Shiny progress bar
#'
#' @seealso  [getpointsnearby()]
#' @import data.table
#'
#' @export
#'
getpointsnearbyviaQuadTree  <- function(frompoints, radius = 3, maxradius = 31.07, avoidorphans = FALSE,
                                        min_distance = 100/1760, retain_unadjusted_distance = TRUE,
                                        report_progress_every_n = 500, quiet = FALSE,
                                        quadtree,
                                        quaddatatable,
                                        updateProgress = NULL) {

  
  
  
  if (!is(quadtree,"QuadTree")) {
    if (shiny::isRunning()) {
      warning('quadtree must be an index created with indexpoints(pts), from SearchTrees package with treeType = "quad" and dataType = "point"')
      return(NULL)
    } else {
      stop('quadtree must be an index created with indexpoints(pts), from SearchTrees package with treeType = "quad" and dataType = "point"')
    }
  }
  if (missing(frompoints)) {
    if (shiny::isRunning()) {
      warning("frompoints missing  ")
      return(NULL)
    } else {
      stop("frompoints missing  ")
    }
  }
  stopifnot(is.data.frame(frompoints), "lat" %in% colnames(frompoints), "lon" %in% colnames(frompoints), NROW(frompoints) >= 1, is.numeric(frompoints$lat))
  if (missing(quadtree)) {
    if (shiny::isRunning()) {
      warning("quadtree ")
      return(NULL)
    } else {
      stop("quadtree is missing ")
    }
  }
  if (!is(quadtree,"QuadTree")) {stop('quadtree  is not class quadtree')}
  stopifnot(is.numeric(radius), radius <= 100, radius >= 0, length(radius) == 1)
  if (missing(radius)) {warning("radius missing so using default radius of 3 miles")}

  if (!data.table::is.data.table(frompoints)) {data.table::setDT(frompoints)} # should we set a key or index here  ? ***


  if (!('ejam_uniq_id' %in% names(frompoints))) {
    frompoints$ejam_uniq_id <- seq.int(length.out = NROW(frompoints))
  }



















  #
  #
  #

  #
  #
  #
  #

  ############################ #
  ##     This would maybe inefficiently make a copy but otherwise would do same as a few lines below:
  # frompoints <- create_quaddata(frompoints, idcolname = NULL, xyzcolnames = c("FAC_X", "FAC_Y", "FAC_Z"))
  # frompoints[ , pointid := NULL] # because create_quaddata() created that but already have it ejam_uniq_id

  # compute and add grid info ####
  earthRadius_miles <- 3959 # in case it is not already in global envt
  radians_per_degree <- pi / 180

  frompoints <- data.table::copy(frompoints) # make a copy to avoid next line altering frompoints in the calling envt by reference bc of how data.table works
  frompoints[ , lat_RAD := lat * radians_per_degree]
  frompoints[ , lon_RAD := lon * radians_per_degree]
  frompoints[ ,  earthRadius_miles_cos_lat := earthRadius_miles * cos(lat_RAD)] # or maybe # frompoints[ , cos_lat := cos(lat_RAD)]
  frompoints[ , FAC_X := earthRadius_miles_cos_lat     * cos(lon_RAD)]
  frompoints[ , FAC_Y := earthRadius_miles_cos_lat     * sin(lon_RAD)]
  frompoints[ , FAC_Z := earthRadius_miles             * sin(lat_RAD)]
  frompoints[, earthRadius_miles_cos_lat := NULL]
  frompoints[, lat_RAD := NULL]; frompoints[, lon_RAD := NULL]
  truedistance <- distance_via_surfacedistance(radius)   # simply 7918*sin(radius/7918) which is nearly identical to unadjusted distances, like 9.999997 vs. 10.000000 miles ! even 29.999928 vs 30 miles

  # done outside loop now - takes more memory but probably faster
  frompoints[ , `:=`(
    x_low = FAC_X - truedistance,
    x_hi  = FAC_X + truedistance,
    z_low = FAC_Z - truedistance,
    z_hi  = FAC_Z + truedistance
  )]
  #---- Get ready for loop here ----

  # allocate memory for result list
  nRowsDf <- NROW(frompoints)
  res <- vector('list', nRowsDf)  # list of data.tables   cols will be   distance, ejam_uniq_id

  if (!quiet) {
    cat("Finding points within ", radius," miles of the site (point), for each of", nRowsDf," sites (points)...\n")
  }

  ######################################################################################################################## #
  ######################################################################################################################## #
  #
  ### # LOOP OVER frompoints SITES HERE - can some be done faster via data.table ?? ***  ----

  for (i in 1:nRowsDf) {

    #### USE quadtree INDEX OF points TO FIND points NEAR THIS 1 SITE, a
    # find vector of the hundreds of point ids that are approximately near this site

    vec <- SearchTrees::rectLookup(
      tree = quadtree,
      ptOne = frompoints[i, c(x_low, z_low)],
      ptTwo = frompoints[i, c(x_hi,  z_hi)]
    )







    tmp <-  quaddatatable[vec, ]  # all the indexed topoints near this 1 site.

    ########################################################################### ## ** SLOWSTEP TO OPTIMIZE:
    # pdist() can Compute a distance matrix between two matrices of observations,
    # but here is used in loop to only check all pts near ONE frompoint at a time. Seems inefficient - cant we vectorize outside loop in batches that are manageable size each? ***
    distances <- as.matrix(pdist::pdist(
      tmp[ , .(BLOCK_X, BLOCK_Y, BLOCK_Z)],           #######   should change from BLOCK to generic points ***
      frompoints[i, c('FAC_X','FAC_Y','FAC_Z')])
    )
    # distances is now just a 1 column data.table of hundreds of distance values. Some may be 5.08 miles even though specified radius of 3 miles even though distance to corner of bounding box should be 1.4142*r= 4.2426, not 5 ?
    # pdist computes a n by p distance matrix using two separate matrices

    # add the distances and id to the table of nearby

    tmp[ , distance := distances[ , 1]]      # converts distances dt into a vector that becomes a column of tmp
    tmp[, ejam_uniq_id := frompoints[i, .(ejam_uniq_id)]]
    #### LIMIT RESULTS SO FAR TO THE RADIUS REQUESTED later? takes more memory to save the ones > radius while looping, but then filter only once at end of loop
    #filter actual distance, exclude pts that are roughly nearby (according to index and bounding boxes) but are just beyond the radius you specified
    # e.g., 805 pts roughly nearby, but only 457 truly within radius.

    res[[i]] <- tmp[ ,  .(pointid, distance, ejam_uniq_id)]

    #   *** SLOW STEP TO OPTIMIZE  #  1 OF SLOWEST LINES *** - cant we do this outside the loop just once?
    # *** but if moved out of loop then if using avoidorphans below (not used for normal ejam calc), it will not have removed where d > true and thus failed to create some orphans?
    # }

    # tmp
    #           _X       _Z        _Y pointid  distance ejam_uniq_id
    # 1: -198.8586 1985.476 -3419.360 3041513 0.4734989      1
    # 2: -199.8715 1984.961 -3419.600 3041514 0.6885808      1

    ################################# #
    #### If avoidorphans TRUE, and no topoint within radius of site, look past radius to maxradius   ############## #
    # But note looking past radius is NOT how EJScreen works, for buffer reports - it just fails to provide any result if no blockpoint is inside circle.
    # (For proximity scores, which are different than circular buffer reports, EJScreen does look beyond radius, but not for circular zone report).
    # Also, you would rarely get here even if avoidorphans set TRUE.
    # cat('about to check avoidorphans\n')
    # if ( 1 == 0 ) {
    if ( avoidorphans && (NROW(res[[i]])  == 0)) {
      if (!quiet) {cat("avoidorphans is TRUE, so avoiding reporting zero blocks nearby at site ", i, " by searching past radius of ", radius, " to maxradius of ", maxradius, "\n")}
      #search neighbors, allow for multiple at equal distance

      stop("next step crashes R -- work in progress")

      vec  <- SearchTrees::knnLookup(
        quadtree,
        unlist(c(frompoints[ , 'FAC_X'])),  # ??? untested - avoidorphans=T needs fixing ***
        unlist(c(frompoints[ , 'FAC_Z'])),  # ???
        k = 10   # why 10?
      )

      tmp <-  quaddata[vec[1, ], ]  # the first distance in the new vector of distances? is that the shortest?

      x <- tmp[, .(BLOCK_X, BLOCK_Y, BLOCK_Z)]    #######   should change from BLOCK to generic points ***
      y <- frompoints[i, .(FAC_X, FAC_Y, FAC_Z)]
      distances <- as.matrix(pdist::pdist(x, y))

      tmp[ , distance := distances[ , c(1)]]
      #
      tmp[ , ejam_uniq_id := frompoints[i, .(ejam_uniq_id)]]
      # keep only the 1 pt that is closest to this site (that is > radius but < maxradius) -- NEED TO CONFIRM/TEST THIS !!
      truemaxdistance <- distance_via_surfacedistance(maxradius)
      data.table::setorder(tmp, distance) # ascending order short to long distance
      #
      res[[i]] <- tmp[distance <= truemaxdistance, .(pointid, distance, ejam_uniq_id)]
      # res[[i]] <- tmp[ , .(pointid, distance, ejam_uniq_id)]
    }
    ### end of if avoidorphans
    ################################# #

    if (((i %% report_progress_every_n) == 0) & interactive()) {cat(paste("Finished finding points near ", i ," of ", nRowsDf),"\n" ) }   # i %% report_progress_every_n indicates i mod report_progress_every_n (“i modulo report_progress_every_n”)

    ## update progress bar at 5% intervals
    pct_inc <- 5
    ## add check that data has enough points to show increments with rounding
    ## i.e. if 5% increments, need at least 20 points or %% will return NaN
    if (is.function(updateProgress) & (nRowsDf >= (100/pct_inc)) & (i %% round(nRowsDf/(100/pct_inc)) < 1)) {
      boldtext <- paste0((pct_inc)*round((100/pct_inc*i/nRowsDf)), '% done')
      updateProgress(message_main = boldtext,
                     value = round((pct_inc)*i/nRowsDf,2)/(pct_inc))
    }

  } # do next site in loop, etc., until end of this loop.
  # end loop over sites ################################################################################################ #
  ###################################################################################################################### #
  ###################################################################################################################### #
  #
  sites2points <- data.table::rbindlist(res)

  data.table::setkey(sites2points, pointid, ejam_uniq_id, distance)

  # if (!quiet) {
  #   cat('Stats via getblocks_diagnostics(), but BEFORE capping at d <= truedistance: \n')
  #   cat("min distance before cap: ", min(sites2points$distance, na.rm = TRUE), "\n")
  #   cat("max distance before cap: ", max(sites2points$distance, na.rm = TRUE), "\n\n")
  #   getblocks_diagnostics(sites2points) # returns NA if no blocks nearby
  #   cat("\n\nCapping so distance <= truedistance now...\n ")
  # }
  sites2points <- sites2points[distance <= truedistance, ] # had been inside the loop.  ***can it be done later, just once, after adjusting distances?






  ########################################################################### ##
  # if (!quiet) {
  #   cat('Stats via getblocks_diagnostics(), but BEFORE ADJUSTING UP FOR VERY SHORT DISTANCES: \n')
  #   cat("min distance before adjustment: ", min(sites2points$distance, na.rm = TRUE), "\n")
  #   cat("max distance before adjustment: ", max(sites2points$distance, na.rm = TRUE), "\n\n")
  #   getblocks_diagnostics(sites2points) # returns NA if no blocks nearby
  #   cat("\n\nAdjusting upwards the very short distances now...\n ")
  # }

  # ADJUST THE VERY SHORT DISTANCES but lack size of unit with people in it  ####

  # distance  adjusted to be the minimum possible value, min_distance

  # used block_radius_miles here, to correct the distances that are small relative to a block size.



  # This also avoids infinitely small or zero distances.

  if (retain_unadjusted_distance) {
    sites2points[ , distance_unadjusted := distance] # wastes space but for development/ debugging probably useful

    ##   join that updated sites2points is irrelevant for non-blocks

    setcolorder(sites2points, c("ejam_uniq_id", "pointid", "distance", "distance_unadjusted"))


  } else {


    setcolorder(sites2points, c("ejam_uniq_id", "pointid", "distance"))


  }



  sites2points[distance < min_distance, distance := min_distance]  # assumes distance is in miles






  # if (!quiet) {
  #   cat('Stats via getblocks_diagnostics(), AFTER ADJUSTING up FOR SHORT DISTANCES: \n')
  #   cat("min distance AFTER adjustment: ", min(sites2points$distance, na.rm = TRUE), "\n")
  #   cat("max distance AFTER adjustment: ", max(sites2points$distance, na.rm = TRUE), "\n\n")
  #   # getblocks_diagnostics(sites2points)
  #   cat("\n")
  # }
  ########################################################################### ##

  # a final filter not needed because the adjustment here can only shrink the distance


  # sites2points <- sites2points[distance <= truedistance, ]











  return(sites2points)
}
