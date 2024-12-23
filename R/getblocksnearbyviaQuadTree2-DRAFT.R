
#' DRAFT / WAS WORK IN PROGRESS - Find nearby blocks using Quad Tree data structure for speed, NO PARALLEL PROCESSING - DRAFT / WORK IN PROGRESS
#'
#' @description Given a set of points and a specified radius in miles,
#'   this function quickly finds all the US Census blocks near each point.
#' @details  This should be almost identical to getblocksnearbyviaQuadTree(), but it uses f2, a copy of sitepoints, and more importantly pulls some code out of the for loop and uses a vectorized approach.
#'   For each point, it uses the specified search radius and finds the distance to
#'   every block within the circle defined by the radius.
#'   Each block is defined by its Census-provided internal point, by latitude and longitude.
#'
#'   Results are the sites2blocks table that would be used by doaggregate(),
#'   with distance in miles as one output column of data.table.
#'   Adjusts distance to avg resident in block when it is very small relative to block size,
#'   the same way EJScreen adjusts distances in creating proximity scores.
#'
#'   Each point can be the location of a regulated facility or other type of site, and
#'   the blocks are a high-resolution source of information about where
#'   residents live.
#'
#'   Finding which blocks have their internal points in a circle provides
#'   a way to quickly estimate what fraction of a block group is
#'   inside the circular buffer more accurately and more quickly than
#'   areal apportionment of block groups would provide.
#'
#' @param sitepoints data.table with columns ejam_uniq_id, lat, lon giving point locations of sites or facilities around which are circular buffers
#' @param radius in miles, defining circular buffer around a site point
#' @param maxradius miles distance (max distance to check if not even 1 block point is within radius)
#' @param avoidorphans logical If TRUE, then where not even 1 BLOCK internal point is within radius of a SITE,
#'   it keeps looking past radius, up to maxradius, to find nearest 1 BLOCK.
#'   What EJScreen does in that case is report NA, right? So,
#'   does EJAM really need to report stats on residents presumed to be within radius,
#'    if no block centroid is within radius?
#'   Best estimate might be to report indicators from nearest block centroid which is
#'   probably almost always the one your site is sitting inside of,
#'   but ideally would adjust total count to be a fraction of blockwt based on
#'   what is area of circular buffer as fraction of area of block it is apparently inside of.
#'   Setting this to TRUE can produce unexpected results, which will not match EJScreen numbers.
#'
#'   Note that if creating a proximity score, by contrast, you instead want to find nearest 1 SITE if none within radius of this BLOCK.
#' @param quadtree (a pointer to the large quadtree object)
#'    created using indexblocks() which uses the SearchTree package.
#'    Takes about 2-5 seconds to create this each time it is needed.
#'    It can be automatically created when the package is attached via the .onAttach() function
#' @param report_progress_every_n Reports progress to console after every n points,
#'   mostly for testing, but a progress bar feature might be useful unless this is super fast.
#' @param quiet Optional. set to TRUE to avoid message about using [getblocks_diagnostics()],
#'   which is relevant only if a user saved the output of this function.
#' @examples
#'   # indexblocks() # if localtree not available yet, quadtree = localtree
#'   x = getblocksnearby2(testpoints_1000, radius = 3)
#' @seealso [ejamit()] [getblocksnearby()]
#' @import data.table
#' @importFrom pdist "pdist"
#'
#' @noRd
#'
getblocksnearbyviaQuadTree2 <- function(sitepoints, radius = 3, maxradius = 31.07, avoidorphans = FALSE,
                                        report_progress_every_n = 500, quiet = FALSE,
                                        quadtree) {

  # indexgridsize was defined at start as say 10 miles in global? could be passed here as a parameter ####
  # and buffer_indexdistance defined here in code but is never used anywhere...
  # buffer_indexdistance <- ceiling(radius / indexgridsize)

  if (class(quadtree) != "QuadTree") {
    if (shiny::isRunning()) {
      warning('quadtree must be an object created with indexblocks(), from SearchTrees package with treeType = "quad" and dataType = "point"')
      return(NULL)
    } else {
      stop('quadtree must be an object created with indexblocks(), from SearchTrees package with treeType = "quad" and dataType = "point"')
    }  }
  if (!data.table::is.data.table(sitepoints)) {data.table::setDT(sitepoints)} # should we set a key or index here, like ? ***

  if (!('ejam_uniq_id' %in% names(sitepoints))) {sitepoints$ejam_uniq_id <- seq.int(length.out = NROW(sitepoints))}

  # pass in a list of uniques and the surface radius distance

  # filter na values? or keep length of out same as input? ####
  # sitepoints <- sitepoints[!is.na(sitepoints$lat) & !is.na(sitepoints$lon), ] # perhaps could do this by reference to avoid making a copy

  # compute and add grid info ####
  earthRadius_miles <- 3959 # in case it is not already in global envt
  radians_per_degree <- pi / 180

  f2   <- data.table::copy(sitepoints) # make a copy to avoid altering sitepoints in the calling envt when modifying by reference using data.table
  f2[         , lat_RAD := lat * radians_per_degree]   # data.table modifies it by reference
  f2[         , lon_RAD := lon * radians_per_degree]
  cos_lat <- cos(         f2[, lat_RAD])    # or maybe # sitepoints[ , cos_lat := cos(lat_RAD)]
  f2[         , FAC_X := earthRadius_miles * cos_lat * cos(lon_RAD)]
  f2[         , FAC_Y := earthRadius_miles * cos_lat * sin(lon_RAD)]
  f2[         , FAC_Z := earthRadius_miles *           sin(lat_RAD)]

  truedistance <- distance_via_surfacedistance(radius)   # simply 7918*sin(radius/7918) which is nearly identical to unadjusted distances, like 9.999997 vs. 10.000000 miles ! even 29.999928 vs 30 miles

  # **** these are now outside the loop, so they can be used in the vectorized way
  f2[ , `:=`(
    x_low = FAC_X - truedistance,
    x_hi  = FAC_X + truedistance,
    z_low = FAC_Z - truedistance,
    z_hi  = FAC_Z + truedistance
  )]

  #---- Get ready for loop here ----

  # allocate memory for result list
  nRowsDf <- NROW(f2)
  res <- vector('list', nRowsDf)  # list of data.tables   cols will be blockid, distance, ejam_uniq_id

  if (!quiet) {
    cat("Finding Census blocks with internal point within ", radius," miles of the site (point), for each of", nRowsDf," sites (points)...\n")
  }

  ######################################################################################################################## #
  ######################################################################################################################## #
  #
  ### # LOOP OVER SITES HERE

  for (i in 1:nRowsDf) {




    # could drop extra fields in f2 and only keep    #   .(ejam_uniq_id, FAC_X, FAC_Y, FAC_Z, x_low, x_hi, z_low, z_hi)




    #### USE quadtree INDEX OF USA BLOCKS TO FIND BLOCKS NEAR THIS 1 SITE, and store blockids as vector in vec, and .(BLOCK_X , BLOCK_Z  , BLOCK_Y, blockid)  as "tmp"
    # find vector of the hundreds of block ids that are approximately near this site? (based on bounding box?)
    vec <- SearchTrees::rectLookup(
      tree = quadtree,
      ptOne = unlist( f2[i, .(x_low, z_low)] ),  # unlist(c(x_low[i, ], z_low[i, ])),
      ptTwo = unlist( f2[i, .(x_hi,  z_hi )] )   # unlist(c(x_hi[ i, ], z_hi[ i, ])))
    )

    # mapfast(blockpoints[blockid %in% vec, .(lat,lon)], radius = 0.01)














    tmp <-  quaddata[vec, ]   # looks up xyz locations of the few hundred nearby blocks via blockid that is stored in vec. tmp now has cols  BLOCK_X,  BLOCK_Z ,  BLOCK_Y, blockid

    ########################################################################### ## ** SLOWSTEP TO OPTIMIZE:

    distances <-           pdist::pdist(
      tmp[ , .(BLOCK_X, BLOCK_Y, BLOCK_Z)],
      f2[i, c('FAC_X','FAC_Y','FAC_Z')]
    )@dist
    # distances <- as.matrix(pdist::pdist(   # is as.matrix any diff in speed than just  @dist ?
    #   tmp[ , .(BLOCK_X, BLOCK_Y, BLOCK_Z)],
    #   f2[i, c('FAC_X','FAC_Y','FAC_Z')]
    #   ))

    # distances is now just a 1 column data.table of hundreds of distance values. Some may be 5.08 miles even though specified radius of 3 miles even though distance to corner of bounding box should be 1.4142*r= 4.2426, not 5 ?
    # pdist computes a n by p distance matrix using two separate matrices

    # add the distances and ejam_uniq_id to the table of nearby blocks
    tmp[ , distance := as.vector(distances)]    # converts distances into a vector that becomes a column of tmp
    tmp[ , ejam_uniq_id :=         f2[i, .(ejam_uniq_id)]]  # the similar clustered function differs, why?

    #### LIMIT RESULTS SO FAR TO THE RADIUS REQUESTED

    #filter actual distance, exclude blocks that are roughly nearby (according to index and bounding boxes) but are just beyond the radius you specified
    # e.g., 805 blocks roughly nearby, but only 457 truly within radius.

    res[[i]]     <- tmp[distance <= truedistance, .(blockid, distance, ejam_uniq_id)]  # ** SLOW STEP TO OPTIMIZE - how do you drop rows that meet some condition, just by reference??



    # tmp
    #      BLOCK_X  BLOCK_Z   BLOCK_Y blockid  distance ejam_uniq_id
    # 1: -198.8586 1985.476 -3419.360 3041513 0.4734989      1
    # 2: -199.8715 1984.961 -3419.600 3041514 0.6885808      1

    ################################# #
    #
    #### If avoidorphans TRUE, and no blockpt within radius of site, look past radius to maxradius   ############## #
    #
    # But note looking past radius is NOT how EJScreen works, for buffer reports - it just fails to provide any result if no blockpoint is inside circle. (For proximity scores, which are different than circular buffer reports, EJScreen does look beyond radius, but not for circular zone report). Also, you would rarely get here even if avoidorphans set TRUE.

    if ( avoidorphans && (nrow(res[[i]]))      == 0) {
      if (!quiet) {cat("avoidorphans is TRUE, so avoiding reporting zero blocks nearby at site ", i, " by searching past radius of ", radius, " to maxradius of ", maxradius, "\n")}
      #search neighbors, allow for multiple at equal distance

      vec  <- SearchTrees::knnLookup(
        quadtree,
        unlist(c(f2[i, 'FAC_X'])),
        unlist(c(f2[i, 'FAC_Z'])),
        k = 10  # why 10?
      )

      tmp <-  quaddata[vec[1, ], ]  # the first distance in the new vector of distances? is that the shortest?

      x <- tmp[, .(BLOCK_X, BLOCK_Y, BLOCK_Z)]
      y <-         f2[i, .(FAC_X, FAC_Y, FAC_Z)]
      distances <- as.matrix(pdist::pdist(x, y))

      tmp[ , distance := distances[ , c(1)]]
      tmp[ , ejam_uniq_id :=         f2[i, .(ejam_uniq_id)]]
      # keep only the 1 block that is closest to this site (that is > radius but < maxradius) -- NEED TO CONFIRM/TEST THIS !!
      truemaxdistance <- distance_via_surfacedistance(maxradius)
      data.table::setorder(tmp, distance) # ascending order short to long distance
      res[[i]] <- tmp[distance <= truemaxdistance, .(blockid, distance, ejam_uniq_id)]
      # saving results as a list of tables to rbind after loop; old code did rbind for each table, inside loop
    }
    ### end of if avoidorphans
    ################################# #

    if ((i %% report_progress_every_n) == 0 & interactive()) {print(paste("Cells currently processing: ",i ," of ", nRowsDf) ) } # i %% report_progress_every_n indicates i mod report_progress_every_n (“i modulo report_progress_every_n”)

  } # do next site in loop, etc., until end of this loop.
  # end loop over sites ################################################################################################ #
  ###################################################################################################################### #
  ###################################################################################################################### #

  sites2blocks <- data.table::rbindlist(res)
  data.table::setkey(sites2blocks, blockid, ejam_uniq_id, distance)

  ########################################################################### ##
  if (!quiet) {
    cat('stats BEFORE ADJUSTING FOR SHORT DISTANCES! \n')
    cat('min distance before adjustment: ', min(sites2blocks$distance, "\n"))
    cat('max distance before adjustment: ', max(sites2blocks$distance, "\n"))
    getblocks_diagnostics(sites2blocks)
    cat("Adjusting for very short distances now.\n ")
  }
  # ADJUST THE VERY SHORT DISTANCES ####

  # distance gets adjusted to be the minimum possible value,  0.9 * effective radius of block_radius_miles (see EJScreen Technical Documentation discussion of proximity analysis for rationale)

  if (!("block_radius_miles" %in% names(blockwts))) {
    # if missing because not added to dataset yet then use placeholder of 100 / meters_per_mile, or 100 meters
    # not sure if this updates by reference blockwts for the remainder of this session and all users, or if this happens each time getblocksnearby... is called.
    message("using temporary approximation of block_radius_miles")
    blockwts[ , block_radius_miles := block_radius_miles_round_temp] # lazy load this and add it into blockwts
  }
  # Add block_radius_miles here, now to be able to correct the distances that are small relative to a block size.
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
  ########################################################################### ##


  ### and with above idea, cant we subset to keep only distance <=  radius here, instead of inside the loop ? Or do it even later, after adjusting short distances? What would make sense to report as distance to avg resident if the effective radius happends to be > radius specified, as with small radius circle in rural huge block?
  # sites2blocks <- sites2blocks[distance <= truedistance, ]



  if (interactive() & !quiet) {
    cat("You can use  getblocks_diagnostics(sites2blocks)  to see this info on distances found:\n\n")
    getblocks_diagnostics(sites2blocks)
  }
  return(sites2blocks)
}
