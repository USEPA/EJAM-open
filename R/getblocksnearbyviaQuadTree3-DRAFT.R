
#' DRAFT / WAS WORK IN PROGRESS  Find nearby blocks using Quad Tree data structure for speed, NO PARALLEL PROCESSING
#'
#' @description Given a set of points and a specified radius (in miles),
#'   this function quickly finds all the US Census blocks near each point.
#'   For each point, it uses the specified search radius and finds the distance to
#'   every block within the circle defined by the radius.
#'   Each block is defined by its Census-provided internal point, by latitude and longitude.
#'
#'
#'
#'
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
#' @param radius in miles, defining circular buffer around site point
#' @param maxradius miles distance (max distance to check if not even 1 block point is within radius)
#' @param avoidorphans logical Whether to avoid case where no block points are within radius,
#'   so if TRUE, it keeps looking past radius to find nearest one within maxradius.
#' @param quadtree (a pointer to the large quadtree object)
#'    created from the SearchTree package example:
#'    SearchTrees::createTree( quaddata, treeType = "quad", dataType = "point")
#'    Takes about 2-5 seconds to create this each time it is needed.
#'    It is automatically created when the package is attached via the .onAttach() function
#' @param report_progress_every_n Reports progress to console after every n points,
#'   mostly for testing, but a progress bar feature might be useful unless this is super fast.
#'
#' @seealso [ejamit()] [getblocksnearby()] [getblocksnearbyviaQuadTree()]
#' @import data.table
#' @importFrom pdist "pdist"
#'
#' @noRd
#'
getblocksnearbyviaQuadTree3 <- function(sitepoints, radius=3, maxradius=31.07,
                                        avoidorphans=TRUE, report_progress_every_n=500,
                                        quadtree) {

  if (class(quadtree) != "QuadTree") {
    if (shiny::isRunning()) {
      warning('quadtree must be an object created with indexblocks(), from SearchTrees package with treeType = "quad" and dataType = "point"')
      return(NULL)
    } else {
      stop('quadtree must be an object created with indexblocks(), from SearchTrees package with treeType = "quad" and dataType = "point"')
    }
  }
  if (!data.table::is.data.table(sitepoints)) {data.table::setDT(sitepoints)}

  if (!('ejam_uniq_id' %in% names(sitepoints))) {sitepoints$ejam_uniq_id <- seq.int(length.out = NROW(sitepoints))}

  #pass in a list of uniques and the surface radius distance

  #filter na values? or keep length of out same as input? ####
  # sitepoints <- sitepoints[!is.na(sitepoints$lat) & !is.na(sitepoints$lon), ] # perhaps could do this by reference to avoid making a copy

  #compute and add grid info ####
  earthRadius_miles <- 3959 # in case it is not already in global envt
  radians_per_degree <- pi / 180

  # f2 <- data.table::copy(sitepoints) # make a copy?
  sitepoints[ , lat_RAD := lat * radians_per_degree]   # PROBLEM? - this warns but makes a shallow copy to avoid altering sitepoints in the calling envt bc of how data.table works
  sitepoints[ , lon_RAD := lon * radians_per_degree]
  cos_lat <- cos(sitepoints[ , lat_RAD])    # or maybe # sitepoints[ , cos_lat := cos(lat_RAD)]
  sitepoints[ , FAC_X := earthRadius_miles * cos_lat * cos(lon_RAD)]
  sitepoints[ , FAC_Y := earthRadius_miles * cos_lat * sin(lon_RAD)]
  sitepoints[ , FAC_Z := earthRadius_miles *           sin(lat_RAD)]

  # indexgridsize was defined at start as say 10 miles in global? could be passed here as a parameter ####
  # and buffer_indexdistance defined here in code but is never used anywhere...
  # buffer_indexdistance <- ceiling(radius / indexgridsize)
  truedistance <- distance_via_surfacedistance(radius)   # simply 7918*sin(radius/7918)

  # main reason for using foreach::foreach() is that it supports parallel execution,
  # that is, it can execute those repeated operations on multiple processors/cores on your computer
  # (and there are other advantages as well)

  #---- Get ready for loop here ----

  # allocate memory for result list
  nRowsDf <- NROW(sitepoints)
  res <- vector('list', nRowsDf)  # list of data.tables   cols will be blockid, distance, ejam_uniq_id

  #

  sitepoints[ , x_low := FAC_X - truedistance]
  sitepoints[ , x_hi  := FAC_X + truedistance]
  sitepoints[ , z_low := FAC_Z - truedistance]
  sitepoints[ , z_hi  := FAC_Z + truedistance]

  for (i in 1:nRowsDf) {    # LOOP OVER SITES HERE ----
    ########################################################################### ## ** SLOW STEP TO OPTIMIZE   *** ** ** **
    # coords <- sitepoints[i, .(FAC_X, FAC_Z)]  # ** SLOW STEP TO OPTIMIZE  (the similar clustered function uses sitepoints2use not sitepoints)
    # x_low  <- coords[,FAC_X]-truedistance;  #  EXTREMELY SLOW LINE
    # x_hi  <-  coords[,FAC_X]+truedistance
    # z_low  <- coords[,FAC_Z]-truedistance;
    # z_hi  <-  coords[,FAC_Z]+truedistance   # ** THIS HAD BEEN THE SLOWEST LINE  OVERALL *** it was moved down to happen during SearchTrees::rectLookup( )
    # moved these above the loop since vectorized now
    # sitepoints[i, x_low := FAC_X - truedistance]
    # sitepoints[i, x_hi  := FAC_X + truedistance]
    # sitepoints[i, z_low := FAC_Z - truedistance]
    # sitepoints[i, z_hi  := FAC_Z + truedistance]

    if ((i %% report_progress_every_n) == 0) {print(paste("Cells currently processing: ",i ," of ", nRowsDf) ) } # i %% report_progress_every_n indicates i mod report_progress_every_n (“i modulo report_progress_every_n”)

    vec <- SearchTrees::rectLookup(
      quadtree,
      # unlist(c(x_low, z_low  )),
      sitepoints[i , .(x_low, z_low)],
      # unlist(c(x_hi, coords[,FAC_Z]+truedistance))
      unlist(sitepoints[i , .(x_hi, z_hi)])
    ) # x and z things are now vectorized

    # *** FIX/CHECK:
     tmp <-  quaddata[vec, ]
    # x <- tmp[ , .(BLOCK_X, BLOCK_Y, BLOCK_Z)] # but not blockid ??
    # y <- sitepoints[i, c('FAC_X','FAC_Y','FAC_Z')]  # the similar clustered function uses something other than sitepoints here - why?
    ########################################################################### ## ** SLOWSTEP TO OPTIMIZE:
    distances <- as.matrix(pdist::pdist(tmp[ , .(BLOCK_X, BLOCK_Y, BLOCK_Z)] ,
                                        sitepoints[i, c('FAC_X','FAC_Y','FAC_Z')] ))  # ** SLOW STEP TO OPTIMIZE  #  1 OF SLOWEST LINES

    # pdist computes a n by p distance matrix using two separate matrices

    #clean up fields
    tmp[ , distance := distances[ , c(1)]]
    tmp[ , ejam_uniq_id := sitepoints[i, .(ejam_uniq_id)]]  # the similar clustered function differs, why?

    #filter actual distance
    ########################################################################### ## ** SLOW STEP TO OPTIMIZE
    res[[i]] <- tmp[distance <= truedistance, .(blockid, distance, ejam_uniq_id)]  # ** SLOW STEP TO OPTIMIZE  #  1 OF SLOWEST LINES

    # hold your horses, what if there are no blocks and you are supposed to avoid that
    if ( avoidorphans && (nrow(res[[i]])) == 0) { # rarely get here so not critical to optimize
      #search neighbors, allow for multiple at equal distance
      # vec  <- SearchTrees::knnLookup(quadtree, unlist(c( coords[ , 'FAC_X'])), unlist(c(coords[ , 'FAC_Z'])), k=10)
      vec  <- SearchTrees::knnLookup(quadtree, unlist(c( sitepoints[i , 'FAC_X'])), unlist(c(sitepoints[i , 'FAC_Z'])), k = 10)

      # *** FIX/CHECK:
      tmp <-  quaddata[vec[1, ], ]

      x <- tmp[, .(BLOCK_X, BLOCK_Y, BLOCK_Z)]
      y <- sitepoints[i, .(FAC_X, FAC_Y, FAC_Z)]
      distances <- as.matrix(pdist::pdist(x, y))

      #clean up fields
      tmp[ , distance := distances[ , c(1)]]
      tmp[ , ejam_uniq_id := sitepoints[i, .(ejam_uniq_id)]]

      #filter to max distance
      truemaxdistance <- distance_via_surfacedistance(maxradius)
      res[[i]] <- tmp[distance <= truemaxdistance, .(blockid, distance, ejam_uniq_id)]
      # saving results as a list of tables to rbind after loop; old code did rbind for each table, inside loop
    } else {
      #?
    }
    if ((i %% report_progress_every_n) == 0 & interactive()) {cat(paste("Finished finding blocks near ",i ," of ", nRowsDf),"\n" ) } # i %% report_progress_every_n indicates i mod report_progress_every_n (“i modulo report_progress_every_n”)
  }
  result <- data.table::rbindlist(res)

  data.table::setkey(result, blockid, ejam_uniq_id, distance)
  # print(summary_of_blockcount(result))

  return(result)
}
