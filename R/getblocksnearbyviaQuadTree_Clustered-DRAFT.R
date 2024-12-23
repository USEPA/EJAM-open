
#' DRAFT / WAS WORK IN PROGRESS  - find nearby blocks using Quad Tree data structure for speed, CLUSTERED FOR PARALLEL PROCESSING
#'
#' @description Uses packages parallel and snow. [parallel::makePSOCKcluster] is an enhanced version of [snow::makeSOCKcluster] in package snow.
#'     It runs Rscript on the specified host(s) to set up a worker process which listens on a socket for expressions to evaluate, and returns the results (as serialized objects).
#'
#' @details  For all examples, see [getblocksnearbyviaQuadTree()]
#'
#'  Uses indexgridsize and quaddata  variables that come from global environment (but should pass to this function rather than assume in global env?)
#'
#' @param sitepoints data.table with columns LAT, LONG
#' @param radius in miles
#' @param maxradius miles distance
#' @param avoidorphans logical
#' @param CountCPU for parallel processing via makeCluster() and [doSNOW::registerDoSNOW()]
#' @param quadtree index of all US blocks like localtree
#' @seealso   [getblocksnearby()] [getblocksnearbyviaQuadTree()]
#'
#' @keywords internal
#'
getblocksnearbyviaQuadTree_Clustered <- function(sitepoints,radius, maxradius, avoidorphans, CountCPU = 1, quadtree) {

  #pass in a list of uniques and the surface radius distance
  #filter na values
sitepoints <- sitepoints[!is.na(sitepoints$LAT) & !is.na(sitepoints$LONG), ]
  #compute and add grid info
  earthRadius_miles <- 3959 # in case it is not already in global envt
  sitepoints[,"LAT_RAD"] <- sitepoints$LAT * pi / 180
  sitepoints[,"LONG_RAD"] <- sitepoints$LONG * pi / 180
  sitepoints[,"FAC_X"] <- earthRadius_miles * cos(sitepoints$LAT_RAD) * cos(sitepoints$LONG_RAD)
  sitepoints[,"FAC_Y"] <- earthRadius_miles * cos(sitepoints$LAT_RAD) * sin(sitepoints$LONG_RAD)
  sitepoints[,"FAC_Z"] <- earthRadius_miles * sin(sitepoints$LAT_RAD)

  #now we need to buffer around the grid cell by the actual radius distance
  # buffer_indexdistance <- ceiling(radius/indexgridsize) # this will be one or larger ... but where is this ever used??  indexgridsize was defined in initialization as say 10 miles

  # allocate result list
  nRowsDf <- nrow(sitepoints)
  res <- vector('list', nRowsDf)

  truedistance <- distance_via_surfacedistance(radius)   # simply 7918*sin(radius/7918)

  #set up cluster, splitting up the sitepoints among the available CPUs
  #   but see this on why detectCores() is a bad idea:  https://www.r-bloggers.com/2022/12/please-avoid-detectcores-in-your-r-packages/
  cpuids <- 1:CountCPU
  sitepoints[,"CPUAFFINITY"] <- rep_len(cpuids, length.out = nrow(sitepoints))
  percpusitepoints <- vector('list', CountCPU)
  for (i in 1:CountCPU)  ## for each CPU
  {
    percpusitepoints[[i]] <- subset(sitepoints, CPUAFFINITY == i)
  }

  # parallel::makePSOCKcluster is an enhanced version of snow::makeSOCKcluster in package snow. It runs Rscript on the specified host(s) to set up a worker process which listens on a socket for expressions to evaluate, and returns the results (as serialized objects).
  cl <- parallel::makeCluster(CountCPU, outfile =  "")
  doSNOW::registerDoSNOW(cl)
  #foreach::registerDoSEQ()

  # main reason for using foreach::foreach() is that it supports parallel execution,
  # that is, it can execute those repeated operations on multiple processors/cores on your computer (and there are other advantages as well)
  cpuIndex <- 1 ; FAC_X <- 0; FAC_Z <- 0 # this just stops the warning about undefined variable since IDE does not understand it being defined in foreach()
  #### LOOP OVER THE CPUs ##############################################################################################
  parref <- foreach::foreach(cpuIndex = 1:CountCPU, .export = c("distance_via_surfacedistance","earthRadius_miles","crd","quaddata"), .packages = c("SearchTrees","data.table","pdist"), .errorhandling = 'pass', .verbose = TRUE) %dopar% {

    # print(.packages())
    #2 seconds overhead to create the quad tree

    sitepoints2use <- percpusitepoints[[cpuIndex]]

    # allocate result list
    subnRowsDf <- nrow(sitepoints2use)
    partialres <- vector('list', subnRowsDf)

    #### LOOP OVER THE sitepoints STARTS HERE, within loop over CPUs ##################################################################

    for (i in 1:subnRowsDf) { ## for each row






      coords <- sitepoints2use[i, .(FAC_X,FAC_Z)]
      x_low <- coords[,FAC_X] - truedistance;
      x_hi  <-  coords[,FAC_X] + truedistance
      z_low <- coords[,FAC_Z] - truedistance;
      z_hi  <-  coords[,FAC_Z] + truedistance

      # if ((i %% 100)==0) {print(paste("Cells currently processing: ",i," of ",nRowsDf) ) }
      print("did we even do anything?")
      tryCatch(
        expr = {
          print('trying')
           # vec <- SearchTrees::rectLookup(quadtree, c(x_low, z_low), c(x_hi, z_hi))
        },
        error = function(e) {
          print("yay")
          print(e)
          # (Optional)
          # Do this if an error is caught...
        },
        warning = function(w) {
          print("don't understand")
          print(w)
          # (Optional)
          # Do this if an warning is caught...
        },
        finally = {
          print('WTF')
          # (Optional)
          # Do this at the end before quitting the tryCatch structure...
        }
      )




      # tmp <- quaddata[vec,]
      # x <- tmp[, .(BLOCK_X,BLOCK_Y,BLOCK_Z)]
      # y <- sitepoints2use[i, .(FAC_X,FAC_Y,FAC_Z)]
      # distances <- as.matrix(pdist(x,y))

      # #clean up fields
      # tmp[,distance := distances[,c(1)]]
      # tmp[,ID := sitepoints2use[i, .(ID)]]

      # #filter actual distance
      # tmp <- tmp[distance <= truedistance, .(blockid, distance, ID)]

      #hold your horses, what if there are no blocks and you are supposed to avoid that
      # && (nrow(tmp))==0
      if ( avoidorphans) {
        #search neighbors, allow for multiple at equal distance
        print("inbefore knn")
        # This should have been done in server.R
        # quadtree <- SearchTrees::createTree( quaddata, treeType = "quad", dataType = "point")
        vec <- SearchTrees::knnLookup(quadtree, c(coords[,FAC_X]), c(coords[,FAC_Z]), k = 10)
        print("did we knn? ")
        tmp <- quaddata[vec[1,],]

        x <- tmp[, .(BLOCK_X,BLOCK_Y,BLOCK_Z)]
        y <- sitepoints2use[i, .(FAC_X,FAC_Y,FAC_Z)]
        distances <- as.matrix(pdist(x,y))

        #clean up fields
        tmp[,distance := distances[,c(1)]]
        tmp[,ID := sitepoints2use[i, .(ID)]]

        #filter to max distance
        truemaxdistance <- distance_via_surfacedistance(maxradius)
        tmp <- tmp[distance <= truemaxdistance, .(blockid, distance,ID)]
        partialres[[i]] <- tmp
      } else {
        partialres[[i]] <- tmp
      }
    }
    partial <- do.call('rbind', partialres)
    return(partial)
  }

  bound <- do.call('rbind', parref)

  print(paste("Total Rowcount: ",nrow(bound)) )

  print(paste("Final Rowcount: ",nrow(bound)) )
  # is this from parallel or snow package?
  parallel::stopCluster(cl)
  return(bound)
}
