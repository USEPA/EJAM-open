
#' DRAFT / WAS WORK IN PROGRESS  Key buffering function - wrapper redirecting to the right version of getblocksnearby()
#'
#' @details  For all examples, see [ejamit()] 
#' 
#' Like getblocksnearby() but tries to handle localtree and quadtree parameter differently 
#'   - not sure how to check if they are in the right environment.
#'  
#' @param sitepoints   see [getblocksnearbyviaQuadTree()] or other such functions
#' @param radius       see [getblocksnearbyviaQuadTree()] or other such functions
#' @param maxradius    see [getblocksnearbyviaQuadTree()] or other such functions
#' @param avoidorphans see [getblocksnearbyviaQuadTree()] or other such functions
#' @param quadtree a large quadtree object created from the SearchTree package example:
#'    SearchTrees::createTree( quaddata, treeType = "quad", dataType = "point")
#' @param ...          see [getblocksnearbyviaQuadTree_Clustered()] or other such functions
#' @seealso [getblocksnearby()] 
#' 
#' @keywords internal
#'
getblocksnearby2 <- function(sitepoints, radius=3, maxradius=31.07, 
                             avoidorphans=FALSE, 
                             # indexgridsize,
                             quadtree=is.null,
                             ...
) {
  
  # TRYING THIS TO SEE IF localtree can be checked in global environment or calling environment or whatever
  # so this function could be used without specifying that index
  # and the index quadtree or localtree could be built using .onAttach() or .onLoad() when package is first attached or just loaded. 
  # not sure that would work on a server, 
  # and not sure it would work for an R user in RStudio who did library to load the EJAM pkg
  
  # wrapper to make it simple to (later?) switch between functions to use for this, clustered vs not, etc.
  if (is.null(quadtree)) {
    if (exists('localtree' )) { # not working yet?
      return(
        getblocksnearbyviaQuadTree(sitepoints = sitepoints, radius = radius, maxradius = maxradius, 
                                   avoidorphans = avoidorphans, 
                                   # indexgridsize = indexgridsize,
                                   quadtree = localtree,
                                   ...)
      )
    } else {
      if (shiny::isRunning()) {
        warning('requires quadtree argument')
        return(NULL)
      } else {
        stop('requires quadtree argument')
        
      }
    }
  }
  
  getblocksnearbyviaQuadTree(sitepoints = sitepoints, radius = radius, maxradius = maxradius, 
                             avoidorphans = avoidorphans, 
                             # indexgridsize=indexgridsize,
                             quadtree = quadtree,
                             ...)
  
  # getblocksnearbyviaQuadTree_Clustered(sitepoints=sitepoints, radius=radius, maxradius=maxradius, 
  #                                        avoidorphans=avoidorphans, 
  #                                      # indexgridsize=indexgridsize,
  #                                      quadtree=quadtree,
  #                                      ...)
  
  # getblocksnearbyviaQuadTree2(sitepoints=sitepoints, radius=radius, maxradius=maxradius, 
  #                               avoidorphans=avoidorphans, 
  #                             # indexgridsize=indexgridsize,
  #                             quadtree=quadtree,
  #                             ...)
  
}
