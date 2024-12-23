
#' DRAFTING A FUNCTION TO COUNT NEARBY SITES OF A CERTAIN TYPE
#'
#' @param sitepoints sitepoints
#' @param frspoints frspoints
#' @param radius radius
#' @param quadtree quadtree
#'
#' @return table
#' 
#' 
#'
getfrsnearby <- function(sitepoints, frspoints, radius = 1, quadtree = NULL) {
  
 stop('not implemented yet - also see proxistat2')
  # INSTEAD OF INDEXING 1.5 MILL FRS POINTS TO ENABLE THIS,
  # WE MIGHT AS WELL USE getblocksnearby() once on all 1.5 mill FRS points, for 1 miles?,
  # and aggregate by block not site, to save those distance pairs (perhaps 1.5m * 1k)
  
  # x = getblocksnearby(testpoints_n(10000, weighting = 'frs'), radius = 1)
  # how many blocks tend to be near a FRS?
  # Some points appear to be in US Island Areas, which may lack some data such as demographic data here
  # min distance before adjustment:  0.0008802615 
  # max distance before adjustment:  0.9999996 
  # 9,930 unique output sites
  # 131 blocks are near the avg site or in avg buffer
  # (based on their block internal point, like a centroid)
  # 1,297,569 blocks including doublecounting in overlaps, 
  # in final row count (block-to-site pairs table)
  # 925,568 actual unique blocks total
  # 703,852 blocks (and their residents) have exactly 1 site nearby 
  # 144,255 blocks (and their residents) have exactly 2 sites nearby 
  # 43,603 blocks (and their residents) have exactly 3 sites nearby 
  # 1.401916 is ratio of blocks including multicounting / actual count of unique blocks
  # 24% of unique blocks could get counted more than once 
  # because those residents are near two or more sites 
  # (assuming they live at the block internal point
  
  
  
  # Count of proposed or listed NPL - also known as superfund - sites 
  # within 5 km
  # (or nearest one beyond 5 km), [UP TO MAX SEARCH DISTANCE OF ????? ]
  # each divided by distance in kilometers
  
  
}

# 
# 
# 
# indexblocks
# function() {
#   cat("Checking for index of Census blocks (localtree)...\n")
#   if (!exists("localtree")) {
#     cat('The index of Census block groups (localtree) has not been created yet...\n')
#     if (!exists("quaddata")) {
#       cat(    "The index of Census block groups (localtree) cannot be created until quaddata is loaded ... Trying dataload_from_pins() before indexblocks() \n")
#       message("The index of Census block groups (localtree) cannot be created until quaddata is loaded ... Trying dataload_from_pins() before indexblocks()")
#       dataload_from_pins("quaddata")
#     } else {
#       cat("Building index of Census Blocks (localtree)...\n")
#       assign(
#         "localtree",
#         SearchTrees::createTree(quaddata, treeType = "quad", dataType = "point"),
#         envir = globalenv()
#         # need to test, but seems to work.
#         # But takes a couple seconds at every reload of pkg.
#       )
#       cat("  Done building index.\n")
#     }
#     
#     
#   } else {
#     cat('The index of Census block groups localtree index of blocks appears to have been created.\n')
#   }
#   return(TRUE)
# }
