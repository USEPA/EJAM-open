

# if radius = 0 was requested,
# adjusted distance is always less than unadjusted,
# except when they both are 0.

set.seed(999)
samp <- sample(1:NROW(blockpoints), 1000)
pts <- data.frame(lat = blockpoints$lat[samp],
                  lon = blockpoints$lon[samp])

testthat::test_that("distance gets adjusted up if radius zero", {
  # zero radius so distance always has to get adjusted
  radius <- 0
  x <- getblocksnearbyviaQuadTree(sitepoints = pts, radius = radius,
                                  quadtree = localtree, quiet = T, report_progress_every_n = 2000)
  testthat::expect_true(all(x$distance >= x$distance_unadjusted))
  testthat::expect_true(all(x$distance > x$distance_unadjusted | x$distance == 0))
  testthat::expect_true(all(x$distance_unadjusted <= x$radius))
})




cat(  "work in progress ! needs to be continued...\n\n")


### typical distance, so original unadj distance <= dist,
### and distance (adj or unadj) always <= radius
# test_that("unadjusted distance <= distance adjusted, and unadj d < radius", {
#
# radius <- 1
# suppressWarnings( {
# x <- getblocksnearbyviaQuadTree(sitepoints = pts, radius = radius,
# quadtree = localtree, quiet = T, report_progress_every_n = 2000,
# avoidorphans = FALSE)})
# testthat::expect_true(all(x$distance_unadjusted <= radius)) # TRUE since avoidorphans FALSE
# testthat::expect_true(all(x$distance_unadjusted <= x$distance)) # *** false why?? strange shape to block can mean even if distunadj < effectrad,  somehow... distunadj > 0.9*effectiveradius ???
# testthat::expect_true(all(x$distance <= radius) ) # ?? it can get adjusted to be >radius, ***but then still may want to filter out to report 0 within radius?
#})



# testthat::test_that("avoidorphans does as expected", {
### avoidorphans  TRUE
###
# suppressWarnings( {
#   x <- getblocksnearbyviaQuadTree(sitepoints = pts, radius = radius,
#   avoidorphans = TRUE,
#   quadtree = localtree, quiet = T, report_progress_every_n = 2000)})
# testthat::expect_true(  ?   all(x$distance_unadjusted <= radius))  # Should it sometimes be FALSE ??
# testthat::expect_true( ? all(x$distance_unadjusted <= x$distance))  # FALSE
# testthat::expect_true( ? all(x$distance <= radius))
# }



########## #


### one point, invalid
###
# pts <- data.frame(
#   lat = 200,
#   lon = 200
# )
# radius <- 3
# x <- getblocksnearbyviaQuadTree(pts, radius = radius,
# quadtree = localtree, quiet = T, report_progress_every_n = 2000)
# NROW(x) == 0   # *** but why not  1 row of NA values??


### >1 point, all invalid



### >1 point, some invalid
