
#' which points are near any of the others in a list?
#'
#' @param lon longitude
#' @param lat latitude 
#' @param distance distance between points in miles to check 
#' @param or_tied if TRUE, checks if less than or equal to distance, otherwise if less than
#' @return logical vector the length of lon or lat, telling if the point is
#'   within distance of any other point in list for example, 
#'   which sites have residents that might also be near others sites?
#' @examples 
#' mapfast(testpoints_500[distance_near_eachother(
#'   lon = testpoints_500$lon, 
#'   lat = testpoints_500$lat, 
#'   3.1), ], radius = 3.1)
#'    
#' @export
#' @keywords internal
#'
distance_near_eachother <- function(lon, lat, distance, or_tied=FALSE) {
  
# returns logical vector the length of lon or lat, telling if the point is within distance of any other point in list
# for example,  which sites have residents that might also be near others sites?
df <- data.frame(lon = lon, lat = lat)
  distance_pairs <-  distances.all(df, df)
distance_pairs <- distance_pairs[distance_pairs$fromrow != distance_pairs$torow, ] # remove distance from point to itself
# circles overlap if 2 facilities are twice the radius apart but we just want to check distance here
is_near_any <- (distance_pairs$d < distance)
original_rownums_near_any <- 1:NROW(lon) %in% distance_pairs$fromrow[is_near_any]
return(original_rownums_near_any)

# blah <- read.csv('./inst/testdata/testpoints_500.csv')
# nnn <- distance_near_eachother(blah$lon, blah$lat, 10)
# plot(blah$lon, blah$lat, main='red points are the ones near some other point')
# points(blah$lon[nnn],blah$lat[nnn], col='red')
# 
# plot(blah$lon, blah$lat,  main='red points are the ones near some other point')
# points(blah$lon[nnn],blah$lat[nnn], col='red')

}
