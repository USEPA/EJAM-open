
## what is trilateration and how to implement it?
#
# https://www.appelsiini.net/2017/trilateration-with-n-points/
# https://gis.stackexchange.com/questions/474716/doing-high-fidelity-trilateration-in-r
# https://gis.stackexchange.com/questions/48937/calculating-intersection-of-two-circles

#################################### # #################################### # 


#' DRAFT - Estimate lat,lon of site(s) from sites2blocks output of getblocksnearby()
#' trilateration -- Use lat,lon of nearby block points and distances to estimate original sitepoints
#' @details
#' This function is needed ONLY if you did not retain site latlons,
#'  and then only for sites not entirely in single states based on their nearby blocks.
#' This is slow and assumes you do not already know the lat,lon of the sitepoints.
#' If for some reason all you have is output of [getblocksnearby()] then this is how you could 
#' estimate where the original sitepoint(s) were that were input(s) to [getblocksnearby()]
#' 
#' But ejamit() or the shiny app do not require doing this since the original latlon
#' of sitepoints are retained and provided to [doaggregate()]
#' which needs to figure out what state each site is in to use the right state percentiles.
#' 
#' @param s2b sites2blocks data.table that is output of [getblocksnearby()]
#'
#' @return data.table with columns ejam_uniq_id, lat, lon, one row per site
#' @examples
#'  pts = testpoints_10
#'  #x = latlon_from_s2b(getblocksnearby(pts, quiet = T))
#'  x = latlon_from_s2b(testoutput_getblocksnearby_10pts_1miles)
#'  cbind(estimate = x, pts, 
#'    latratio = x$lat/pts$lat, lonratio = x$lon/pts$lon)
#' 
#' @keywords internal
#'
latlon_from_s2b <- function(s2b) {
  
  # could recode to join latlon only for a few blockpoints that are closest, since that is all we are using here
  # out the hundreds of blocks at each site!
  
  x = data.table::copy(s2b)
  if (!("lat" %in% names(x)) || !("lon" %in% names(x))) {
    EJAM:::latlon_join_on_blockid(s2b = x) # from EJAM
  } 
  x <- x[distance > 0,]
  data.table::setorder(x, distance)
  
  sitepoints_approx = list()
  ids = unique(x$ejam_uniq_id)
  
  for (i in  seq_along(ids)) {
    
    x1 <- x[ejam_uniq_id == ids[i], ]
    
    # try 3 points in just trilat3: 
    lat = x1$lat[   c(min(2,NROW(x1)), min(4,NROW(x1)), min(6,NROW(x1)))]
    lon = x1$lon[   c(min(2,NROW(x1)), min(4,NROW(x1)), min(6,NROW(x1)))]
    d = x1$distance[c(min(2,NROW(x1)), min(4,NROW(x1)), min(6,NROW(x1)))]
    bestguess =  (trilat3(data.frame(lat = lat, lon = lon), d))
    sitepoints_approx[[i]] <- data.table(ejam_uniq_id = ids[i], 
                                         lat = bestguess$lat, lon = bestguess$lon)
  }
  
  sitepoints_approx <- rbindlist(sitepoints_approx)
  setorder(sitepoints_approx, ejam_uniq_id)
  return(sitepoints_approx)
}
#################################### # #################################### # 


trilat3 <- function(pts, distance) {
  
  # library(geosphere)  # that pkg used meters. replace with distances.all() which uses miles
  
  ## Uses any number of points (and distance of each) to locate the 1 point
  # https://gist.github.com/tuupola/0df4934758fa04a3f07c96d55cd31bb1
  # https://www.appelsiini.net/2017/trilateration-with-n-points/
  
  locations <- data.frame(latitude = pts$lat, longitude = pts$lon, distance = distance)
  # Use average as the starting point, then iterate
  fitLongitude = mean(locations$longitude) # this line was missing from example
  fitLatitude  = mean(locations$latitude)  # this line was missing from example
  
  fit <- stats::nls(
    # distance ~ geosphere::distm(  #  calculates haversine distance matrix in meters
    distance ~ as.matrix(distances.all( # distance matrix in miles
      data.frame(lon = longitude, lat = latitude),
      data.frame(lon = fitLongitude, lat = fitLatitude),
      return.crosstab = TRUE)
    ),
    data = locations,
    start = list(
      fitLongitude = fitLongitude,  # changed from example
      fitLatitude  = fitLatitude    # changed from example
    ),
    control = list(maxiter = 500, tol = 1e-02)
  )
  latitude  <- summary(fit)$coefficients[2]
  longitude <- summary(fit)$coefficients[1]
  return(data.frame(lat = latitude, lon = longitude))
}
#################################### # #################################### # 


