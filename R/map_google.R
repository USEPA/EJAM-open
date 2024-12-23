
#' Map - get URL(s) that would display map(s) on maps.google.com
#'
#' @param lat - Anything that can be handled by [sitepoints_from_any()].
#'   Leave unspecified to interactively browse to a .xlsx file that has lat,lon columns,
#'   or lat can be a data.frame with lat,lon column names in which case longitude should not be provided,
#'   such as lat = testpoints[1,], or lat and lon can be separately provided as vectors.
#' @param lon longitude, or omit this parameter to provide points as the first parameter.
#' 
#' @param zoom zoomed out value could be 3 or 5, zoomed in default is 12
#' @param point logical, if TRUE, URL will have a marker at the point
#'   and zoom parameter is ignored. Otherwise just the map.
#'
#' @return URL(s) vector one per point
#' @seealso [map_google()]
#' 
#' @export
#' @keywords internal
#'
url_map_google <- function(lat, lon, zoom = 13, point = TRUE) {
  
  # "https://developers.google.com/maps/documentation/urls/get-started"
  
  # also see map_google()
  
  if (missing(lon)) {
    pts = sitepoints_from_any(lat)
    lat = pts$lat
    lon = pts$lon
  }
  
  if (point) {
    
    paste0("https://www.google.com/maps/search/?api=1&query=", lat, "%2C", lon) 
    # provides marker at the point but cannot specify zoom
    
  } else {
    if (kind == 1) {
      paste0("https://www.google.com/maps/@?api=1&map_action=map&basemap=satellite&center=", lat, "%2C", lon, "&", "zoom=", zoom ) 
      # no marker but can specify zoom and basemap satellite
    } else {
      paste0("https://www.google.com/maps/@", lat, ",", lon, ",", zoom, "z")
      # no marker but can specify zoom and the default is satellite?
    }
  }
}
############################ ############################# ############################# #
