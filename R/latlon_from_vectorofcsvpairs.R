

#' helper function - convert vector of lat,lon pairs to data.frame
#'
#' @param x vector of comma-separated pairs of lat,lon values
#'   stored as character strings
#'   such as c("30,-83",  "  32.5,  -86.377325 ")
#' @examples
#' lat_x = testpoints_10$lat
#' lon_x = testpoints_10$lon
#' latlon_pairs = latlon2csv(lat = lat_x, lon = lon_x)
#' latlon_from_vectorofcsvpairs(latlon_pairs) 
#' all.equal(testpoints_10[, c("lat", "lon")], latlon_from_vectorofcsvpairs(latlon_pairs))
#' 
#' x = latlon_from_vectorofcsvpairs(c("30,-83",  "  32.5,  -86.377325 "))
#' x
#' latlon_is.valid(x)
#' 
#' @return data.frame with colnames lat and lon
#' 
#' @keywords internal
#' @export
#'
latlon_from_vectorofcsvpairs <- function(x) {
  
  # latloncsv2df()  could be an alias
  
  stopifnot(!is.null(x) && is.atomic(x) && is.vector(x) && (is.character(x) || all(is.na(x))))
  if (all(is.na(x))) {
    # if only n provided and all are NA, then return a n-row data.frame with NA values for lat and lon, just like the row returned for each NA if multiple inputs where some are ok and some are NA
    return(
      structure(list(lat = rep(NA_real_, length(x)), lon = rep(NA_real_, length(x))), row.names = 1:length(x), class = "data.frame")
    )
  }
  
  # remove all spaces
  x = gsub(" ", "", x)
  
  if (!all(grepl(",", na.omit(x)))) {
    stop('commas missing -- x must be a character vector where each non-NA element has a comma, separating two numbers that are lat and lon, e.g., c("30,-83","32.5,-86.377325")')}
  if (!all(is.numericish( unlist(strsplit(x,",")) ))) {
    stop('non-numeric values found -- x must be a character vector where each non-NA element has a comma, separating two numbers that are lat and lon, e.g., c("30,-83","32.5,-86.377325")')}
  
  x <- gsub(pattern = " ", "", x)
  x <- as.data.frame(do.call(rbind, strsplit(x, ",")))
  if (NCOL(x) != 2) {stop("problem trying to parse lat,lon pairs - maybe more than one comma used in what should be a lat,lon pair")}
  colnames(x) <- c("lat", "lon")
  x <- as.data.frame(lapply(x, as.numeric))
  
  return(x)
  
  # latlon_pairs
  ## [1] "30.977402,-83.368997"  "32.515813,-86.377325"  "42.23498,-88.30541"    "33.870013,-118.377777"
  ## [5] "34.014929,-118.205387" "40.731099,-74.173067"  "37.81144,-121.29348"   "44.85387,-93.04713"   
  ## [9] "41.18661,-111.94904"   "40.71239,-74.5847"    
  # 
  # #> latlon_from_vectorofcsvpairs(latlon_pairs) # not rounded
  ##          lat         lon
  ## 1  30.977402  -83.368997
  ## 2  32.515813  -86.377325
  # etc.
  # #> testpoints_10[, c("lat", "lon")] # prints it rounded to 5 decimals
  ##         lat        lon
  ## 1  30.97740  -83.36900
  ## 2  32.51581  -86.37732
  # etc.
}
##################################################### #


#' helper function - combine lat/lon values into csv format
#' @description Combines a vector of latitudes and a vector of longitudes
#'   into one vector of comma-separated pairs like latitude,longitude 
#' @param lat vector of latitudes
#' @param lon vector of longitudes
#'
#' @return vector of comma-separated pairs (see example)
#' 
#' @examples
#'    lat_example = c(30.01,30.26,30.51)
#'    lon_example = c(-90.61,-90.95,-91.23)
#'    latloncsv_example = c("30.01,-90.61", "30.26,-90.95", "30.51,-91.23")
#'    all.equal(latloncsv_example, 
#'              latlon2csv(lat = lat_example, lon = lon_example)
#'    )
#'
#' @keywords internal
#' @export
#'
latlon2csv <- function(lat, lon) {
  
  ## combines a list of latitudes and list of longitudes
  ## into a vector of comma-separated values for latitude and longitude 
  
  latloncsv <- paste(lat, lon, sep = ',')
  return(latloncsv)
}
##################################################### #


#' helper function - combine lat/lon values to paste into NEXUS tool
#' 
#' Converts vector of comma-separated values for latitude and longitude 
#'   into a format you can paste into NEXUS tool lat/lon site selection box
#' @param latloncsv a vector of comma-separated values with lat,lon
#'
#' @return a single character string that has all the csv pairs, 
#'   with a semicolon between each pair and the next, like
#'   "30.01,-90.61;30.26,-90.95;30.51,-91.23"
#' @examples
#'  latloncsv_example = c("30.01,-90.61", "30.26,-90.95", "30.51,-91.23")
#'  latloncsv2nexus(latloncsv_example)
#'
#' @keywords internal
#' @export
#'
latloncsv2nexus <- function(latloncsv) {
  
  ## converts vector of comma-separated values for latitude and longitude 
  ## into a format you can paste into NEXUS tool lat/lon site selection box
  
  # some error checking
  latitudes  = as.numeric( gsub('(.*),.*', '\\1', latloncsv))
  longitudes = as.numeric(gsub('.*,(.*)', '\\1', latloncsv))
  if (length(latitudes) != length(longitudes))  {
    stop('must have same number of lat as lon')}
  if (any(is.na(latitudes)) | any(is.na(longitudes))) {
    stop('some lat or lon are NA')}
  if (any(!latlon_is.valid(lat = latitudes, lon = longitudes))) {
    stop('some lat or lon do not seem to be valid numbers')}
  
  nexusformat  <- paste(latloncsv, collapse = ';')
  return(nexusformat)
}
##################################################### #


#' helper function - combine lat/lon values to paste into NEXUS tool
#' 
#' Converts 2 vectors of values for latitude and longitude 
#'   into a format you can paste into NEXUS tool lat/lon site selection box
#' @param lat vector of latitudes
#' @param lon vector of longitudes
#'
#' @return a single character string that has all the csv pairs, 
#'   with a semicolon between each pair and the next like 
#'   "30.01,-90.61; 30.26,-90.95; 30.51,-91.23"
#'
#' @examples
#'   lat_example = c(30.01,30.26,30.51)
#'   lon_example = c(-90.61,-90.95,-91.23)
#'   latlon2nexus(lat=lat_example, lon=lon_example)
#'   
#' @keywords internal
#' @export
#'   
latlon2nexus <- function(lat, lon) {
  
  ## combines a list of latitudes and list of longitudes
  ## into a format you can paste into NEXUS tool lat/lon site selection box
  
  latloncsv    <- latlon2csv(lat, lon)
  nexusformat  <- latloncsv2nexus(latloncsv)
  return(nexusformat)
}
##################################################### #
