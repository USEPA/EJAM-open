
#' Check if lat lon not NA using !is.na()
#'
#' @param lat vector of latitudes
#' @param lon vector of longitudes
#' @return logical vector, one element per lat lon pair (location)
#' @seealso   [latlon_is.usa()] [latlon_is.islandareas()] [latlon_is.available()] [latlon_is.possible()]
#'
#' @keywords internal
#'
latlon_is.available  <- function(lat, lon) {
  if(missing(lat) | missing(lon)){
    warning('"lat" and/or "lon" argument not provided, please provide both values.')
    return(FALSE)
  }
  if(all(is.na(as.numeric(lat))) | all(is.na(as.numeric(lon)))){
    warning('"lat" and/or "lon" cannot be coerced to a numeric.')
    return(FALSE)
  }
  if(is.null(lat) | is.null(lon)){
    warning('No lat or lon column found')
    return(FALSE)
  }
  !is.na(lat) & !is.na(lon)
}
############################################### #


#' Check lat lon coordinates to see if each is approx. in general area of USA excluding Island Areas
#'
#' @param lat vector of latitudes
#' @param lon vector of longitudes
#' @return logical vector, one element per lat lon pair (location)
#'   Indicates the point is approximately in one of the
#'   rough bounding boxes that includes the USA without
#'   the Island Areas Guam, American Samoa, USVI, N Marianas Islands.
#' @seealso   [latlon_is.usa()] [latlon_is.islandareas()] [latlon_is.available()] [latlon_is.possible()]
#'
#' @keywords internal
#'
latlon_is.usa <- function(lat, lon) {
  if(missing(lat) | missing(lon)){
    warning('"lat" and/or "lon" argument not provided, please provide both values.')
    return(FALSE)
  }
  if(all(is.na(as.numeric(lat))) | all(is.na(as.numeric(lon)))){
    warning('"lat" and/or "lon" cannot be coerced to a numeric.')
    return(FALSE)
  }
  if(is.null(lat) | is.null(lon)){
    warning('No lat or lon column found')
    return(FALSE)
  }
  !(
    (lat < 17.5 | lat > 71.5) |   (lon > -64 & lon < 172) |  (lon > 180 | lon < -180)
  )
}
############################################### #


#' Check if lat lon between -180 and +180
#'
#' @param lat vector of latitudes
#' @param lon vector of longitudes
#' @return logical vector, one element per lat lon pair (location)
#' @seealso   [latlon_is.usa()] [latlon_is.islandareas()] [latlon_is.available()] [latlon_is.possible()]
#'
#' @keywords internal
#'
latlon_is.possible   <- function(lat, lon) {
  if(missing(lat) | missing(lon)){
    warning('"lat" and/or "lon" argument not provided, please provide both values.')
    return(FALSE)
  }else if (is.null(lat) | is.null(lon)){ 
    warning('"lat" and/or "lon" argument not provided, please provide both values.')
    return(FALSE)
  }
  if(all(is.na(as.numeric(lat))) | all(is.na(as.numeric(lon)))){
    warning('"lat" and/or "lon" cannot be coerced to a numeric.')
    return(FALSE)
  }
  (lat < 180 & lat > -180  &  lon < 180 & lon > -180)
}
############################################### #


#' Check lat lon coordinates to see if each is approx. in general area of US Island Areas Guam, USVI, Amer Samoa or N Marianas
#'
#' See [islandareas]
#' @param lat vector of latitudes
#' @param lon vector of longitudes
#' @param exact_but_slow_islandareas optional logical, set it to TRUE to check each point vs
#'   boundaries in [states_shapefile] to identify which ones are in Island Areas according to that shapefile.
#'   The default method here is much faster, but just checks if a point is within a bounding box
#'   that should approximate each of the Island Areas, found in the object [islandareas].
#' @return vector of TRUE / FALSE values indicating a given lat lon pair
#'   is approximately in one of the rough bounding boxes that includes the 4 Island Areas.
#' @seealso   [latlon_is.usa()] [latlon_is.islandareas()] [latlon_is.available()] [latlon_is.possible()]
#' @examples
#' \dontrun{
#' # this would require the testpoints_1000 data from the EJAM package:
#'   isles <- which(latlon_is.islandareas(lat = testpoints_1000$lat, lon = testpoints_1000$lon))
#'   mapfast(testpoints_1000[isles, ]) # c(213,785)
#'   which(!(latlon_is.usa(lat = testpoints_1000$lat, lon = testpoints_1000$lon)))
#' }
#'
#' @keywords internal
#'
latlon_is.islandareas <- function(lat, lon, exact_but_slow_islandareas = FALSE)  {
  
  if (missing(lat) | missing(lon)) {
    warning('"lat" and/or "lon" argument not provided, please provide both values.')
    return(FALSE)
  }
  if (all(is.na(as.numeric(lat))) | all(is.na(as.numeric(lon)))) {
    warning('"lat" and/or "lon" cannot be coerced to a numeric.')
    return(FALSE)
  }
  if (is.null(lat) | is.null(lon)) {
    warning('No lat or lon column found')
    return(FALSE)
  }
  
  if (exact_but_slow_islandareas) {
    ST <- state_from_latlon(lat = lat, lon = lon)$ST
    ok <- ST %in% islandareas$ST
  } else {
    
    ## caveat: See details at https://www.britannica.com/place/Trust-Territory-of-the-Pacific-Islands on areas no longer part of the US but still with some sites in FRS, "110009291462" "110013804678" "110067353429" "110067377430" "110070929074" E.G. https://echo.epa.gov/detailed-facility-report?fid=110067353429 https://echo.epa.gov/detailed-facility-report?fid=110013804678 
    x <- islandareas
    states <- unique(x$ST)
    # ok <- rep(TRUE, length(states))
    ok  <- list()
    for (i in 1:length(states)) {
      ok[[i]] <- (lat > x$lat[x$limit == "min" & x$ST == states[i]]) &
        (lat < x$lat[x$limit == "max" & x$ST == states[i]]) &
        (lon > x$lon[x$limit == "min" & x$ST == states[i]]) &
        (lon < x$lon[x$limit == "max" & x$ST == states[i]])
    }
    ok <- apply(do.call(rbind, ok), 2, any)
  }
  return(ok)
}
############################################### #


#' Check if lat lon are OK -- validate latitudes and longitudes
#'
#' @description Check each latitude and longitude value to see if they are valid.
#' @details
#'   NA or outside expected numeric ranges
#'
#'   (based on approx ranges of lat lon seen among block internal points dataset)
#'
#'   But note Guam, American Samoa, Northern Mariana Islands, and U.S. Virgin Islands ranges are approximated!
#'   EJScreen has not had demographic data in those locations anyway, but can map sites there.
#'   see latlon_is.islandareas()
#'   and note details at https://www.britannica.com/place/Trust-Territory-of-the-Pacific-Islands
#'    on areas no longer part of the US but still with some sites in FRS,
#'    ids "110009291462" "110013804678" "110067353429" "110067377430" "110070929074" 
#'    e.g., https://echo.epa.gov/detailed-facility-report?fid=110067353429
#'    or https://echo.epa.gov/detailed-facility-report?fid=110013804678 
#'
#'   lat must be between 17.5 and 71.5, and
#'
#'   lon must be ( between -180 and -64) OR (between 172 and 180)
#' @param lat vector of latitudes 
#'   (or data.frame with colnames lat and lon, in which case lon param must be missing)
#' @param lon vector of longitudes
#' @param quiet optional logical, if TRUE, show list of bad values in console
#' @param invalid_msg_table set TRUE if you want a data.frame with colnames "valid" and "invalid_msg"
#' @return logical vector, one element per lat lon pair (location)
#' @param exact_but_slow_islandareas see [latlon_is.islandareas()]
#' @seealso   [latlon_is.usa()] [latlon_is.islandareas()] [latlon_is.available()] [latlon_is.possible()]
#'   [latlon_df_clean()] [latlon_infer()] [latlon_is.valid()] [latlon_as.numeric()]
#' @examples  \dontrun{
#'  # this would only work using the EJAM package datasets frs and blockpoints:
#'    if (!exists("frs")) dataload_from_pins("frs")
#'  table(latlon_is.valid(lat =  frs$lat, lon =  frs$lon))
#'  # blockpoints may need to be downloaded using dataload_from_pins()
#'  table(latlon_is.valid(lat =  blockpoints$lat, lon =  blockpoints$lon))
#'   }
#'
#' @export
#'
latlon_is.valid <- function(lat, lon, quiet = TRUE, invalid_msg_table = FALSE , exact_but_slow_islandareas = FALSE) {

  
  if (missing(lon) && !missing(lat)) {
    if (is.data.frame(lat)) {
      # allow user to provide a data.frame with lat and lon columns, as an option
      # do no use latlon_infer() here -- needs to have been done already elsewhere
      if (!all(c("lat", "lon") %in% colnames(lat))) {
        warning("if lat is a data.frame it must have lat and lon as colnames")
        if (invalid_msg_table) {
          return(data.frame(valid = rep(FALSE, NROW(lat)), invalid_msg = "lat or lon column names invalid"))
        } else {
          return(FALSE)
        }
      }
      lon <- lat$lon
      lat <- lat$lat
    } else {
      warning('no lon values provided')
      if (invalid_msg_table) {
        return(data.frame(valid = rep(FALSE, NROW(lat)), invalid_msg = "lon missing"))
      } else {
      return(FALSE)
    }}
  }
  if (missing(lat)) {
    warning('no lat values provided')
    if (invalid_msg_table) {
      return(data.frame(valid = rep(FALSE, NROW(lon)), invalid_msg = "lat missing"))
    } else {
      return(FALSE)
    }
  }
  if (is.null(lat) | is.null(lon)) {
    warning('No lat provided or no lon provided')
    if (invalid_msg_table) {
      return(data.frame(valid = rep(FALSE, NROW(lon) + NROW(lat)), invalid_msg = "lat or lon were NULL"))
    } else {
      return(FALSE)
    }
  }
  if (all(is.na(as.numeric(lat))) | all(is.na(as.numeric(lon)))) {
    warning('"lat" and/or "lon" cannot be coerced to a numeric.')
    if (invalid_msg_table) {
    return(data.frame(valid = rep(FALSE, NROW(lat)), invalid_msg = "lat or lon not numeric"))
    } else {
    return(FALSE)
  }

}
  
  
  # assume none bad until proven otherwise
  bad         <- rep(FALSE, length(lat))
  
  unavailable <- !latlon_is.available(lat = lat, lon = lon) # is.na(lat) | is.na(lon)
  
  impossible  <-  !latlon_is.possible(lat = lat, lon = lon) # lat > 180 | lat < -180  |  lon > 180 | lon < -180
  
  roughly_in_core_usa_excluding_islandareas <- latlon_is.usa(lat = lat, lon = lon)
  #   !(
  #   (lat < 17.5 | lat > 71.5) |   (lon > -64 & lon < 172) |  (lon > 180 | lon < -180)
  # )
  
  in_islandareas <- latlon_is.islandareas(lat = lat, lon = lon, exact_but_slow_islandareas = exact_but_slow_islandareas)
  if (any(in_islandareas, na.rm = TRUE)) {
    message("Some points appear to be in US Island Areas, which may lack some data such as demographic data here")
  }
  
  bad <- unavailable | impossible |
    ( !roughly_in_core_usa_excluding_islandareas & !in_islandareas )
  
  if (any(bad)) {
    warning('Some lat or lon values are invalid - NA or number entirely outside expected ranges (US including Island Areas)')
    if (!quiet) {
      cat('\nInvalid lat lon points:\n\n ')
      print(data.table(lat = lat[bad], lon = lon[bad]))
      cat('\n\n')
    }
    if (all(bad)) {
      if (any(latlon_is.valid(lat = c(lon, testpoints_10$lon[1]), lon = c(lat, testpoints_10$lat[1])))) {
        # added 1 valid but swapped lat/lon pair to avoid infinite recursion loop here if all bad both ways
        warning("Maybe lat vs lon got mixed up. Did you accidentally provide lon,lat instead of lat,lon ?")
      }
    } 
  }
  if (invalid_msg_table) {
    # ("valid" and "invalid_msg")
    x = data.frame(valid = !bad, invalid_msg = ifelse(bad, "invalid latlon", ""))
    x$invalid_msg[!roughly_in_core_usa_excluding_islandareas & !in_islandareas] <- "latlon may be outside States/PR/Island Areas"
    x$invalid_msg[impossible] <- "latlon impossible"
    x$invalid_msg[unavailable] <- "latlon missing"
    return(x)
  } else {
    return(!bad)
  }
  }
  # sort(unique(substr(bgpts$bgfips,1,2)))     # bgpts has PR not island areas
  # sort(unique(substr(blockid2fips$blockfips,1,2))) #  has PR not island areas
  ## same for bgid2fips
  # sort(unique(substr(blockgroupstats$bgfips,1,2))) # has island areas too: "60" "66" "69" "72" "78" (but not lat,lon)
  
  #
  #   > range(blockpoints$lat)
  # [1] 17.88513 71.39840
  # > # lat must be between 17.5 and 71.5
  #
  # > min(blockpoints$lon[blockpoints$lon > -65])
  # [1] 172.5912 # and must be < 180 (one is at 179.6212)
  # > max(blockpoints$lon[blockpoints$lon < 0])
  # [1] -65.20799 # and must be > -180 (min seen is -179.1084)
  # > # lon must be ( between -180 and -65) OR (between 172 and 180) -- but actually US VI might reach almost -64 longitude
  
  #    NOTE THAT Guam etc. have points outside these ranges!
  
  # BUT NOTE FRS SEEMS TO HAVE PROBLEMATIC LAT LON VALUES
  #
  # > range(frs$lat)
  # [1] -43.78711  88.07278
  # > range(frs$lon)
  # [1] -179.3000  179.2599
  
  # > table(latlon_is.valid(lat = frs$lat, lon=frs$lon))
  #
  # FALSE    TRUE
  # 1384 3454658
  #

