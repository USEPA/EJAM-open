
#' Use EJScreen API to get stats on ONLY ONE circular buffer
#' 
#' Get EJScreen report results for one circular buffer, as a data.frame
#' 
#' @details
#' Specify a radius and vector of latitude longitude points,
#' and get for a buffer the population weighted mean value of each raw indicator
#' like percent low-income, and total population count, and percentiles for those
#' raw indicator scores, all from EJScreen, as in an EJScreen standard report. 
#' 
#' Note that this API is fairly slow, so it is fine for 10 sites, but not large numbers.
#' 
#'  Relies on [ejscreenRESTbroker()] for the actual request via API,
#'   and [ejscreenRESTbroker2table()] to format it and handle errors.
#' 
#'   It essentially does this: ejscreenRESTbroker2table(ejscreenRESTbroker()) 
#'    And then it adds the columns  "pdf_report" and "pdf_acs_report" 
#'   
#'   It returns a 1-row data.frame 
#'   
#'   and makes relevant columns values numeric, 
#'   and converts text like 45% to the number 45.
#' 
#'   It also drops redundant columns where the same numbers had been returned from API
#'   using the normal name and a synonym name, as with TOTALPOP and "totalPop"
#'    
#'    See  (https://www.epa.gov/ejscreen/ejscreen-api)
#' 
#' @param lon Longitude numeric 
#' @param lat Latitude numeric 
#' @param radius radius, in miles, of circular buffer 
#' @param unit miles (default) or kilometers
#' @param wkid optional spatial reference code. https://epsg.io/4326
#' @param fips if used instead of lon,lat it should be a character FIPS code 
#'   (counties, tracts, or blockgroups)
#' @param namestr optional text used on report if fips provided and you want to show this text instead of the FIPS code on the report
#' @param shapefile not implemented
#' @param format_report_or_json default is "pjson" but could "report" to get URL for a pdf report
#' @param ipurl IP or URL start
#' @param getstatefromplacename set to FALSE if you need the exact output of API and
#'   TRUE if you want to try to extract ST abbrev and statename from the placename field,
#'   which is more likely to be correct than the stateAbbr and stateName fields in the API output.
#' @seealso [ejscreenit()] [ejscreenapi_plus()] [ejscreenapi()]
#'    that uses [ejscreenapi1()] and [ejscreenRESTbroker()]  and [ejscreenRESTbroker2table()]
#' @examples  
#'  \dontrun{
#'  # Specify size of buffer circle and pick random points as example data
#'  myradius <- 1
#'  pts <- structure(list(lon = c(-96.4798957, -111.7674343, -75.4173589, 
#'  -95.9573172, -87.8402677, -77.9996191, -73.920702, -79.9545638, 
#'  -76.0638877, -114.9881473), lat = c(31.782716, 33.7522735, 39.8697972, 
#'  33.2522474, 41.9763992, 38.4661259, 41.2940801, 32.8099327, 40.9888266, 
#'  36.0043628), id = 1:10), row.names = c(NA, -10L), class = "data.frame")
#'  out1 <- ejscreenapi1(lon = pts$lon[1], lat=pts$lat[1], radius = myradius)
#'  t(out1)
#'  #out <- ejscreenapi(lon=pts$lon, lat=pts$lat, radius = myradius)
#'  #t(out[1:2,])
#'  }
#'  
#' @export
#' @keywords internal
#'  
ejscreenapi1 <- function(lon, lat, radius = 3, unit = 'miles', wkid = 4326, 
                         fips = NULL, 
                         shapefile = NULL,
                         namestr = '',
                         format_report_or_json = 'pjson', ipurl = 'ejscreen.epa.gov',
                         getstatefromplacename = TRUE) {
  
  if (!is.null(shapefile)) {warning('shapefile not implemented yet')}
  
  if (any(!is.null(fips))) {
    radius <- 0
    lat = rep(NA, length(fips))
    lon = rep(NA, length(fips))
  } else {
    #################################################################################### #
    # warn if invalid lat lon values or radius ####
    # overlaps with ejscreenapi()
    ok_point <- latlon_is.valid(lon = lon, lat = lat)
    if (!all(ok_point)) {
      warning(paste0(sum(!ok_point), ' lat lon values look invalid'))
      lat[!ok_point] <- NA; lon[!ok_point] <- NA
      # rather than drop those rows, try to return NA values
    } 
    if (any(is.na(radius)) | any(radius <= 0) | any(radius > 100)) {stop('radius outside allowed range')}
  }
  if (!(unit %in% c('miles', 'kilometers'))) {stop('unit must be miles or kilometers')}
  unitcode = switch(unit,
                    'miles' = 9035,
                    'kilometers' = 9036
  )
  #################################################################################### #
  
  # If a report was requested, return the URL ####
  
  if (!(format_report_or_json %in% c('pjson', 'report'))) {stop('format_report_or_json must be pjson or report')}
  
  if (format_report_or_json == 'report') {
    # do not need to use the API just to create the URL
    #cat('see url_ejscreen_report() for more options in getting links to reports')
    myurl <- url_ejscreen_report(
      lon = lon, lat = lat, radius = radius, 
      as_html = FALSE, mobile = FALSE, 
      # areatype = ,  # function infers that from fips
      # shapefile = shapefile, ## how would shapefile be handled?
      areaid = fips, 
      namestr = namestr, # to test
      wkid = wkid, unit = unit, 
      f = "report")
    return(myurl)
  }
  #################################################################################### #
  
  # MAKE THE REQUEST VIA API ####
  
  ej.data <- try(ejscreenRESTbroker(
    fips = fips, 
    namestr = namestr,  # to test
    shapefile = shapefile, # would need POST not GET
    lon = lon, lat = lat, 
    radius = radius, 
    unit = unitcode, wkid = wkid,
    f = 'pjson', ipurl = ipurl
  ))
  #################################################################################### #
  
  # parse pjson results to data.frame format, for just this one buffer ####
  # and ejscreenRESTbroker2table does error-checking
  ej.data <- ejscreenRESTbroker2table(ej.data, getstatefromplacename = getstatefromplacename)
  
  ################################################################## #
  invisible(ej.data)
}
