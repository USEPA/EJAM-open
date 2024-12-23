
#' query FRS API to find EPA facilities by registry ID or program ID
#' 
#' @description  Uses the Facility Registry Service (FRS) API to find sites by ID.
#'   This uses an API to find sites, but it is faster to look in a table
#'   if that FRS dataset is already loaded in an app, for example.
#' @param id vector of one or more character strings that must be 
#'   registry IDs (default) or program IDs 
#' @param type either frs (default) or program. 
#'   frs means all are registry_id and
#'   program means all are pgm_sys_id
#' @param ... passed through to [locate_by_id1()]
#' @seealso [locate_by_id1()] and alias is [get_facility_info_via_FRS()]
#' @aliases get_facility_info_via_FRS
#'
#' @return data.frame with one row per queried id, columns as returned by API
#'   but lat lon instead of Latitude83 Longitude83
#' @examples 
#'   \dontrun{
#'     
#'   ids <- testids_program_sys_id
#'   # ids <- c('ILR000128264','600039382','TXR1592DZ','TSCA8851', 
#'   #    'CT0000000900908716', 'CEDRI10043548',  'CO0000000812305826')
#'   # ids <- c('ILR000128264','600039382')
#'   locate_by_id(ids, type = 'program')   # stopped working   ***********
#'    
#'  sites_found_by_registry_id <- locate_by_id(testids_registry_id[1],    
#'    type='frs')
#'  sites_found_by_program_id  <- locate_by_id(testids_program_sys_id[1], 
#'    type='program')
#'  data.frame(example_REGID  <- t(sites_found_by_registry_id))
#'  data.frame(example_PGMID  <- t(sites_found_by_program_id))
#'  # Finding several facilities is slow:
#'  sites_found_by_registry_id <- locate_by_id(testids_registry_id,    
#'    type='frs')
#'  sites_found_by_program_id  <- locate_by_id(testids_program_sys_id, 
#'    type='program')
#'  names(sites_found_by_program_id) 
#'  sites_found_by_registry_id[,c('RegistryId', 'lon','lat')]
#'  ## and  just to show agreement:  
#'  cbind(
#'   testids_program_sys_id,
#'   frs =  frs[match(
#'     sites_found_by_program_id$RegistryId, 
#'      frs$REGISTRY_ID
#'     ), 
#'     c('PGM_SYS_ACRNMS',  'REGISTRY_ID')],
#'   api = sites_found_by_program_id[, c('RegistryId', 'lat', 'lon')] 
#'  )
#'  
#'  # Leaflet map of sites, with a popup when one clicks on a site
#'  df <- sites_found_by_registry_id
#'  others <-  frs[sample(1:NROW( frs), 5000), 
#'   c('lon', 'lat')]
#'    leaflet::leaflet(df[4:6,]) |> leaflet::addTiles() |>
#'      leaflet::addMarkers(popup = popup_from_df(df[4:6,])) |>
#'      leaflet::addCircles(lng = others$lon, lat=others$lat, 
#'        radius = 3*meters_per_mile, color = 'gray')
#'       
#' # Map with clickable icons   
#' leaflet::leaflet(df) |> leaflet::addTiles() |> 
#' # leaflet::addCircleMarkers(lng=~lon, lat=~lat, radius = 3) |>
#'  leaflet::addMarkers(popup = popup_from_df(df), 
#'  icon = icons(iconUrl = './www/factory1.svg', 
#'    iconWidth = 30, iconAnchorX = 14, iconAnchorY = 18)
#'  )
#'   # Simple map of sites queried (among a sample of US EPA-regulated sites):
#'     
#'  xl <- c(-125,-66); yl <- c(17,50) # just continental US plus PR 
#'  plot( frs[sample(1:NROW(  frs),5000),c('lon','lat')],
#'   xlim=xl, ylim=yl, col='gray')
#'  graphics::points(cbind(
#'   LONG=sites_found_by_program_id$lon, 
#'   LAT=sites_found_by_program_id$lat), 
#'   col='red', pch=16)
#'  }
#'  
#' @keywords internal
#'
locate_by_id <- function(id, type='frs', ...) {
  # functions to QUERY FRS API by registry id or by program system id
  # STILL BUGGY:   locate_by_id(testids_registry_id[5:6], type='frs')  # AS AN EXAMPLE
  if (length(type) > 1) {'only 1 type can be specified for all the ids in a query here, at the moment'}
  seq <- 1:length(id)
  x <- vector('list', length = length(id))
  for (i in seq) {
    x[[i]] <- locate_by_id1(id[i], type = type, ...) #(id[i])  
    # could say type[i] to allow each type to be different in vector of ids, but would need type to be recycled to same length as id
  }
  # could say do.call(rbind, x) if they all had identical columns, but they do not necessarily.
  # consolidate those rows of results even if some rows have only some of the columns or extra columns:
  # because the API sometimes returns "SupplementalLocation" for example but not always
  # works only for x2: allnames <- unique(as.vector(sapply(x, function(x) (names(x)))))
  # works only for x9: allnames <- unlist(sapply(x, function(x) (names(x))))
  allnames <- unique(unlist(lapply(x,   function(y) (names(y)))))
  out <- data.frame(matrix(data = NA, nrow = 0, ncol = length(allnames)))
  colnames(out) <- allnames
  for (i in 1:length(x)) {
    out[i, colnames(x[[i]])] <- x[[i]]
  }
  rownames(out) <- seq.int(length.out = NROW(out))
  return(out)
  #################################################################### #
}


## get_facility_info_via_FRS  ####

get_facility_info_via_FRS <- locate_by_id # get_facility_info_via_FRS() alias

#################################################################### #


#' Get the URLs to use to query FRS API to find EPA facilities by ID
#' 
#' @description This uses an API to find sites, but it is faster to look in a table
#'   if that FRS dataset is already loaded in an app, for example.
#' @details For details on FRS API, 
#'   see https://www.epa.gov/frs/frs-rest-services 
#'   and examples at https://www.epa.gov/frs/frs-rest-services#ex1 
#'   and more at https://www.epa.gov/frs/frs-rest-services#appendixa 
#'   For example: 
#'  https://frs-public.epa.gov/ords/frs_public2/frs_rest_services.get_facilities?pgm_sys_id=VA0088986
#'  https://frs-public.epa.gov/ords/frs_public2/frs_rest_services.get_facilities?registry_id=110010912496
#'   Note: API URL for internal use at EPA appears to be different than public one? 
#' @param id vector of one or more character strings with pgm_sys_id or registry_id
#'   values (all need to be the same type, as defined by type parameter).
#'   Program ids are like "VA0088986" and frs ids are like "110015787683"
#' @param type one word, applies to all. default is frs but can be program or the word other.
#' @param ... appended to the end of the URL as-is, useful if type is other, for example
#' @seealso locate_by_id()
#'
#' @return vector of URLs as strings, same length as id parameter
#' @examples  \dontrun{
#'     url_by_id(testids_registry_id)
#'     browseURL(url_by_id(testids_registry_id)[1])
#' }
#' 
#' @keywords internal
#'
url_by_id <- function(id, type='frs', ...) {
  # specify URL for query in FRS API
  # given one or more facility IDs, return URL to use in query via FRS API
  # where id type is frs for registry_id or is program for pgm_sys_id 
  # 
  # functions to QUERY FRS API by registry id or by program system id
  
  if (!(type %in% c('frs', 'program', 'other'))) {
    stop('type of ID specified must be "frs" or "program", with program ids like "VA0088986" or frs ids like "110015787683" ')
  }
  if (type == 'program') {
    baseurl <- 'https://frs-public.epa.gov/ords/frs_public2/frs_rest_services.get_facilities?pgm_sys_id='
  }
  if (type == 'frs') {
    baseurl <- 'https://frs-public.epa.gov/ords/frs_public2/frs_rest_services.get_facilities?registry_id='
  }
  if (type == 'other') {
    baseurl <- 'https://frs-public.epa.gov/ords/frs_public2/frs_rest_services.get_facilities?'
    id <- ''
  }
  fullurl <- paste0(baseurl, id, ...)
  return(fullurl)
  #################################################################### #
  # API info  ####
  # 
  # browseURL('https://www.epa.gov/frs/frs-rest-services')
  # browseURL('https://www.epa.gov/frs/frs-rest-services#ex1') # examples are shown   
  # browseURL('https://www.epa.gov/frs/frs-rest-services#appendixa') 
  #
  # FRS lookup API's get_facilities
  #   (to get lat lon by facility's registry ID or program/system ID):
  #
  # # Public 
  #  # browseURL('https://frs-public.epa.gov/ords/frs_public2/frs_rest_services.get_facilities?pgm_sys_id=VA0088986')
  #  # browseURL('https://frs-public.epa.gov/ords/frs_public2/frs_rest_services.get_facilities?registry_id=110010912496')
  #  #
  # # Internal use?? API URL for internal use at EPA appears to be different than public one?:
  #  # browseURL('https://ofmpub.epa.gov/frs_public2/frs_rest_services.get_facilities?registry_id=110005250682')

}
#################################################################### #


#' Helper function to query FRS API to find 1 EPA facility
#' 
#' @description  Uses the Facility Registry Service (FRS) API to find a site   
#'   by registry ID or program ID.
#'   This uses an API to find sites, but it is faster to look in a table
#'   if that FRS dataset is already loaded in an app, for example.
#' @param id one character string that must be a registry ID (default) or program ID
#' @param type either frs (default) which means registry_id or
#'   program which means pgm_sys_id
#' @param ... passed through to url_by_id()
#' @seealso locate_by_id() and url_by_id() 
#'
#' @return a 1 row data.frame, columns as returned by the API, 
#'   but lat lon instead of Latitude83 Longitude83
#'   ("RegistryId", "FacilityName", "LocationAddress", "CityName", 
#'   "CountyName", "StateAbbr", "ZipCode", "FIPSCode", 
#'   "lat", "lon")
#'   
#' @keywords internal
#' @export
#'
locate_by_id1 <- function(id, type='frs', ...) {
  
  # functions to QUERY FRS API by registry id or by program system id
  if (length(id) > 1) {stop('only one id at a time')}
  if (length(type) > 1) {stop('only one type at a time')}
  if (is.null(id)) id <- NA
  if (is.na(id) | length(id) == 0 | nchar(id) == 0 | (grepl('[^[:alnum:]]', id))) {
    warning('bad id - function will return NA values')
    id <- NA # If "NA" API returns results for some default site it looks like, 
    # and then later below we can replace those results with NA values
  } # **********
  # if (!(type %in% c('frs', 'program'))) {stop('type of ID specified must be "frs" or "program",
  # with program ids like "VA0088986" or frs ids like "110015787683" ')}
  url_this_request <- url_by_id(id, type = type, ...)
  x <- try(httr::GET(url_this_request))
  x <- XML::xmlTreeParse(x)$doc$children$Results
  if (length(x) == 0) {
    # no results, probably bad ID specified that was not caught by simple error checking
    id <- NA
    x <- try(httr::GET(url_by_id(id, type = type)))
    x <- XML::xmlTreeParse(x)$doc$children$Results
  }
  x <- XML::xmlValue(as.vector(x$children$FRSFacility$children))
  if (is.list(x)) {
    x <- unlist(x) # looks like malformed xml from one example query caused a problem this fixes
  }
  x <- as.data.frame(rbind(x))
  if (is.na(id)) x[,] <- NA # replace each value w NA, to return correct format but all values NA
  x$lat <- as.numeric(x$Latitude83); x$Latitude83 <- NULL
  x$lon <- as.numeric(x$Longitude83); x$Longitude83 <- NULL
  return(x)
  #################################################################### #
}
