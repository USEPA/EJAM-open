################################################### #################################################### #

# LIST OF FUNCTIONS HERE ####
# 
#   see outline via ctrl-shift-O
#   see also URL_FUNCTIONS_part1.R



######################################################################### #

#' URL functions - Create URLs in columns, for EJAM
#' 
#' Could start to use this in server and ejamit(), and already used in table_xls_format() 
#' @details used in [table_xls_format()] 
#' 
#' @param lat vector of latitudes
#' @param lon vector of longitudes
#' 
#' @param radius vector of values for radius in miles
#' 
#' @param regid optional vector of FRS registry IDs if available to use to create links
#'   to detailed ECHO facility reports
#'   
#' @param fips vector of FIPS codes if relevant, to use instead of lat lon,
#'   Passed to [url_ejscreen_report()] as areaid
#'   Note that nearly half of all county fips codes are impossible to distinguish from 
#'   5-digit zipcodes because the same numbers are used for both purposes.
#'   
#' @param wherestr optional because inferred from fips if provided.
#'   Passed to [url_ejscreenmap()] and can be name of city, county, state like
#'   from fips2name(201090), or "new rochelle, ny" or "AK"
#'   or even a zip code, but NOT a fips code! (for FIPS, use the fips parameter instead).
#'   Note that nearly half of all county fips codes are impossible to distinguish from 
#'   5-digit zipcodes because the same numbers are used for both purposes.
#' @param namestr passed to [url_ejscreen_report()]
#' 
#' @param shapefile not implemented
#' 
#' @param as_html logical, optional.
#'   passed to [url_ejscreen_report()] and [url_ejscreenmap()]
#' @param ... passed to [url_ejscreen_report()] such as areaid="0201090" for a city fips
#' 
#' @seealso  [url_ejscreen_report()] [url_ejscreenmap()] [url_echo_facility_webpage()] [urls_clusters_and_sort_cols()]
#' @return list of data.frames to append to the list of data.frames created by
#'   [ejamit()] or [doaggregate()],
#'   
#'  `list(results_bysite = results_bysite, `
#'  `    results_overall = results_overall,`
#'  `      newcolnames=newcolnames)`
#'
#' @export
#' @keywords internal
#'
url_4table <- function(lat, lon, radius=NULL, regid = NULL, 
                       fips, wherestr = "", namestr = NULL, 
                       shapefile = NULL,
                       as_html = TRUE, ...) {
  
  # add error checking***
  
  if (!missing(fips) & !is.null(fips) & !all(is.na(fips))) {
    if (missing(wherestr)) {
    wherestr <- fips2name(fips)
    }
    if (!missing(lat) & !is.null(lat) & !all(is.na(fips))) {warning('should provide lat,lon or fips but both were provided')}
    lat = NULL
    lon = NULL
  }
  
  # Also could add other links such as these:
  #   url_frs_report()
  #   url_enviromapper()
  #   url_envirofacts_data()  ?
  # url_countyhealthrankings() ?
  
  if (!is.null(regid)) {
    echolink <- url_echo_facility_webpage(regid, as_html = as_html)
  } else {
    echolink <- rep(NA, max(NROW(lat), NROW(fips))) # server used 'N/A' instead of NA -- which do we want to use?
  }
  
  newcolnames <- c(
    # "EJScreen Report",
    # "EJScreen Map",
    # "ACS Report",
    "ECHO Report"
  )
  
  results_bysite <- data.table(
   # `EJScreen Report` = ,#url_ejscreen_report(lat = lat, lon = lon, radius = radius, as_html = as_html, 
                              #              areaid = fips, namestr = namestr, 
                              #              shapefile = shapefile, interactiveprompt = FALSE, ...), # linktext = 
    
  #  `EJScreen Map`    = #url_ejscreenmap(    lat = lat, lon = lon,                  as_html = as_html, 
                              #              wherestr = wherestr,              
                              #              shapefile = shapefile),  # linktext =   would need to be different than for report
   #`ACS Report`      = 'N/A',#url_ejscreen_acs_report(lat = lat, lon = lon, radius = radius, as_html = as_html),
   `ECHO Report` = echolink
  )
  results_overall <- data.table(
    # `EJScreen Report` = NA,
    # `EJScreen Map`    = NA,
    # `ACS Report` = NA,
    `ECHO Report`     = NA
  )
  
  # setcolorder(out$results_bysite,  neworder = newcolnames)
  # setcolorder(out$results_overall, neworder = newcolnames)
  # out$longnames <- c(newcolnames, out$longnames)
  
  return(list(
    results_bysite  = results_bysite,
    results_overall = results_overall,
    newcolnames = newcolnames
  ))
}
######################################################################### #


#' URL functions - Get URLs of useful report(s) on Counties from countyhealthrankings.org
#'
#' @param fips vector of fips codes of counties, 5 characters each, like "10003"
#' @param year e.g., 2025
#'
#' @return vector of URLs
#' @examples 
#'  url_countyhealthrankings(fips_counties_from_state_abbrev("DE"))
#'  # browseURL(url_countyhealthrankings(fips_counties_from_state_abbrev("DE"))[1])
#' 
#' @export
#'
url_countyhealthrankings <- function(fips, year = 2025) {
  
  if (missing(year) && year != as.numeric(substr(Sys.Date(), 1, 4))) {
    warning("default year is ", year, " but newer data might be available")
  }
  if (missing(fips)) {
    return("https://www.countyhealthrankings.org")
  }
  statename  <- tolower(EJAM::fips2statename( fips))
  countyname <- tolower(EJAM::fips2countyname(fips, includestate = ""))
  countyname <- trimws(gsub(" county", "", countyname))
  countyname <- gsub(" ", "-", countyname)
  # https://www.countyhealthrankings.org/explore-health-rankings/maryland/montgomery?year=2023
  baseurl <- "https://www.countyhealthrankings.org/explore-health-rankings/"
  url <- paste0(baseurl, statename, "/", countyname, "?year=", year)
  return(url)
}
######################################################################### #


#' URL functions - url_naics.com - Get URL for page with info about industry sectors by text query term
#' 
#' See (https://naics.com) for more information on NAICS codes
#' 
#' @param query string query term like "gasoline" or "copper smelting"
#' @param as_html Whether to return as just the urls or as html hyperlinks to use in a DT::datatable() for example
#' @param linktext used as text for hyperlinks, if supplied and as_html=TRUE
#' @return URL as string
#' 
#' @export
#' 
url_naics.com <- function(query, as_html=FALSE, linktext) {
  
  query <- gsub(" ", "+", query)
  urlout = paste0("https://www.naics.com/code-search/?trms=", query, "&v=2017&styp=naics")
  if (as_html) {
    if (missing(linktext)) {linktext <- query}  #   paste0("EJScreen Map ", 1:length(lon)) 
    urlout <- url_linkify(urlout, text = linktext)
  }   
  return(urlout)
}
######################################################################### #


#' URL functions - url_get_via_url - helper function work in progress: GET json via url of ejscreen ejquery map services
#'
#' @param url the url for an EJScreen ejquery request
#'
#' @return json
#' 
#' @export
#' @keywords internal
#'
url_get_via_url <- function(url) { 
  
  # function to GET json via url of ejscreen ejquery map services ### #
  
  x <- httr::GET(url)
  if (x$status_code == 400) {
    warning('Query failed with status code 400: ', url)
  }
  if (x$status_code == 404) {
    warning('Query failed with status code 404, possibly requesting too many locations at once: ', url)
  }
  x <- try(rawToChar(x$content))
  x <- try(jsonlite::fromJSON(x))
  alldata <- x$features$attributes
  # print(str(x)) # nothing else useful except possible x$features$geometry data.frame
  return(alldata)
}
######################################################################### #


#' URL functions - url_get_eparest_chunked_by_id - experimental/ work in progress: in chunks, get ACS data or Block weights nearby via EPA API
#'
#' @param objectIds see API
#' @param chunksize see API
#' @param ... passed to url_getacs_epaquery()
#'
#' @return a table
#' 
#' @export
#' @keywords internal
#'
url_get_eparest_chunked_by_id <- function(objectIds, chunksize=200, ...) {
  
  #  to get ACS data or Block weights nearby from EPA server via API  ###   #
  
  ############################################################## #
  # generic function to break request into chunks ####
  ############################################################## #
  
  if (missing(objectIds)) {
    warning('this only works for objectIds so far, breaking up into groups of 200 or so objectIds.')
    return(NULL)
    # could write it to check if >1000 would be returned and then request in chunks in that case.
  }
  x <- list()
  n <- length(objectIds)
  extrachunk <- ifelse((n %/% chunksize) * chunksize == n, 0, 1) 
  
  for (chunk in 1:(extrachunk + (n %/% chunksize))) {
    istart <- 1 + (chunk - 1) * chunksize
    iend <- chunksize + istart - 1
    iend <- min(iend, n)
    idchunk <- objectIds[istart:iend]
    
    x[[chunk]] <- url_getacs_epaquery(objectIds = idchunk,  ...)
  }
  return(do.call(rbind, x))
}
############################################################## #  ############################################################## #



#' URL functions - url_getacs_epaquery_chunked - experimental/ work in progress: in chunks, get ACS data via EPA API
#'
#' @param servicenumber see API
#' @param objectIds see API
#' @param outFields see API
#' @param returnGeometry  see API
#' @param justurl  see API
#' @param chunksize eg 200 for chunks of 200 each request
#' @param ... passed to url_getacs_epaquery()
#'
#' @return table
#' 
#' @export
#' @keywords internal
#'
#' @examples \donttest{
#'  # x <- list() # chunked chunks. best not to ask for all these:
#'  # x[[1]] <- url_getacs_epaquery_chunked(   1:1000, chunksize = 100)
#'  # x[[2]] <- url_getacs_epaquery_chunked(1001:5000, chunksize = 100)
#'  # xall <- do.call(rbind, x)
#'  } 
url_getacs_epaquery_chunked <- function(objectIds=1:3, 
                                        servicenumber=7,
                                        outFields=NULL, 
                                        returnGeometry=FALSE, 
                                        justurl=FALSE, 
                                        chunksize=200, ...) {
  
  ############################################################## #
  # generic function to break request into chunks 
  ############################################################## #
  
  if (missing(objectIds)) {
    warning('this only works for objectIds so far, breaking up into groups of 200 or so objectIds.')
    return(NULL)
    # could write it to check if >1000 would be returned and then request in chunks in that case.
  }
  
  # warning('check if still has a bug to fix where it duplicates the last row of each chunk')
  x <- list()
  n <- length(objectIds)
  extrachunk <- ifelse((n %/% chunksize) * chunksize == n, 0, 1) 
  
  for (chunk in 1:(extrachunk + (n %/% chunksize))) {
    istart <- 1 + (chunk - 1) * chunksize
    iend <- chunksize + istart - 1
    iend <- min(iend, n)
    idchunk <- objectIds[istart:iend]
    # # TESTING:
    #   x[[chunk]] <- data.frame(id=idchunk, dat=NA)
    #   print(idchunk); print(x) # cumulative so far
    
    x[[chunk]] <- url_getacs_epaquery(objectIds = idchunk, outFields = outFields, servicenumber = servicenumber, ...)
  }
  return(do.call(rbind, x))  
} 
############################################################## ############################################################### #



#' URL functions - url_getacs_epaquery - experimental/ work in progress: get ACS data via EPA API (for <200 places)
#'
#' @description  uses ACS2019 rest services ejscreen ejquery MapServer 7
#' 
#'   Documentation of format and examples of input parameters:
#'   
#'   <https://geopub.epa.gov/arcgis/sdk/rest/index.html#/Query_Map_Service_Layer/02ss0000000r000000/>
#' 
#' @param objectIds see API
#' @param servicenumber see API
#' @param outFields see API. eg "STCNTRBG","TOTALPOP","PCT_HISP",
#' @param returnGeometry see API
#' @param justurl if TRUE, returns url instead of default making API request
#' @param ... passed to url_getacs_epaquery_chunked()
#' @examples  url_getacs_epaquery(justurl=TRUE)
#' 
#' @return table
#' 
#' @export
#' @keywords internal
#'
url_getacs_epaquery <- function(objectIds=1:3, 
                                servicenumber=7,
                                outFields=NULL, 
                                returnGeometry=FALSE, 
                                justurl=FALSE, 
                                ...) {
  
  # Documentation of format and examples of input parameters:
  # https://geopub.epa.gov/arcgis/sdk/rest/index.html#/Query_Map_Service_Layer/02ss0000000r000000/
  
  print('this uses ACS2019 rest services ejscreen ejquery MapServer 7')
  
  
  # if (length(objectIds) < 1 | !all(is.numeric(objectIds))) {stop('no objectIds specified or some are not numbers')}
  if (any(objectIds == '*')) {
    warning('Trying to specify all objectIds will not work')
    return(NULL)
  }
  if (length(objectIds) > 200) {
    warning('seems to crash if more than about 211 requested per query - chunked version not yet tested')
    
    # return(url_get_eparest_chunked_by_id(objectIds=objectIds, 
    #                                   servicenumber=servicenumber,
    #                                   outFields=outFields, 
    #                                   returnGeometry=returnGeometry, 
    #                                   justurl=justurl, 
    #                                   ...))
    return(url_getacs_epaquery_chunked(objectIds = objectIds, 
                                       servicenumber = servicenumber,
                                       outFields = outFields, 
                                       returnGeometry = returnGeometry, 
                                       justurl = justurl, 
                                       ...))
  }
  
  # use bestvars default outFields if unspecified ### #
  if (is.null(outFields)) {
    bestvars <- c( ## bestvars ### #
      # outFields
      "OBJECTID",  # unique id 1 onwards
      "STCNTRBG",  # blockgroup fips
      "AREALAND", "AREAWATER",
      
      "TOTALPOP",   # population count 
      
      "LOWINC", "POV_UNIVERSE_FRT", "PCT_LOWINC",
      
      "HH_BPOV", "HSHOLDS", "PCT_HH_BPOV",
      
      "EDU_LTHS", "EDU_UNIVERSE", "PCT_EDU_LTHS",
      
      "LINGISO", "PCT_LINGISO",
      
      "EMP_STAT_UNEMPLOYED",
      "EMP_STAT_UNIVERSE",
      "PCT_EMP_STAT_UNEMPLOYED",
      
      "AGE_LT5",      "PCT_AGE_LT5",
      "AGE_GT64",     "PCT_AGE_GT64",
      
      "NUM_MINORITY", "PCT_MINORITY",
      
      "WHITE",        "PCT_WHITE",
      "BLACK",        "PCT_BLACK",
      "HISP" ,        "PCT_HISP",
      "ASIAN",        "PCT_ASIAN",
      "AMERIND",      "PCT_AMERIND",
      "HAWPAC",       "PCT_HAWPAC",
      "OTHER_RACE",   "PCT_OTHER_RACE",
      "TWOMORE",      "PCT_TWOMORE",
      "NHWHITE",      "PCT_NHWHITE",
      "NHBLACK",      "PCT_NHBLACK",
      "NHASIAN",      "PCT_NHASIAN",
      "NHAMERIND",    "PCT_NHAMERIND",
      "NHHAWPAC",     "PCT_NHHAWPAC",
      "NHOTHER_RACE", "PCT_NHOTHER_RACE",
      "NHTWOMORE",    "PCT_NHTWOMORE",
      
      "HOME_PRE60", "HSUNITS", "PCT_HOME_PRE60"
    )
    outFields <- bestvars
  } # use default fields if none specified
  # reformat the parameters - now done by url_... function 
  # outFields <- paste(outFields, collapse = ',') # if a vector of values was provided, collapse them into one comma-separated string
  # objectIds <- paste(objectIds, collapse = ',') # if a vector of values was provided, collapse them into one comma-separated string
  
  ################################################################### #
  # assemble URL 
  # url_to_get_ACS2019_rest_services_ejscreen_ejquery_MapServer_7()
  url_to_use <- url_to_get_ACS2019_rest_services_ejscreen_ejquery_MapServer_7(
    # servicenumber=servicenumber, 
    objectIds = objectIds,
    returnGeometry = returnGeometry,
    outFields = outFields,         # make same name?
    ...)
  ################################################################### #
  
  # call GET function (submit the query 
  
  if (justurl) {return(url_to_use)}
  return(url_get_via_url(url_to_use))
}
############################################################## #  ############################################################## #




#' URL functions - DRAFT FRAGMENTS OF CODE - url_to_any_rest_services_ejscreen_ejquery
#'
#' @details 
#'  # generic function wrapping ejscreen/ejquery API calls ####
#'  
#'  Disadvantage of this generic approach is it does not help you by showing
#'   a list of parameters, since those are specific to the service like 7 vs 71. 
#'  
#'  see links to documentation on using APIs here: 
#'  
#'  EJAM/dev/intersect-distance/arcgis/ArcGIS REST API basics.txt
#'  
#' @param servicenumber na
#'
#' @return tbd
#' @examples # na
#' 
#' @noRd
#' @keywords internal
#'
url_to_any_rest_services_ejscreen_ejquery <- function(servicenumber=7) {
  
  ####################################### #
  ## notes - examples 
  if (1 == 0) {
    url_getacs_epaquery(  objectIds = 1:4,                 outFields = 'STCNTRBG', justurl = TRUE)
    t(url_getacs_epaquery(objectIds = sample(1:220000,2),  outFields = '*'))
    t(url_getacs_epaquery(objectIds = sample(1:220000,2)))
    url_getacs_epaquery(  objectIds = sample(1:220000,10), outFields = c('STCNTRBG', 'STATE', 'COUNTY', 'TRACT', 'BLKGRP'), justurl = FALSE)
    y <- url_get_via_url(url_to_any_rest_services_ejscreen_ejquery(         servicenumber = 71))
    x <- url_get_via_url(url_to_get_nearby_blocks_rest_services_ejscreen_ejquery_MapServer_71(lat = 30.494982, lon = -91.132107, miles = 1))
    z <- url_get_via_url(url_to_get_ACS2019_rest_services_ejscreen_ejquery_MapServer_7())
  }
  ####################################### #
  
  params_with_NULL   <- c(as.list(environment()))
  params_with_NULL <- subset(params_with_NULL, names(params_with_NULL) != 'servicenumber')
  params_with_NULL <- lapply(params_with_NULL, function(x) paste(x, collapse = ','))
  params_text_with_NULL <- paste(paste0(names(params_with_NULL), '=', params_with_NULL), collapse = '&')
  
  baseurl <- paste0(
    'https://geopub.epa.gov/arcgis/rest/services/ejscreen/ejquery/MapServer/',
    servicenumber,  # 7 is ACS 2019 block groups
    '/query?'
  )
  queryurl <- paste0(
    baseurl,  
    params_text_with_NULL
  )
  return(queryurl)
}
################################################################################# # 



#' URL functions - DRAFT FRAGMENTS OF AN URL FUNCTION url_to_get_ACS2019_rest_services_ejscreen_ejquery_MapServer_7
#'
#' @param objectIds na
#' @param sqlFormat na
#' @param text na
#' @param where na
#' @param havingClause na
#' @param outFields na
#' @param orderByFields na
#' @param groupByFieldsForStatistics na
#' @param outStatistics na
#' @param f na
#' @param returnGeometry na
#' @param returnIdsOnly na
#' @param returnCountOnly na
#' @param returnExtentOnly na
#' @param returnDistinctValues na 
#' @param returnTrueCurves na
#' @param returnZ na
#' @param returnM na
#' @param geometry na
#' @param geometryType na
#' @param featureEncoding na
#' @param spatialRel na
#' @param units na
#' @param distance na 
#' @param inSR na
#' @param outSR na
#' @param relationParam na
#' @param geometryPrecision na
#' @param gdbVersion na
#' @param datumTransformation na
#' @param parameterValues na
#' @param rangeValues na
#' @param quantizationParameters na
#' @param maxAllowableOffset na
#' @param resultOffset na
#' @param resultRecordCount na
#' @param historicMoment na
#' @param time na
#' @param timeRelation na
#' 
#' @examples #tbd
#'
#' @return tbd
#' 
#' @noRd
#' 
url_to_get_ACS2019_rest_services_ejscreen_ejquery_MapServer_7 <- function(
    
  objectIds=NULL, 
  sqlFormat='none', 
  text=NULL, 
  where=NULL, 
  havingClause=NULL, 
  
  # select what stats
  
  outFields=NULL, 
  orderByFields=NULL, 
  groupByFieldsForStatistics=NULL, 
  outStatistics=NULL, 
  f='pjson',
  returnGeometry='true', 
  returnIdsOnly='false', 
  returnCountOnly='false', 
  returnExtentOnly='false', 
  returnDistinctValues='false', 
  returnTrueCurves='false', 
  returnZ='false', 
  returnM='false', 
  
  # geoprocessing like buffer/intersect, etc.
  
  geometry=NULL, 
  geometryType='esriGeometryEnvelope', 
  featureEncoding='esriDefault', 
  spatialRel='esriSpatialRelIntersects', 
  units='esriSRUnit_Foot', 
  distance=NULL, 
  inSR=NULL, 
  outSR=NULL, 
  relationParam=NULL, 
  geometryPrecision=NULL, 
  gdbVersion=NULL, 
  datumTransformation=NULL, 
  parameterValues=NULL, 
  rangeValues=NULL, 
  quantizationParameters=NULL, 
  maxAllowableOffset=NULL, 
  resultOffset=NULL, 
  resultRecordCount=NULL, 
  historicMoment=NULL, 
  time=NULL,
  timeRelation='esriTimeRelationOverlaps'
) {
  
  ## notes - examples 
  if (1 == 0) {
    url_getacs_epaquery(  objectIds = 1:4,                 outFields = 'STCNTRBG', justurl = TRUE)
    t(url_getacs_epaquery(objectIds = sample(1:220000,2),  outFields = '*'))
    t(url_getacs_epaquery(objectIds = sample(1:220000,2)))
    url_getacs_epaquery(  objectIds = sample(1:220000,10), outFields = c('STCNTRBG', 'STATE', 'COUNTY', 'TRACT', 'BLKGRP'), justurl = FALSE)
    y <- url_get_via_url(url_to_any_rest_services_ejscreen_ejquery(         servicenumber = 71))
    x <- url_get_via_url(url_to_get_nearby_blocks_rest_services_ejscreen_ejquery_MapServer_71(lat = 30.494982, lon = -91.132107, miles = 1))
    z <- url_get_via_url(url_to_get_ACS2019_rest_services_ejscreen_ejquery_MapServer_7())
  }
  
  # Documentation of format and examples of input parameters:
  # https://geopub.epa.gov/arcgis/sdk/rest/index.html#/Query_Map_Service_Layer/02ss0000000r000000/ 
  
  #  useful map services to query ####
  
  #https://geopub.epa.gov/arcgis/sdk/rest/index.html#/Query_Map_Service_Layer/02ss0000000r000000/
  # https://geopub.epa.gov/arcgis/sdk/rest/index.html#/Query_Map_Service_Layer/02ss0000000r000000/
  # https://geopub.epa.gov/arcgis/sdk/rest/index.html#/Map_Service/02ss0000006v000000/
  # 'https://geopub.epa.gov/arcgis/rest/services/ejscreen/ejquery/MapServer/'
  # MaxRecordCount: 1000
  # https://geopub.epa.gov/arcgis/rest/services/ejscreen/ejquery/MapServer/7/2?f=pjson 
  
  
  # ASSEMBLE URL BY PUTTING ALL THE PARAMETERS INTO ONE LONG STRING IN CORRECT FORMAT:
  # params_with_NULL <- c(as.list(environment()), list(...))  # if ... was among function parameter options
  params_with_NULL   <- c(as.list(environment()))
  params_with_NULL <- lapply(params_with_NULL, function(x) paste(x, collapse = ','))
  params_text_with_NULL <- paste(paste0(names(params_with_NULL), '=', params_with_NULL), collapse = '&')
  
  baseurl <- paste0(
    'https://geopub.epa.gov/arcgis/rest/services/ejscreen/ejquery/MapServer/',
    7,  # servicenumber 7 is ACS 2019 block groups
    '/query?'
  )
  queryurl <- paste0(
    baseurl,  
    params_text_with_NULL
  )
  return(queryurl)  
}
############################################################## #  ############################################################## #


#' URL functions - DRAFT FRAGMENTS OF CODE - url_to_get_nearby_blocks_rest_services_ejscreen_ejquery_MapServer_71
#'
#' @param lat na
#' @param lon na
#' @param miles na
#' @param outFields na
#' @param returnCountOnly na
#' @examples #na
#' @return na
#' 
#' @noRd
#'
url_to_get_nearby_blocks_rest_services_ejscreen_ejquery_MapServer_71 <- function(
    lat, lon, miles, 
    outFields='GEOID10,OBJECTID,POP_WEIGHT', returnCountOnly='false') {
  
  # function for service 71, nearby blockweights 
  ## notes - examples 
  if (1 == 0) {
    url_getacs_epaquery(  objectIds = 1:4,                 outFields = 'STCNTRBG', justurl = TRUE)
    t(url_getacs_epaquery(objectIds = sample(1:220000,2),  outFields = '*'))
    t(url_getacs_epaquery(objectIds = sample(1:220000,2)))
    url_getacs_epaquery(  objectIds = sample(1:220000,10), outFields = c('STCNTRBG', 'STATE', 'COUNTY', 'TRACT', 'BLKGRP'), justurl = FALSE)
    y <- url_get_via_url(url_to_any_rest_services_ejscreen_ejquery(         servicenumber = 71))
    x <- url_get_via_url(url_to_get_nearby_blocks_rest_services_ejscreen_ejquery_MapServer_71(lat = 30.494982, lon = -91.132107, miles = 1))
    z <- url_get_via_url(url_to_get_ACS2019_rest_services_ejscreen_ejquery_MapServer_7())
  }
  # Documentation of format and examples of input parameters:
  # https://geopub.epa.gov/arcgis/sdk/rest/index.html#/Query_Map_Service_Layer/02ss0000000r000000/
  
  
  
  baseurl <- "https://geopub.epa.gov/arcgis/rest/services/ejscreen/ejquery/MapServer/71/query?"
  params <- paste0( 
    'outFields=', outFields,
    '&geometry=', lon, '%2C', lat, #  -91.0211604%2C30.4848044',
    '&distance=', miles,
    '&returnCountOnly=', returnCountOnly, 
    '&where=',
    '&text=',
    '&objectIds=',
    '&time=',
    '&timeRelation=esriTimeRelationOverlaps',
    '&geometryType=esriGeometryPoint',
    '&inSR=',
    '&spatialRel=esriSpatialRelContains',
    '&units=esriSRUnit_StatuteMile',
    '&relationParam=',
    '&returnGeometry=false',
    '&returnTrueCurves=false',
    '&maxAllowableOffset=',
    '&geometryPrecision=',
    '&outSR=',
    '&havingClause=',
    '&returnIdsOnly=false',
    '&orderByFields=',
    '&groupByFieldsForStatistics=',
    '&outStatistics=',
    '&returnZ=false',
    '&returnM=false',
    '&gdbVersion=',
    '&historicMoment=',
    '&returnDistinctValues=false',
    '&resultOffset=',
    '&resultRecordCount=',
    '&returnExtentOnly=false',
    '&sqlFormat=none&datumTransformation=',
    '&parameterValues=',
    '&rangeValues=',
    '&quantizationParameters=',
    '&featureEncoding=esriDefault',
    '&f=pjson')
  url <- paste0(baseurl, params)
  return(url)
}
############################################################## #  ############################################################## #



#' URL functions - DRAFT FRAGMENTS OF CODE - url_bookmark_save
#' 
#' save bookmarked EJScreen session (map location and indicator)
#' @details 
#' WORK IN PROGRESS - NOT USED AS OF EARLY 2023. 
#' You can use this function to create and save a json file that is a bookmark 
#' for a specific place/ map view/ data layer in EJScreen. 
#' You can later pull up that exact map in EJScreen by launching EJScreen, 
#' clicking Tools, Save Session, Load from File.
#' 
#' ***Units are not lat lon: "spatialReference":{"latestWkid":3857,"wkid":102100}
#' 
#' Note: 
#' (1) The number of sessions that can be saved depends on the browser cache size. 
#' (2) Session files, if saved, are available from the default Downloads folder on your computer. 
#' (3) Users should exercise caution when saving sessions that may contain sensitive or confidential data.
#' 
#' @param ... passed to [url_bookmark_text()]
#' @param file path and name of .json file you want to save locally
#'
#' @return URL for 1 bookmarked EJScreen map location and variable displayed on map
#' 
#' @noRd
#'
url_bookmark_save <- function(..., file="ejscreenbookmark.json") {
  mytext <- url_bookmark_text(...)
  write(mytext, file = file)
  return(mytext)
  
  # example, at EJAM/inst/testdata/Sessions_Traffic in LA area.json
  # [{"extent":{"spatialReference":{"latestWkid":3857,"wkid":102100},"xmin":-13232599.178424664,"ymin":3970069.245971938,"xmax":-13085305.024919074,"ymax":4067373.5829790044},"basemap":"Streets","layers":[{"id":"digitizelayer","type":"graphics","title":"digitize graphics","visible":true,"graphics":[]},{"id":"ejindex_map","title":"Pollution and Sources","isDynamic":true,"layerType":"ejscreen","pctlevel":"nation","renderField":"B_PTRAF","renderIndex":4,"type":"map-image","url":"https://geopub.epa.gov/arcgis/rest/services/ejscreen/ejscreen_v2022_with_AS_CNMI_GU_VI/MapServer","visible":true,"opacity":0.5}],"graphics":[],"name":"Traffic in LA area"}]
  #
  # [{
  #   "extent":{"spatialReference":{"latestWkid":3857,"wkid":102100},"xmin":-13232599.178424664,"ymin":3970069.245971938,"xmax":-13085305.024919074,"ymax":4067373.5829790044},
  #   "basemap":"Streets",
  #   "layers":[
  #     {"id":"digitizelayer","type":"graphics","title":"digitize graphics","visible":true,"graphics":[]},
  #     {"id":"ejindex_map",
  #       "title":"Pollution and Sources",
  #       "isDynamic":true,
  #       "layerType":"ejscreen",
  #       "pctlevel":"nation",
  #       "renderField":"B_PTRAF",
  #       "renderIndex":4,
  #       "type":"map-image",
  #       "url":"https://geopub.epa.gov/arcgis/rest/services/ejscreen/ejscreen_v2022_with_AS_CNMI_GU_VI/MapServer",
  #       "visible":true,
  #       "opacity":0.5
  #     }
  #   ],
  #   "graphics":[],
  #   "name":"Traffic in LA area"
  # }]
  
}
############################################################## #  ############################################################## #


#' URL functions - DRAFT FRAGMENTS OF CODE - url_bookmark_text
#' 
#' URL for 1 bookmarked EJScreen session (map location and indicator)
#' @details 
#' WORK IN PROGRESS - NOT USED AS OF EARLY 2023. 
#' You can use this function to create and save a json file that is a bookmark 
#' for a specific place/ map view/ data layer in EJScreen. 
#' You can later pull up that exact map in EJScreen by launching EJScreen, 
#' clicking Tools, Save Session, Load from File.
#' 
#' Note: 
#' (1) The number of sessions that can be saved depends on the browser cache size. 
#' (2) Session files, if saved, are available from the default Downloads folder on your computer. 
#' (3) Users should exercise caution when saving sessions that may contain sensitive or confidential data.
#' 
#' @param x vector of approx topleft, bottomright longitudes in some units EJScreen uses? 
#'    Units are not lat lon: "spatialReference":{"latestWkid":3857,"wkid":102100}
#' @param y vector of approx topleft, bottomright latitudes in some units EJScreen uses? 
#'    Units are not lat lon: "spatialReference":{"latestWkid":3857,"wkid":102100}
#' @param name Your name for the map bookmark
#' @param title Your name for the map like Socioeconomic Indicators  or  Pollution and Sources
#' @param renderField name of variable shown on map, like B_UNEMPPCT for map color bins of percent unemployed
#'   or B_PTRAF for traffic indicator
#' @param pctlevel nation or state
#' @param xmin  calculated bounding box for map view
#' @param xmax  calculated bounding box for map view
#' @param ymin  calculated bounding box for map view
#' @param ymax  calculated bounding box for map view
#' @param urlrest Just use the default but it changes each year
#' @seealso [url_bookmark_save()]
#' @return URL for 1 bookmarked EJScreen map location and variable displayed on map
#'
#' @examples \donttest{
#'   url_bookmark_text()
#'   url_bookmark_save(
#'     x=c(-10173158.179197036, -10128824.702791695), 
#'     y=c(3548990.034736070,3579297.316451102), 
#'     file="./mysavedejscreensession1.json")
#'   }
#'
#' @noRd
#'
url_bookmark_text <- function(
    x=c(-13232599.178424664, -13085305.024919074),
    y=c(3970069.245971938, 4067373.5829790044),
    # x=c(-172.305626, -59.454062),  # if longitude, zoomed way out to corners of USA plus some
    # y=c(63.774548, 16.955558), # if latitude, zoomed way out to corners of USA plus some
    name="BookmarkedEJScreenMap",
    title="Socioeconomic Indicators", # Pollution and Sources
    renderField="B_UNEMPPCT",   # B_PTRAF
    pctlevel="nation",
    xmin=1.1*min(x), # >1 because it is negative longitude in USA
    xmax=0.9*min(x), # <1 because it is negative longitude in USA
    ymin=0.9*min(y),
    ymax=1.1*min(y),
    urlrest=paste0("https://geopub.epa.gov/arcgis/rest/services", 
                   "/ejscreen/ejscreen_v2022_with_AS_CNMI_GU_VI/MapServer")
) {
  
  yrinurl <- gsub(".*v20(..).*", "20\\1", urlrest)
  yrnow <- substr(Sys.time(),1,4)
  if (yrnow > yrinurl + 1) {warning("Check that URL in url_bookmark_text() is updated to the latest dataset of EJScreen.")}
  # example, at EJAM/inst/testdata/Sessions_Traffic in LA area.json
  # [{"extent":{"spatialReference":{"latestWkid":3857,"wkid":102100},"xmin":-13232599.178424664,"ymin":3970069.245971938,"xmax":-13085305.024919074,"ymax":4067373.5829790044},"basemap":"Streets","layers":[{"id":"digitizelayer","type":"graphics","title":"digitize graphics","visible":true,"graphics":[]},{"id":"ejindex_map","title":"Pollution and Sources","isDynamic":true,"layerType":"ejscreen","pctlevel":"nation","renderField":"B_PTRAF","renderIndex":4,"type":"map-image","url":"https://geopub.epa.gov/arcgis/rest/services/ejscreen/ejscreen_v2022_with_AS_CNMI_GU_VI/MapServer","visible":true,"opacity":0.5}],"graphics":[],"name":"Traffic in LA area"}]
  #
  # [{
  #   "extent":{"spatialReference":{"latestWkid":3857,"wkid":102100},"xmin":-13232599.178424664,"ymin":3970069.245971938,"xmax":-13085305.024919074,"ymax":4067373.5829790044},
  #   "basemap":"Streets",
  #   "layers":[
  #     {"id":"digitizelayer","type":"graphics","title":"digitize graphics","visible":true,"graphics":[]},
  #     {"id":"ejindex_map",
  #       "title":"Pollution and Sources",
  #       "isDynamic":true,
  #       "layerType":"ejscreen",
  #       "pctlevel":"nation",
  #       "renderField":"B_PTRAF",
  #       "renderIndex":4,
  #       "type":"map-image",
  #       "url":"https://geopub.epa.gov/arcgis/rest/services/ejscreen/ejscreen_v2022_with_AS_CNMI_GU_VI/MapServer",
  #       "visible":true,
  #       "opacity":0.5
  #     }
  #   ],
  #   "graphics":[],
  #   "name":"Traffic in LA area"
  # }]
  
  # old urlrest was         "https://v18ovhrtay722.aa.ad.epa.gov/arcgis/rest/services/ejscreen/ejscreen_v2021/MapServer"  
  
  
  
  urltext <- paste0(
    '[{"extent":{"spatialReference":{"latestWkid":3857,"wkid":102100},',
    
    '"xmin":',
    xmin,                   ###########   PARAMETER ################ #### #-10173158.179197036, ##################### #
    ',"ymin":',
    ymin,                   ###########   PARAMETER ################ ##### #3548990.0347360703, ##################### #
    ',"xmax":',
    xmax,                   ###########   PARAMETER ################ ##### #-10128824.702791695, ##################### #
    ',"ymax":',
    ymax,                   ###########   PARAMETER ################ ##### #3579297.316451102, ##################### #
    
    '},"basemap":"Streets","layers":[{"id":"digitizelayer","type":"graphics","title":"digitize graphics","visible":true,"graphics":[]},{"id":',
    '"', 
    'ejindex_map',  ###########  ????????????  
    '",',   
    '"title":"',
    title,                     ###########   PARAMETER ################ #
    '",',
    '"isDynamic":true,"layerType":',
    '"',
    'ejscreen',
    '",',
    '"pctlevel":"',
    pctlevel,                      ###########   PARAMETER ################ #
    '",',
    '"renderField":"',
    renderField,                  ###########   PARAMETER ################ #
    '",',
    '"renderIndex":4,"type":"map-image",',
    '"url":"',
    urlrest,                    ###########   PARAMETER ################ #
    '",',
    '"visible":true,"opacity":0.5}],"graphics":[],',
    '"name":"',
    name,
    '"',
    '}]'
  )
  return(urltext)
}
############################################################## #  ############################################################## #

