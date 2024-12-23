################################################### #################################################### #

# LIST OF FUNCTIONS HERE ####
# 
#   see outline via ctrl-shift-O
#   see also URL_FUNCTIONS_part2.R


# ECHO reports on facilities:
# [url_echo_facility_webpage()]
# <https://echo.epa.gov/tools/web-services>
# browseURL("https://echo.epa.gov/tools/web-services")
# browseURL("https://echo.epa.gov/detailed-facility-report?fid=110068700043#")
# paste0("https://echo.epa.gov/detailed-facility-report?fid=", regid, "#")

# FRS reports on facilities: 
# [url_frs_report()]
# see also  https://www.epa.gov/frs/frs-rest-services  or https://www.epa.gov/frs 
# browseURL("https://www.epa.gov/frs/frs-physical-data-model")
# browseURL("https://frs-public.epa.gov/ords/frs_public2/fii_query_detail.disp_program_facility?p_registry_id=110035807259")
# browseURL("https://www.epa.gov/frs/frs-query#industrial naics")

# EnviroMapper app webpage:
# [url_enviromapper()]

# Envirofacts API, data on facilities:
# [url_envirofacts_data()]
# browseURL("https://www.epa.gov/enviro/web-services")
# browseURL("https://www.epa.gov/enviro/envirofacts-data-service-api")
# <https://www.epa.gov/enviro/web-services> and
# <https://www.epa.gov/enviro/envirofacts-data-service-api>

# EJScreen report on a location

# [url_ejscreen_report()] or [url_ejscreen_acs_report()]
################################################### #################################################### #



#' Add Links to ejscreenapi output, to launch EJScreen report for given point(s)
#' 
#' @description Add or update, and reorder, columns with results
#' @details Was used for [ejscreenapi_plus()] not for [ejamit()], to create weblinks to maps, for output table.
#' 
#' 1. Adds weblinks in column.
#' 2. Adds a column to flag sites that are close to other sites, and 
#' 3. Puts certain columns first.
#'   
#' @param results_table from [ejscreenapi()] function for example
#' @seealso [url_4table()] for the EJAM version. [url_ejscreenmap()] [distance_near_eachother()]
#' @return the input table but with extra columns
#' 
#' @keywords internal
#'
urls_clusters_and_sort_cols <- function(results_table) {
  
  if (is.data.table(results_table)) {
    setDF(results_table)
    on.exit(setDT(results_table))
  }
  ########################################### #  
  # Add columns with hyperlinks to site reports
  # 
  ## 1. EJSCREEN REPORT URL = pdfurl ####
  #
  ## Fix existing link to pdf-like report 
  # to make URL clickable in table, move to near 1st column, 
  # NOTE: browser can print that report to pdf with margins = c(0.3, 0.3, 0.3, 1.75) # Left Top Right Bottom

  if ("areaid" %in% names(results_table)) {
    areaid   <- results_table$areaid
  } else {
    lon = results_table$lon
    lat = results_table$lat
    areaid   <- ""
  }
  if ("areatype" %in% names(results_table)) {
    areatype <- results_table$areatype
  } else {
    areatype <- ""
  }
  if ("namestr" %in% names(results_table)) {
    namestr  <- results_table$namestr
  } else {
    namestr  <- ""
  }
  pdfurl <- url_ejscreen_report(lon = lon, lat = lat, radius = results_table$distance, 
                                areaid = areaid, areatype = areatype, namestr = namestr,
                                as_html = FALSE, linktext = "EJScreen Report")
  encodedlink <- URLencode( pdfurl)
  pdfurl <- paste0('<a href=\"', encodedlink, '\", target=\"_blank\">EJScreen Report ', rownames(results_table), '</a>')
  # (but does not work like that for csv/excel download)
  if ("pdfurl" %in% names(results_table) ) results_table$pdfurl <- NULL # gets recreated later below
  
  ## 2. EJSCREEN MAP URL = mapurl ####
  if (!all(areaid == '')) {
     mapurl <- url_ejscreenmap(wherestr = fips2name(fips) )  # e.g.,  "https://ejscreen.epa.gov/mapper/index.html?wherestr=35.3827475,-86.2464592"
  } else {
    mapurl <- url_ejscreenmap(lat = lat, lon = lon  )  # e.g.,  "https://ejscreen.epa.gov/mapper/index.html?wherestr=35.3827475,-86.2464592"
  }
 
  mapurl  <- paste0('<a href=\"', mapurl, '\", target=\"_blank\">EJScreen Map ', rownames(results_table), '</a>')
  # (but does not work like that for csv/excel download)
  
  ## 3. ACS REPORT URL = acsurl or EJScreenACS  - NOW OBSOLETE IN 7/2023 ##  ##
  # 
  # acsurl <- url_ejscreen_acs_report(lat = results_table$lat, lon = results_table$lon, radius = results_table$distance)   # e.g.,  "https://ejscreen.epa.gov/mapper/demogreportpdf.aspx?feattype=point&radius=3&coords=-77.029851,38.895902"  
  # acsurl  <- paste0('<a href=\"', acsurl, '\", target=\"_blank\">EJScreen ACS report ', rownames(results_table), '</a>')
  # (but does not work like that for csv/excel download)
  
  ########################################### #   
  # Add column to flag sites that are near each other ####
  #
  # want this to reflect radius in this data run, not whatever user may have just changed it to for the next run, so do not use is_clustered()
  if (!is.null(lat)) {
  results_table$overlaps_another <- distance_near_eachother(lon = lon, lat = lat, 
                                                   distance = 2 * results_table$distance) # not radius_miles() !
  } else {
    results_table$overlaps_another <- NA
  }
  ########################################### #  
  # Re-order Columns ####  
  #
  # "id"  "RAW_D_MINOR" "RAW_D_INCOME"  "totalPop" "distance" etc. are the output names at this point
  #  something like  "siteid"  "sitename"  "lon" "lat" may be uploaded
  
  firstcols <- c('id', 'distance', 'overlaps_another', 'totalPop') # these after the input points;  then the rest of the outputs
  
  results_table <- data.frame(
    
    a = pdfurl, # 
    b = mapurl, #  
    
    results_table[ , firstcols], 
    results_table[, !(names(results_table) %in% firstcols)], 
    stringsAsFactors = FALSE)
  names(results_table) <- gsub("^a$", "EJScreen Report", names(results_table))
  names(results_table) <- gsub("^b$", "EJScreen Map", names(results_table))
  
  ########################################### #
  
  return(results_table)
}
########################################### #  ########################################### #


#' utility to make html link from URL
#' 
#' Convert URL to HTML link that opens in new tab
#' 
#' @param url string that is URL
#' @param text string that is label
#'
#' @return url_linkify('epa.gov','EPA') returns `"<a href=\"epa.gov\", target=\"_blank\">EPA</a>"`
#' 
#' @keywords internal
#' @export
#'
url_linkify <- function(url, text) {
  
  if (missing(text)) {text = gsub(pattern = "http[s]?://","",url)}
  paste0('<a href=\"', URLencode(url), '\", target=\"_blank\">', text, '</a>')
  
  # Consider instead using something like golem utility enurl()
  # 
  #   enurl <- function(url, text) {tags$a(href = url, text)}
} 
################################################### #################################################### #


# convert EJAM html versions of weblinks back to simple URLs
# in the output tables from ejamit or doaggregate

unlinkify = function(x) {
  
  unlinkify_column <- function(z) {gsub('.*https', 'https', gsub('=report.*', '=report', gsub('., target.*', '', as.vector(unlist(z))))) }
  if (NCOL(x) > 1) {
    fixed = lapply(x, unlinkify_column)
  } else {
    fixed = unlinkify_column(x)
  }
  if (is.data.table(x)) {return(as.data.table(fixed))}
  if (is.data.frame(x)) {return(data.frame(fixed))}
  return(fixed)
}
# test_vec = testoutput_ejamit_10pts_1miles$results_bysite$`EJScreen Report`
# test_df1 = as.data.frame(testoutput_ejamit_10pts_1miles$results_bysite[ , 1])
# test_df2 = as.data.frame(testoutput_ejamit_10pts_1miles$results_bysite[ , 1:2])
# test_dt1 = testoutput_ejamit_10pts_1miles$results_bysite[ , 1]
# test_dt2 = testoutput_ejamit_10pts_1miles$results_bysite[ , 1:2]
# 
# unlinkify(test_df1[1,1])
# unlinkify(test_vec); class(unlinkify(test_vec))
# unlinkify(test_df1); class(unlinkify(test_df1))
# unlinkify(test_dt1); class(unlinkify(test_dt1))
# unlinkify(test_df2); class(unlinkify(test_df2))
# unlinkify(test_dt2); class(unlinkify(test_dt2))  
################################################### #################################################### #

#' utility to prep URLs for being written to Excel
#' 
#' @param urls vector of urls such as from [url_ejscreen_report()]
#'
#' @param urltext The text to appear in Excel cells instead of just the URL showing
#' @details
#'   ## works best if using [openxlsx::writeData()] not [openxlsx::write.xlsx()]
#'   ## to write this column of urls to a worksheet
#'   lat <- c(30.977402, 32.515813); lon = c(-83.368997, -86.377325)
#'   radius <- 1
#'   urlejtest <- function(lat,lon,radius) {
#'   paste0('https://ejscreen.epa.gov/mapper/EJscreen_SOE_report.aspx',
#'     '?&geometry={\"spatialReference\":{\"wkid\":4326},\"x\":', 
#'     lon,  ',\"y\":', lat, '}&distance=', radius,'&unit=9035&areatype=&areaid=&namestr=&f=report')
#'   }
#'   urls <- urlejtest(lat, lon, radius)
#'   
#'   urlx <- url_xl_style(urls, urltext = paste0("Report ", 1:2))
#'   
#'   wb <- openxlsx::createWorkbook()
#'   openxlsx::addWorksheet(wb, sheetName = 'tab1')
#'   writeData(wb, sheet = 1, x = urlx, startCol = 1, startRow = 2)
#'   openxlsx::saveWorkbook(wb, file = 'test1.xlsx', overwrite = TRUE)
#'   
#'   # using just [write.xlsx()] is simpler but ignores the urltext param:
#'   openxlsx::write.xlsx(data.frame(lat = lat, lon = lon, urlx), file = 'test2.xlsx')
#'   
#' @keywords internal
#' @noRd
#' 
url_xl_style <- function(urls, urltext = urls) {
  
  x <- urls
  names(x) <- urltext
  class(x) <- 'hyperlink'
  return(x)
  
}
################################################### #################################################### #


#' Get URLs of EJScreen reports
#' 
#' Get URL(s) for EJScreen standard report on residents near given point(s)
#' 
#' @details Also see [ejscreenRESTbroker()] and [ejscreenapi1()]
#' 
#'   and (https://ejscreen.epa.gov/mapper/EJAPIinstructions.pdf)
#'   
#'   and (https://ejscreen.epa.gov/mapper/ejscreenapi1.html)
#'   
#' @param lat one or more latitudes (or a table with lat, lon columns, or filepath with that, or omit to interactively select file)
#' @param lon one or more longitudes (or omitted -- see lat parameter details)
#' @param radius miles radius
#' @param as_html Whether to return as just the urls or as html hyperlinks to use in a DT::datatable() for example
#' @param linktext used as text for hyperlinks, if supplied and as_html=TRUE
#' @param mobile If TRUE, provides URL for the mobile browser version, not desktop version
#' @param areatype passed as areatype= in API, inferred if not provided but areaid is provided
#' @param areaid fips codes if used,  passed as areaid=  in API, can be FIPS for blockgroups, tracts, counties.
#' @param namestr The character string text to show on the report as the name of the place
#' @param shapefile not implemented
#' @param wkid default is 4326 -WGS84 - World Geodetic System 1984, used in GPS - see (https://epsg.io/4326)
#' @param unit default is 9035 which means miles; for kilometers use 9036
#' @param f can be "report" or "pjson" or "json"
#' @param interactiveprompt passed to [sitepoints_from_any()]
#' @seealso  [url_ejscreen_report()]  [url_ejscreen_acs_report()]   [url_ejscreenmap()]
#'   [url_echo_facility_webpage()] [url_frs_report()]  [url_enviromapper()]  [url_envirofacts_data()]
#' @return URL(s)
#' 
#' @export
#'
url_ejscreen_report <- function(lat='', lon='', radius='', as_html=FALSE, linktext, mobile=FALSE,
                                areatype="", areaid = "", namestr = "", 
                                shapefile = NULL,  # would require POST not just a simple url-encoded GET API call?
                                wkid = 4326, unit = 9035, f = "report",
                                interactiveprompt = FALSE) {
  
  if (!any(areaid == "") && !any(is.null(areaid))) {
    
    fips <- areaid
    if (missing(areatype)) {
      areatype <- fipstype(fips)
    }
    if (!(all(areatype %in% c("blockgroup", "tract", "city", "county", 'state')))) {warning("FIPS must be one of 'blockgroup', 'tract', 'city', 'county', 'state' for the EJScreen API")}
    if (!(length(areatype) %in% c(1, length(areaid)))) {warning("must provide either 1 areatype value for all or exactly one per areaid")}
    
    # namestr <- fips   # user could specify something else here
    if (is.null(namestr)) {namestr <- ''} 
    # The FIPS can be displayed as the name of the place on the EJScreen report since it already looks up and displays the actual name of a county or city
    # namestr <- rep("", length(areatype))
    # namestr[namestr == "county"] <- fips2countyname(fips[namestr == "county"])
    # # namestr[namestr == "state"] <- fips2statename(fips[namestr == "state"])
    
  } else {
    
    # Flexibly allow for user to provide latlon as 1 table or as 2 vectors or 1 filename or 1 interactively selected file
    if (!(!missing(lat) && all(is.na(lat)))) {
    latlon_table <- sitepoints_from_anything(lat, lon, interactiveprompt = interactiveprompt)[ , c("lat","lon")]
    lat <- latlon_table$lat
    lon <- latlon_table$lon
    }
    # error checking lat lon radius
    
    latlon_radius_validate_lengths <- function(lat, lon, radius) {
      if (!is.numeric(radius) || !is.numeric(lat) || !is.numeric(lon)) {warning("lat or lon or radius is not numeric")}
      # but that is OK in url_ejscreen_report context where areaid can be used instead and lat default is ""
      if (length(radius) == 0 || length(lat) == 0 || length(lon) == 0) {warning("lat or lon or radius missing entirely (length of a vector is zero")}
      if (is.null(radius)     || is.null(lat)     || is.null(lon))     {warning("lat or lon or radius is NULL")}
      if (anyNA(radius)       || anyNA(lat)       || anyNA(lon))       {warning("lat or lon or radius contain NA value(s)")}
      if (length(lat)  != length(lon)) {warning("did not find exactly one lat for each lon value (lengths of vectors differ)")}
      if (!(length(radius) %in% c(1, length(lat), length(lon)))) {warning("must provide either 1 radius value for all sites or exactly one per site")}
      if (!( "" %in% lat | "" %in% lon ) & (any(is.na(radius)) | "" %in% radius)) {warning('radius is missing but needed when lat/lon specified')} # ??
    }
    latlon_radius_validate_lengths(lat = lat, lon = lon, radius = radius)
  }
  if (( "" %in% lat | "" %in% lon ) & ("" %in% areaid)) {
    warning('at least some of lat or lon are empty and at least one areaid is empty as well - must use one or the other')
  }
  
  # ejscreenRESTbroker works only for one url at a time:
  #  ejscreenRESTbroker(lon = lon, lat = lat, radius = radius, f = "report" )
  # 'https://ejscreen.epa.gov/mapper/EJscreen_SOE_report.aspx?namestr=&geometry={"spatialReference":{"wkid":4326},"x":-100.11715256086383,"y":36.65046624822855}&distance=1&unit=9035&areatype=&areaid=&f=report' 
  if (mobile) {
    baseurl <- "https://ejscreen.epa.gov/mapper/mobile/EJSCREEN_mobile.aspx?"   # ok 7/23/23
  } else {
    baseurl <- 'https://ejscreen.epa.gov/mapper/EJscreen_SOE_report.aspx?'  # ok 7/23/23
  }
  
  xytext <- paste0('"x":', lon, ',"y":', lat, '}')
  # f = "report"
  # wkid = 4326 
  # unit = 9035
  url <-  paste0(baseurl,
                 '&geometry={"spatialReference":{"wkid":',wkid,'},',
                 # '"x":', lon, ',"y":', lat, '}',
                 xytext,
                 '&distance=', radius,
                 '&unit=', unit, 
                 "&areatype=", areatype,
                 "&areaid=", areaid,
                 "&namestr=", namestr,
                 
                 '&f=', f
  )
  
  if (as_html) {
    if (missing(linktext)) {linktext <- paste0("EJScreen Report")}
    url <- url_linkify(url, text = linktext)    
  }
  return(url)
}  
################################################### #################################################### #


#' Get URLs of EJScreen older ACS reports - will be phased out
#' 
#' Get URL(s) for EJScreen ACS report on residents near given point(s)
#' 
#' @details  NOT USED BY APP NOW THAT COMMUNITY REPORT EXISTS
#' 
#' @param lon one or more longitudes
#' @param lat one or more latitudes
#' @param radius miles radius
#' @param as_html Whether to return as just the urls or as html hyperlinks to use in a DT::datatable() for example
#' @param linktext used as text for hyperlinks, if supplied and as_html=TRUE
#' @seealso  [url_ejscreen_report()]  [url_ejscreen_acs_report()]   [url_ejscreenmap()]
#'   [url_echo_facility_webpage()] [url_frs_report()]  [url_enviromapper()]  [url_envirofacts_data()]
#' @return URL(s)
#' 
#' @keywords internal
#' @export
#'
url_ejscreen_acs_report <- function(lon, lat, radius, as_html=FALSE, linktext) {
  
  baseurl <- "https://ejscreen.epa.gov/mapper/demogreportpdf.aspx?feattype=point&radius="   
  part2 <- "&coords="
  url <- paste0(baseurl, radius, part2, paste(lon,lat,sep = ",")) # it has to be lon then lat in this URL
  if (as_html) {
    if (missing(linktext)) {linktext <- paste0("ACS report")}
    url <- url_linkify(url, text = linktext)    
  }
  return(url)
  # "https://ejscreen.epa.gov/mapper/demogreportpdf.aspx?feattype=point&radius=3.1&coords=-81.978358,30.296344"
  #  dd=3.1; lon=-81.978358; lat=30.296344
  # url_ejscreen_acs_report(lat=lat, lon = lon, radius = dd)
  # browseURL(url_ejscreen_acs_report(lat = testpoints_50$lat, lon = testpoints_50$lon, radius = 3.1)[1])
} 
################################################### #################################################### #


#' Get URL(s) for EJScreen map centered at given point(s)
#' 
#' @param lon one or more longitudes
#' @param lat one or more latitudes
#' @param as_html Whether to return as just the urls or as html hyperlinks to use in a DT::datatable() for example
#' @param linktext used as text for hyperlinks, if supplied and as_html=TRUE
#' @param wherestr Name of place to center on (not FIPS code!).
#'   Passed to API in URL as wherestr= , if lat/lon not used.
#'   Can be State abbrev like "NY" or full state name,
#'   or city like "New Rochelle, NY" as from fips2name() -- using fips2name()
#'   works for state, county, or city FIPS code converted to name.
#'   Also wherestr can be street address or zipcode.
#'   Not sure that part of the EJScreen URL-encoded map request is documented.
#' @param shapefile not implemented
#' @return URL(s)
#' @seealso  [url_ejscreen_report()]  [url_ejscreen_acs_report()]   [url_ejscreenmap()]
#'   [url_echo_facility_webpage()] [url_frs_report()]  [url_enviromapper()]  [url_envirofacts_data()]
#'   
#' @export
#'
url_ejscreenmap <- function(lon, lat, as_html=FALSE, linktext, 
                            wherestr = "", 
                            shapefile = NULL) {
  
  # https://ejscreen.epa.gov/mapper/index.html?wherestr=30.450000,-91.090000
  baseurl <- 'https://ejscreen.epa.gov/mapper/index.html?wherestr='
  if (!missing(wherestr) && any(missing(lat) & missing(lon)) || ((is.null(lat) & is.null(lon)) ) ) {
    where <- wherestr
  }  else {
    where <- paste( lat,  lon, sep = ',')
  }
  url <- URLencode(paste0(baseurl, where))
  if (as_html) {
    if (missing(linktext)) {linktext <- "EJScreen Map"}  #   paste0("EJScreen Map ", 1:length(lon)) 
    url <- url_linkify(url, text = linktext)    
  }
  return(url)
} 
################################################### #################################################### #


#' Get URLs of ECHO reports
#' 
#' Get URL(s) for EPA ECHO webpage with facility information
#' 
#' @param regid EPA FRS Registry ID
#' @param as_html Whether to return as just the urls or as html hyperlinks to use in a DT::datatable() for example
#' @param linktext used as text for hyperlinks, if supplied and as_html=TRUE
#' @seealso  [url_ejscreen_report()]  [url_ejscreen_acs_report()]   [url_ejscreenmap()]
#'   [url_echo_facility_webpage()] [url_frs_report()]  [url_enviromapper()]  [url_envirofacts_data()]
#'   
#' @seealso  [url_ejscreen_report()]  [url_ejscreen_acs_report()]   [url_ejscreenmap()]
#'   [url_echo_facility_webpage()] [url_frs_report()]  [url_enviromapper()]  [url_envirofacts_data()]
#' @return URL(s)
#' @examples  \dontrun{
#'  browseURL(url_echo_facility_webpage(110070874073))
#'  }
#'  
#' @export
#'
url_echo_facility_webpage <- function(regid, as_html=FALSE, linktext) {
  
  # do error checking here
  
  baseurl <- "https://echo.epa.gov/detailed-facility-report?fid="
  url <-  paste0(baseurl, regid)
  if (as_html) {
    if (missing(linktext)) {linktext <- regid}
    url <- url_linkify(url, text = linktext)    
  }
  return(url)
} 
################################################### #################################################### #


#' Get URLs of FRS reports
#' 
#' Get URL(s) for reports on facilities from EPA FRS (facility registry service)
#' 
#' @param regid one or more EPA FRS Registry IDs. 
#' @param as_html Whether to return as just the urls or as html hyperlinks to use in a DT::datatable() for example
#' @param linktext used as text for hyperlinks, if supplied and as_html=TRUE
#'
#' @seealso  [url_ejscreen_report()]  [url_ejscreen_acs_report()]   [url_ejscreenmap()]
#'   [url_echo_facility_webpage()] [url_frs_report()]  [url_enviromapper()]  [url_envirofacts_data()]
#'   
#' @return URL(s)
#' @examples \dontrun{
#' browseURL(url_frs_report(testids_registry_id)[1])
#' }
#' url_frs_report(testids_registry_id)
#' 
#' @export
#'
url_frs_report <- function(regid, as_html=FALSE, linktext ) {
  
  # both of these URLs seem to work:
  #baseurl <- "https://ofmpub.epa.gov/frs_public2/fii_query_dtl.disp_program_facility?p_registry_id="
  baseurl <- "https://frs-public.epa.gov/ords/frs_public2/fii_query_dtl.disp_program_facility?p_registry_id="
  
  url <- paste0(baseurl, regid)
  if (as_html) {
    if (missing(linktext)) {linktext <- paste0("Envirofacts")}
    url <- url_linkify(url, text = linktext)    
  }
  return(url)
}
################################################### #################################################### #


#' Get URLs of EnviroMapper reports
#' 
#' Get URL(s) for EnviroMapper web-based tool, to open map at specified point location(s)
#' 
#' @details EnviroMapper lets you view EPA-regulated facilities and other information on a map, given the lat,lon
#' @param lon one longitude
#' @param lat one latitude
#' @param as_html Whether to return as just the urls or as html hyperlinks to use in a DT::datatable() for example
#' @param linktext used as text for hyperlinks, if supplied and as_html=TRUE
#' @param zoom initial map zoom extent, with smaller numbers if zoomed in closer
#' @seealso  [url_ejscreen_report()]  [url_ejscreen_acs_report()]   [url_ejscreenmap()]
#'   [url_echo_facility_webpage()] [url_frs_report()]  [url_enviromapper()]  [url_envirofacts_data()]
#'   
#' @return URL of one webpage (that launches the mapping tool)
#' 
#' @export
#'
url_enviromapper <- function(lon, lat, as_html=FALSE, linktext, zoom=13) {
  
  # "https://geopub.epa.gov/myem/efmap/index.html?ve=13,38.895237,-77.029145"
  
  baseurl <- "https://geopub.epa.gov/myem/efmap/index.html?ve="
  
  url <- paste0(baseurl, zoom, ",", lat, ",", lon)
  if (as_html) {
    if (missing(linktext)) {linktext <- paste0("EnviroMapper")}
    url <- url_linkify(url, text = linktext)    
  }
  return(url)
}
################################################### #################################################### #


#' Get URLs of EPA EnviroFacts API queries - DRAFT INCOMPLETE WORK IN PROGRESS
#' 
#' Get URL(s) to query and get data on facilities from Envirofacts, in XML,JSON,CSV, or EXCEL format
#' 
#' @details 
#'   see <https://www.epa.gov/enviro/web-services> and
#'  
#'   <https://www.epa.gov/enviro/envirofacts-data-service-api>
#'   
#'   (https://data.epa.gov/efservice/multisystem/minLatitude/35.465158/maxLatitude/52.912225/minLongitude/-104.387994/maxLongitude/-69.231744/count) ???
#'   
#'   WORK IN PROGRESS .... need to look at the documentation more closely but what it returns is not what is expected for some attempts
#'   
#'   WEBPAGE URL (returns a webpage)
#'   e.g., <https://enviro.epa.gov/enviro/efsystemquery.multisystem?fac_search=primary_name&fac_value=&fac_search_type=Beginning+With&postal_code=&location_address=&add_search_type=Beginning+With&city_name=&county_name=&state_code=&TribalLand=0&TribeType=selectTribeALL&selectTribe=noselect&tribedistance1=onLand&sic_type=Equal+to&sic_code_to=&naics_type=Beginning+with&naics_to=326&chem_name=&chem_search=Beginning+With&cas_num=&page_no=1&output_sql_switch=FALSE&report=1&database_type=Multisystem>
#'   https://enviro.epa.gov/enviro/efsystemquery.multisystem?state_code=DE&naics_type=Beginning+with&naics_to=326&database_type=Multisystem
#'    
#'   API URL (returns data)
#'   https://data.epa.gov/efservice/multisystem/minLatitude/35.465158/maxLatitude/52.912225/minLongitude/-104.387994/maxLongitude/-69.231744/naics_type/Beginning+with/naics_to/32611
#'   https://data.epa.gov/efservice/multisystem/minLatitude/35.465158/maxLatitude/52.912225/minLongitude/-104.387994/maxLongitude/-69.231744/EXCEL
#'   
#' @param regid EPA-regulated facility registry identification (ID), vector
#' @param as_html Whether to return as just the urls or as html hyperlinks to use in a DT::datatable() for example
#' @param linktext used as text for hyperlinks, if supplied and as_html=TRUE
#'
#' @seealso  [url_ejscreen_report()]  [url_ejscreen_acs_report()]   [url_ejscreenmap()]
#'   [url_echo_facility_webpage()] [url_frs_report()]  [url_enviromapper()]  [url_envirofacts_data()]
#'   
#' @return URL(s)
#' 
#' @keywords internal
#' @export
#'
url_envirofacts_data <- function(regid, as_html=FALSE, linktext ) {
  
  stop("work in progress")
  baseurl <- "https://"
  
  url <- paste0(baseurl, regid)
  if (as_html) {
    if (missing(linktext)) {linktext <- paste0("Envirofacts")}
    url <- url_linkify(url, text = linktext)    
  }
  return(url)
}
################################################### #################################################### #
