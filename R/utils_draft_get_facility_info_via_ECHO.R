
#' NOT USED - WAS WORK IN PROGRESS - was to get info on EPA-regulated facilities via EPA ECHO API - query by NAICS etc.
#' 
#' @description see EJAM package functions like 
#'   latlon_from_naics() latlon_from_siteid()  
#'   latlon_from_programid()  latlon_from_program() 
#'   that might make this function obsolete.
#' 
#' @details 
#'  
#'   See info about ECHO web services at <https://echo.epa.gov/tools/web-services>. 
#'   Use the Metadata service endpoint for a list of available output objects, 
#'   their Column Ids, and their definitions to help you build your customized output,
#'   and see examples at 
#'   <https://echo.epa.gov/tools/web-services/facility-search-all-data#/Metadata> and 
#'   <https://echodata.epa.gov/echo/echo_rest_services.metadata?output=JSON>
#'   
#'   In ECHO, one can search for facilities or permits by 
#'   EPA Registry ID (i.e., FRS ID) or by the 
#'   Program System ID (CWA, CAA, SDWA, or RCRA). 
#'   The web interface at <https://echo.epa.gov/facilities/facility-search>
#'   allows data entry of up to 2,000 IDs pasted from spreadsheet column, or comma- or return-separated. 
#'  
#' @param p_ncs NAICS industrial code
#' @param qcolumns vector specifying which variables to return (see varsinfo_ECHO_API).
#'   Column numbers work and are what the ECHO API expects, but here you can instead - or in addition - 
#'   also use these words referring to groups of variables defined in this package:
#'     critical  best   useful   programid    ej
#'   to specify variables where, e.g., the word critical would 
#'   get variables where varsinfo_ECHO_API$critical == TRUE
#' @param output JSON by default, to get output in that format 
#' @param otherparameters appended text at end of URL
#' @param url_not_query logical, just return the URL but not query 
#' @param testing logical 
#' @param getcsv logical, use get_download
#' 
#' @seealso varsinfo_ECHO_API  get_siteid_from_naics()
#' @return Tries to return a table via data.table::as.data.table(), with these columns: 
#'    "ObjectId" "FacName" "RegistryID" "FacLat" "FacLong" "lat" "lon" "registry_id"
#' @import data.table
#' @examples 
#'   x1 = get_facility_info_via_ECHO(562213, url_not_query = F)
#'   x2 = latlon_from_naics(562213)
#'   
#' @keywords internal
#' 
get_facility_info_via_ECHO <- function(p_ncs=NULL, qcolumns=c(16,17), output='JSON', otherparameters=NULL, 
                                       url_not_query=TRUE, testing=FALSE, getcsv=FALSE) {
   warning('NOT WORKING YET - WORK IN PROGRESS. see x <- latlon_from_naics(562213)')
  #  THIS IS JUST WORK IN PROGRESS, started EXPLORING HOW TO USE THE ECHO API TO QUERY ECHO/FRS 
  #  AND GET A LIST OF LAT/LON/ETC BY NAICS
  
  # more likely to use  locate_by_id() that relies on FRS API
  # or just look in  frs or similar data file instead of an API
  
  if (testing) p_ncs = '325110' # or 311514
  # or say, 562213 - Solid Waste Combustors and Incinerators
  baseurl <- 'https://echodata.epa.gov/echo/echo_rest_services.get_facility_info'
  # basevalidate <- 'https://echodata.epa.gov/echo/echo_rest_services.get_facilities'
  # https://echo.epa.gov/tools/web-services/facility-search-all-data#/Facility%20Info/get_echo_rest_services_get_facility_info
  # https://echodata.epa.gov/echo/echo_rest_services.get_facilities?output=JSON&p_ncs=325%2C331410&qcolumns=16%2C17
  # https://www.naics.com/search/ 
  # p_ncs=c(325,331410)
  # good url #  https://echodata.epa.gov/echo/echo_rest_services.get_facility_info?p_ncs=325&qcolumns=16%2C17
  #
  # JUST ONE STATE, ONE 3-DIGIT NAICS, ASK FOR LAT / LON COLUMNS:
  #    https://echodata.epa.gov/echo/echo_rest_services.get_facility_info?p_st=DE&p_ncs=325&qcolumns=16%2C17
  #
  # PROBLEM WITH ECHO API is you get lat/lon per cluster of facilities, not every facility, 
  #  if you try to ask for too many in one query, via get_facility_info
  
  if (is.null(p_ncs)) {
    p_ncs <- ''
  } else {
    p_ncs <- paste0('&p_ncs=', paste(p_ncs, collapse = ','))
  }
  
  qcolumns <- echo_colids_from_num_name_group(qcolumns) # checks and converts all forms of col specifier to col numbers
  qcolumns <- paste0('&qcolumns=', paste(qcolumns, collapse = ','), sep = '')
  
  if (is.null(output)) {
    output <- ''
  } else {
    output <- paste0('output=', output)
  }
  
  urlquery <- paste0(baseurl, '?', output, p_ncs, qcolumns)
  if (!is.null(otherparameters)) {
    urlquery <- paste0(urlquery, otherparameters)
  }
  
  if (testing)  urlquery <- paste0(urlquery, '&p_st=DE') # testing
  
  urlquery <- URLencode(urlquery) # urltools::url_encode(urlquery) 
  
  if (testing) {
    print(p_ncs)
    print(qcolumns)
    print(urlquery)
  }
  
  # just return URL that would give full results on facilities ####
  
  if (url_not_query) {return(urlquery)}
  
  
  # stop if cannot validate query,  and get basic info    ####
  
  urlvalidation <- gsub('get_facility_info', 'get_facilities', urlquery)
  x <- httr::GET(urlvalidation) 
  # jsonlite::fromJSON(rawToChar(x$content))$Results$ClusterOutput$ClusterData[,1:8]
  x <- as.data.frame(as.list(unlist((jsonlite::fromJSON(rawToChar(x$content)))[['Results']])))
  if (x$Message != "Success") {warning('failed query'); return()}
  
  hits <- x$QueryRows
  print(paste0(hits,' results found'))
  
  
  # do full query for csv format, save as csv file and stop #### 
  
  if (getcsv) {
    warning('does not work completely - glitches in format of csv data')
    # QueryID to get csv download
    # https://echodata.epa.gov/echo/echo_rest_services.get_download?output=csv&qid=311
    mycsv <- paste0('https://echodata.epa.gov/echo/echo_rest_services.get_download?output=csv&qid=', x$QueryID)
    mycsv <- try(httr::GET(mycsv))
    if (mycsv$Message != "Success") {stop('csv query failed')}
    mycsv <- (rawToChar(mycsv$content))
    # write to file
    cat(mycsv, file = 'testfile.csv')
    invisible(mycsv) # cat()  on the returned value shows it as csv format
  }
  
  
  # do full query and turn into a data.frame  ####
  
  requested <- try(httr::GET(urlquery))  # THIS FAILS NOW? ********
  if  (any(class(requested) == 'try-error')) {stop('GET failed')}
  
  # parse results to data.frame format, for just this one buffer ####
  stuff <- jsonlite::fromJSON(rawToChar(requested$content))
  
  # > jsonlite::fromJSON(rawToChar(requested$content))
  # $Results
  # $Results$Message
  # [1] "Success"
  # 
  # $Results$QueryID
  # [1] "76"
  # 
  # etc
  
  # > cbind(jsonlite::fromJSON(rawToChar(requested$content))$Results)
  # [,1]                                                                          
  # Message           "Success"                                                                     
  # QueryID           "76"                                                                          
  # QueryRows         "4"                                                                           
  # IndianCountryRows "0"                                                                           
  # SVRows            "1"                                                                           
  # CVRows            "1"                                                                           
  # V3Rows            "2"                                                                           
  # FEARows           "2"                                                                           
  # InfFEARows        "2"                                                                           
  # INSPRows          "3"                                                                           
  # TotalPenalties    "$521,400"                                                                    
  # CAARows           "3"                                                                           
  # CWARows           "1"                                                                           
  # RCRRows           "4"                                                                           
  # TRIRows           "2"                                                                           
  # BadSystemIDs      NULL                                                                          
  # IconBaseURL       "https://echo.epa.gov/themes/custom/echo/images/map/"                         
  # PopUpBaseURL      "https://echodata.epa.gov/echo/echo_rest_services.pop_up?p_id="               
  # ServiceBaseURL    "https://ordsint.rtpnc.epa.gov/ords/echo/echo_rest_services.get_facility_info"
  # QueryParameters   data.frame,2                                                                  
  # Facilities        data.frame,5   
  
  
  # $Results$Facilities
  # ObjectId                                            FacName   RegistryID    FacLat    FacLong
  # 1        1                                AKZO CHEMICALS INC. 110001102459   39.5878   -75.6392
  # 2        2                     BASF COLORS & EFFECTS, NEWPORT 110041998005  39.71161  -75.60906
  # 3        3                              CRODA INC ATLAS POINT 110000338652 39.694609 -75.540494
  # 4        4 STANDARD CHLORINE OF DELAWARE, INC. SUPERFUND SITE 110000338554  39.59807  -75.63618
  # 
  # 
  # stuff$Results[[1]]
  # [1] "Success"
  # stuff$Results$PopUpBaseURL
  # [1] "https://echodata.epa.gov/echo/echo_rest_services.pop_up?p_id="
  
  cat('Query message: ', stuff$Results$Message, '\n')
   
  sitelist <- stuff$Results$Facilities 
  sitelist <- data.table::copy(data.table::setDT(sitelist))
  # create versions of those names that are what is used in these packages, for convenience
  if ('FacLat' %in% names(sitelist)) {
    sitelist[ , lat := FacLat]} # these columns will be duplicated if renamed also to these versions of names but that is prob ok
  if ('FacLong' %in% names(sitelist))  { 
    sitelist[ , lon := FacLong]
  }
  if ('RegistryID' %in% names(sitelist)) {
    sitelist[ , registry_id := RegistryID] 
  }
  # if error in JSON parsing ####
  if  (any(class(sitelist) == 'try-error')) {
    warning('error in parsing JSON returned by ECHO API ', urlquery, ' - Returning no result.')
    sitelist <- NA
  }
  return(sitelist)
}
