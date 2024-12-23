
#' Low-level utility to query EJScreen API for one circular buffer
#' 
#' @description Use EJScreen API to get raw json-format response, ready to be
#' parsed by [ejscreenRESTbroker2table()]. This function underlies higher level
#' functions like [ejscreenapi()] and overall [ejscreenit()]
#' 
#' @details  Note the public and internal IP addresses differ. 
#' 
#'    See <https://ejscreen.epa.gov/mapper/ejscreenRESTbroker1.aspx?geometry={%22spatialReference%22:{%22wkid%22:4326},%22x%22:-100.2,%22y%22:36}&distance=1&unit=9035&f=json> 
#'    
#'    for example of json output format, or see source code of this function. 
#'    
#'    See <https://ejscreen.epa.gov/mapper/EJscreen_SOE_report.aspx?namestr=&geometry={%22spatialReference%22:{%22wkid%22:4326},%22x%22:-100.12811027526683,%22y%22:36.6582500495267}&distance=10&unit=9035&areatype=&areaid=&f=report>
#'     for an example of the pdf report output.
#'     
#'    Note some variables are duplicated in outputs.
#'    
#'    API: Introduction to how the API works: 
#'    
#'    - <https://www.epa.gov/ejscreen/ejscreen-api>
#'    
#'    API: Simple web interface to try out the API and learn how parameters can be specified in URL:
#'    
#'    - 2023_07         ver 2.2         <https://ejscreen.epa.gov/mapper/ejscreenapi1.html>
#'    - old style/mini ver 2.2 (fewer indicators) <https://ejscreen.epa.gov/mapper/ejscreenapi.html>
#'    
#'    API: REST endpoint:
#'    
#'    - 2023_07  ver 2.2 <https://ejscreen.epa.gov/mapper/ejscreenRESTbroker1.aspx?namestr=>
#'    - old style/mini ver 2.2 <https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx?namestr=>
#'    
#'    csv/gdb files: Data dictionary for downloadable dataset: 
#'
#'    - 2023_07  ver 2.2 <https://gaftp.epa.gov/EJScreen/2023/EJSCREEN_2023_BG_Columns.xlsx>
#'    
#'    API: Data dictionary of variable names: 
#'
#'    - 2023_07  ver 2.2 <https://ejscreen.epa.gov/mapper/ejsoefielddesc1.html>
#'    - old style/mini ver 2.2 <https://ejscreen.epa.gov/mapper/ejsoefielddesc.html>
#'    
#'    Webpage explaining the indicators
#'    
#'    - ver 2.2  <https://origin-awswest-www.epa.gov/ejscreen/ejscreen-map-descriptions>
#'    
#'    Web tool user guide: 
#'    
#'    - 2023_07         ver 2.2 <https://ejscreen.epa.gov/mapper/help/ejscreen_help.pdf>
#'
#' @param lon a longitude 
#' @param lat a latitude
#' @param radius radius of the circular buffer (normally in miles) 
#' @param unit 9035 for miles, 9036 for kilometers, but default is miles
#' @param wkid spatial reference. https://epsg.io/4326
#' @param f pjson for results in JSON format, or report for the URL of the pdf report
#' @param url URL base for API (provides access to the shorter version vs default longer list of indicators),
#'   where newer 2023 version is default, using ejscreenRESTbroker1.aspx, 
#'   and old style/mini version is at ejscreenRESTbroker.aspx
#' @param ipurl fixed ip or domain/URL to try
#' @param reportstyle EJscreen_SOE_report for the full community profile that was new as of 7/2023,
#'   or EJSCREEN_report for the older style standard report (which has fewer indicators on it).
#' @param fips If specified, lon and lat are ignored, and the one fips code must be the 
#'   FIPS code of a blockgroup or tract, or county (5 digits with leading zero)
#'   or city/town/cdp/etc. (7 digits with leading zero).
#'   A character string is best, with leading zero if relevant.
#' @param namestr optional text
#' @param shapefile not implemented
#' @return Returns JSON by default. See source code of this function for notes on format. 
#'   status_code
#' @seealso [ejscreenit()]  or 
#'   [ejscreenit()] which use [ejscreenapi_plus()] 
#'   and that uses [ejscreenapi()] that uses [ejscreenapi1()] and [ejscreenRESTbroker()] and [ejscreenRESTbroker2table()]
#' @examples \dontrun{
#'   browseURL(ejscreenRESTbroker(lon = -80, lat = 42, f = 'report'))
#'   
#'   x = ejscreenRESTbroker(lon = -80, lat = 42) 
#'   df = ejscreenRESTbroker2table(x)
#'   class(df)
#'   t(df)
#'   
#'   x = ejscreenRESTbroker(lon = -80, lat = 42) 
#'   names(x)
#'   x$url
#'   x$status_code
#'   names(jsonlite::fromJSON(rawToChar(x$content)))
#'   names(jsonlite::fromJSON(rawToChar(x$content))$data)
#'   names(jsonlite::fromJSON(rawToChar(x$content))$data$main)
#'   names(jsonlite::fromJSON(rawToChar(x$content))$data$extras)
#'   names(jsonlite::fromJSON(rawToChar(x$content))$data$demographics)
#'   }
#'
#' @export
#' @keywords internal
#' 
ejscreenRESTbroker <- function(lon = NULL, lat = NULL, radius = 3, 
                               fips = NULL,
                               namestr = '',
                               shapefile = NULL, # would need POST not GET
                               url = c('https://ejscreen.epa.gov/mapper/ejscreenRESTbroker1.aspx?', 'https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx?')[1],
                               ipurl='ejscreen.epa.gov',
                               wkid = 4326, 
                               unit = 9035, 
                               f = 'pjson', 
                               reportstyle=c("EJscreen_SOE_report", "EJSCREEN_report")[1]) {
  
  url <- sub('(https://).*?(/mapper)', paste0('\\1',ipurl,'\\2'), url)
  
  if (!missing(shapefile) & !is.null(shapefile)) {
    sitetype <- 'shp'
  } else {
    if (!is.null(fips)) {
      sitetype <- 'fips'
    } else {
      if (!is.null(lon) & !is.null(lat)) {
        sitetype <- 'latlon'
      } else {
        # no type found
        stop('must provide lat and lon, or fips, or shapefile')
      }
    }
  }
  
  #################### #  
  
  # shapefile ####

  # Example of how theEJScreen API could be used to analyze a polygon, which must use POST not GET:
  # HTTP POST URL: https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx
  # HTTP POST Body:
  #   namestr=
  #   geometry={"spatialReference":{"wkid":4326},"rings":[[[-76.6418006649668,39.41979061319584],[-76.54223706633402,39.403875492879656],[-76.48158343568997,39.32424541053687],[-76.45526191279846,39.24452456392063],[-76.63378974482964,39.202856485626576],[-76.74021979854052,39.284396329589654],[-76.74594187237864,39.37911140807963],[-76.6418006649668,39.41979061319584]]]}
  #   distance=
  #   unit=9035
  #   f=pjson
  
  #################### #  
  
  # fips ####
  
  if (sitetype == 'fips') {
    
    if (length(fips) != 1) {stop('fips must be just one number, as numeric or character')}
    fips <- fips_lead_zero(fips)
    if (missing(namestr)) {namestr <- fips}
    areatype <- fipstype(fips)
    if (!(areatype %in% c('blockgroup', 'tract', 'city', 'county'))) {
      stop('fips must be   5, 7, 11, or 12 digits, i.e., fips for county, city/cdp/town/etc., tract, or blockgroup')
    }
    if ((!is.null(lat) || !is.null(lon)) && !all(is.na(lat)) && !all(is.na(lon))) {warning("ignoring lat and lon because fips was specified")}
    if (!missing(radius) && radius != 0) {warning("ignoring radius because fips was specified")}
    
    # https://ejscreen.epa.gov/mapper/ejscreenRESTbroker1.aspx?namestr=400079517001&geometry=&distance=&unit=9035&areatype=blockgroup&areaid=400079517001&f=pjson
    
    geotext <- ''
    
  } 
  #################### #  
  
  # latlon ####
  
  if (sitetype == 'latlon') {
    
    if (any(NROW(lon) > 1, NROW(lat) > 1, NROW(radius) > 1 )) {stop('input must be only one point with one distance, so lat, lon, and radius must each be a single number')}
    if (all(is.na(lat)) | all(is.na(lon))) {stop('lat and lon must not be NA values')}
    
    # MAY WANT TO SPLIT THIS OUT AS A FUNCTION, TO MAKE IT EASIER TO GET JSON AND ALSO APPEND THE PDF URL TO THAT
    # see url_ejscreen_report() for obtaining a vector of URLs, with more options and error-handling
    
    areatype <- ''
    fips     <- ''
    
    geometry <- paste0('{"spatialReference":{"wkid":',wkid, '},','"x":', lon, ',"y":', lat, '}')
    geotext <- paste0(
      '&geometry=', geometry,
      '&distance=', radius
    )
    
  }
  #################### #    #################### #  
  
  this_request <-  paste0(url,
                          '&areatype=', areatype,
                          '&areaid=',   fips,
                          '&namestr=',  namestr,
                          geotext,  # geometry and distance
                          '&unit=', unit, 
                          '&f=', f
  )
  
  ## Alternative method of crafting the request: 
  #
  # url <- urltools  ::  param_set(url, key = "areatype", value = areatype)
  # url <- urltools  ::  param_set(url, key = "areaid",   value = fips)
  # url <- urltools  ::  param_set(url, key = "namestr",  value = namestr)
  # url <- urltools  ::  param_set(url, key = "geometry", value = geometry)
  # url <- urltools  ::  param_set(url, key = "distance", value = radius)
  # url <- urltools  ::  param_set(url, key = "unit",     value = unit)
  # url <- urltools  ::  param_set(url, key = "f",        value = f)
  # this_request <- url

    
  if (f == 'report') {
    
    # cat("see url_ejscreen_report() for obtaining a vector of URLs, with more options and error-handling\n")
    # PDFURL <- url_ejscreen_report() # could be used here to replace the line below but would need to test etc.
    PDFURL <- gsub('ejscreenRESTbroker1?', reportstyle, this_request) # gets rid of the 1 as well, if it is found
    
    # https://ejscreen.epa.gov/mapper/EJSCREEN_report.aspx?namestr=&geometry={"spatialReference":{"wkid":4326},"x":-88.14039550781403,"y":40.06610618160108}&distance=1&unit=9036&areatype=&areaid=&f=report 
    
    return(PDFURL) # returns the URL
    
  } else {
    
    # print(this_request)
    # using httr::GET(), but note GET() also is a function name used in other packages.
    
    gotten <- try(httr::GET(this_request))
    if (inherits(gotten, "try-error")) {
      warning("API GET request failed -- server may be unavailable or query was invalid, returning results of try(httr::GET()) which provides error message info")
    }
    
    ############################################################## # 
    ## See format of json results:
    # str(gotten)
    ## gotten is returned with these names:
    ##
    ##   "url"   "status_code"  "headers"   "all_headers"   "cookies"     
    ##   "content"     "date"      "times"   "request"     "handle"
    ############################################################## # 
    
    return(gotten)
  }
  
}
####################################################################################### #



####################################################################################### # 

##   just the data part:
#
# {
#   "data":
#     {
#       "demographics":

#         {"P_ENGLISH":"97","P_SPANISH":"3","P_FRENCH":"0","P_RUS_POL_SLAV":"0","P_OTHER_IE":"0","P_VIETNAMESE":"0","P_OTHER_ASIAN":"0","P_ARABIC":"0","P_OTHER":"0","P_NON_ENGLISH":"3",
#          "P_WHITE":"91","P_BLACK":"0","P_ASIAN":"1","P_HISP":"3","P_AMERIND":"0","P_HAWPAC":"0","P_OTHER_RACE":"3","P_TWOMORE":"5","P_AGE_LT5":"3","P_AGE_LT18":"22","P_AGE_GT17":"78","P_AGE_GT64":"28","P_HLI_SPANISH_LI":"100","P_HLI_IE_LI":"0","P_HLI_API_LI":"0","P_HLI_OTHER_LI":"0","P_LOWINC":"26","PCT_MINORITY":"9","P_EDU_LTHS":"11","P_LIMITED_ENG_HH":"4","P_EMP_STAT_UNEMPLOYED":"2","P_DISABILITY":"19","P_MALES":"51","P_FEMALES":"49",
#          "LIFEEXP":"79","PER_CAP_INC":"33502","HSHOLDS":"216","P_OWN_OCCUPIED":"74"},

#       "main":

#         {"RAW_D_PEOPCOLOR":"11%","RAW_D_INCOME":"17%","RAW_D_LESSHS":"21%","RAW_D_LING":"8%","RAW_D_UNDER5":"0%","RAW_D_OVER64":"25%","RAW_D_UNEMPLOYED":"3%","RAW_D_LIFEEXP":"19%","RAW_D_DEMOGIDX2":"14%","RAW_D_DEMOGIDX5":"13%","RAW_E_LEAD":"0.62","RAW_E_DIESEL":"0.0417","RAW_E_CANCER":"20","RAW_E_RESP":"0.2","RAW_E_TRAFFIC":"0.68","RAW_E_NPDES":"0.00023","RAW_E_NPL":"0.018","RAW_E_RMP":"0.067","RAW_E_TSDF":"0.015","RAW_E_O3":"58.2","RAW_E_PM25":"6.15","RAW_E_UST":"0.0029","RAW_E_RSEI_AIR":"2.5",
#          "S_D_PEOPCOLOR":"35%","S_D_INCOME":"37%","S_D_LESSHS":"12%","S_D_LING":"2%","S_D_UNDER5":"6%","S_D_OVER64":"16%","S_D_UNEMPLOYED":"5%","S_D_LIFEEXP":"22%","S_D_DEMOGIDX2":"36%","S_D_DEMOGIDX5":"16%","S_E_LEAD":"0.25","S_E_DIESEL":"0.166","S_E_CANCER":"29","S_E_RESP":"0.3","S_E_TRAFFIC":"83","S_E_NPDES":"0.058","S_E_NPL":"0.048","S_E_RMP":"0.38","S_E_TSDF":"0.43","S_E_O3":"62.3","S_E_PM25":"9.03","S_E_UST":"1.7","S_E_RSEI_AIR":"4100",
#          "S_D_PEOPCOLOR_PER":"7","S_D_INCOME_PER":"19","S_D_LESSHS_PER":"84","S_D_LING_PER":"92","S_D_UNDER5_PER":"8","S_D_OVER64_PER":"85","S_D_UNEMPLOYED_PER":"38","S_D_LIFEEXP_PER":"19","S_D_DEMOGIDX2_PER":"9","S_D_DEMOGIDX5_PER":"41","S_E_LEAD_PER":"87","S_E_DIESEL_PER":"0","S_E_CANCER_PER":"1","S_E_RESP_PER":"1","S_E_TRAFFIC_PER":"6","S_E_NPDES_PER":"52","S_E_NPL_PER":"32","S_E_RMP_PER":"17","S_E_TSDF_PER":"1","S_E_O3_PER":"3","S_E_PM25_PER":"0","S_E_UST_PER":"0","S_E_RSEI_AIR_PER":"5",
#          "S_P2_LEAD":"41","S_P2_DIESEL":"0","S_P2_CANCER":"3","S_P2_RESP":"0","S_P2_TRAFFIC":"4","S_P2_NPDES":"26","S_P2_NPL":"15","S_P2_RMP":"9","S_P2_TSDF":"0","S_P2_O3":"0","S_P2_PM25":"0","S_P2_UST":"0","S_P2_RSEI_AIR":"4","S_P5_LEAD":"71","S_P5_DIESEL":"0","S_P5_CANCER":"7","S_P5_RESP":"4","S_P5_TRAFFIC":"6","S_P5_NPDES":"53","S_P5_NPL":"37","S_P5_RMP":"19","S_P5_TSDF":"2","S_P5_O3":"3","S_P5_PM25":"0","S_P5_UST":"0","S_P5_RSEI_AIR":"5",
#          "N_D_PEOPCOLOR":"39%","N_D_INCOME":"31%","N_D_LESSHS":"12%","N_D_LING":"5%","N_D_UNDER5":"6%","N_D_OVER64":"17%","N_D_UNEMPLOYED":"6%","N_D_LIFEEXP":"20%","N_D_DEMOGIDX2":"35%","N_D_DEMOGIDX5":"14%","N_E_LEAD":"0.3","N_E_DIESEL":"0.261","N_E_CANCER":"28","N_E_RESP":"0.31","N_E_TRAFFIC":"210","N_E_NPDES":"22","N_E_NPL":"0.13","N_E_RMP":"0.43","N_E_TSDF":"1.9","N_E_O3":"61.6","N_E_PM25":"8.08","N_E_UST":"3.9","N_E_RSEI_AIR":"4600",
#          "N_D_PEOPCOLOR_PER":"25","N_D_INCOME_PER":"32","N_D_LESSHS_PER":"82","N_D_LING_PER":"82","N_D_UNDER5_PER":"0","N_D_OVER64_PER":"81","N_D_UNEMPLOYED_PER":"40","N_D_LIFEEXP_PER":"41","N_D_DEMOGIDX2_PER":"19","N_D_DEMOGIDX5_PER":"54","N_E_LEAD_PER":"81","N_E_DIESEL_PER":"3","N_E_CANCER_PER":"3","N_E_RESP_PER":"4","N_E_TRAFFIC_PER":"2","N_E_NPDES_PER":"36","N_E_NPL_PER":"15","N_E_RMP_PER":"16","N_E_TSDF_PER":"1","N_E_O3_PER":"25","N_E_PM25_PER":"9","N_E_UST_PER":"0","N_E_RSEI_AIR_PER":"5",
#          "N_P2_LEAD":"49","N_P2_DIESEL":"3","N_P2_CANCER":"11","N_P2_RESP":"11","N_P2_TRAFFIC":"3","N_P2_NPDES":"26","N_P2_NPL":"11","N_P2_RMP":"13","N_P2_TSDF":"0","N_P2_O3":"20","N_P2_PM25":"9","N_P2_UST":"0","N_P2_RSEI_AIR":"4","N_P5_LEAD":"75","N_P5_DIESEL":"4","N_P5_CANCER":"21","N_P5_RESP":"20","N_P5_TRAFFIC":"3","N_P5_NPDES":"45","N_P5_NPL":"19","N_P5_RMP":"23","N_P5_TSDF":"0","N_P5_O3":"33","N_P5_PM25":"13","N_P5_UST":"0","N_P5_RSEI_AIR":"6",
#   **       "stateAbbr":"OK","stateName":"OKLAHOMA","epaRegion":"6","totalPop":"214",
#            "NUM_NPL":"0","NUM_TSDF":"0","NUM_WATERDIS":"1","NUM_AIRPOLL":"29","NUM_BROWNFIELD":"0","NUM_TRI":"0","NUM_SCHOOL":"1","NUM_HOSPITAL":"0","NUM_CHURCH":"2","YESNO_TRIBAL":"No","YESNO_CEJSTDIS":"Yes","YESNO_IRADIS":"Yes","YESNO_AIRNONATT":"No","YESNO_IMPWATERS":"Yes","YESNO_HOUSEBURDEN":"No","YESNO_TRANSDIS":"Yes","YESNO_FOODDESERT":"Yes",
#            "centroidX":"-100.117152560864","centroidY":"36.6504662482285",
#   **       "geometry":{"spatialReference":{"wkid":4326},"x":-100.11715256086383,"y":36.650466248228547},
#   **       "statLayerCount":"4","statLayerZeroPopCount":"0","weightLayerCount":"181","timeSeconds":"1.3689622","distance":"10","unit":"9035","areaid":null,"areatype":null,"statlevel":"blockgroup","inputAreaMiles":"314.03",
#            "placename":"Beaver County, OK"},

#       "extras":

#         {"RAW_HI_LIFEEXP":"79.4","RAW_HI_LIFEEXPPCT":"19%","RAW_HI_HEARTDISEASE":"9","RAW_HI_ASTHMA":"10.2","RAW_HI_CANCER":"8.2","RAW_HI_DISABILITYPCT":"18.6%","RAW_CG_LIMITEDBBPCT":"20%","RAW_CG_NOHINCPCT":"13%","RAW_CI_FLOOD":"12%","RAW_CI_FLOOD30":"12%","RAW_CI_FIRE":"90%","RAW_CI_FIRE30":"91%",
#          "S_HI_LIFEEXP_AVG":"76","S_HI_LIFEEXPPCT_AVG":"22%","S_HI_HEARTDISEASE_AVG":"7.1","S_HI_ASTHMA_AVG":"11.1","S_HI_CANCER_AVG":"6.3","S_HI_DISABILITYPCT_AVG":"16.9%","S_CG_LIMITEDBBPCT_AVG":"17%","S_CG_NOHINCPCT_AVG":"15%","S_CI_FLOOD_AVG":"8%","S_CI_FLOOD30_AVG":"8%","S_CI_FIRE_AVG":"43%","S_CI_FIRE30_AVG":"57%",
#          "S_HI_LIFEEXP_PCTILE":"80","S_HI_LIFEEXPPCT_PCTILE":"19","S_HI_HEARTDISEASE_PCTILE":"83","S_HI_ASTHMA_PCTILE":"21","S_HI_CANCER_PCTILE":"94","S_HI_DISABILITYPCT_PCTILE":"63","S_CG_LIMITEDBBPCT_PCTILE":"64","S_CG_NOHINCPCT_PCTILE":"42","S_CI_FLOOD_PCTILE":"80","S_CI_FLOOD30_PCTILE":"80","S_CI_FIRE_PCTILE":"73","S_CI_FIRE30_PCTILE":"63",
#          "N_HI_LIFEEXP_AVG":"78.5","N_HI_LIFEEXPPCT_AVG":"20%","N_HI_HEARTDISEASE_AVG":"6.1","N_HI_ASTHMA_AVG":"10","N_HI_CANCER_AVG":"6.1","N_HI_DISABILITYPCT_AVG":"13.4%","N_CG_LIMITEDBBPCT_AVG":"14%","N_CG_NOHINCPCT_AVG":"9%","N_CI_FLOOD_AVG":"12%","N_CI_FLOOD30_AVG":"13%","N_CI_FIRE_AVG":"14%","N_CI_FIRE30_AVG":"19%",
#          "N_HI_LIFEEXP_PCTILE":"58","N_HI_LIFEEXPPCT_PCTILE":"41","N_HI_HEARTDISEASE_PCTILE":"92","N_HI_ASTHMA_PCTILE":"61","N_HI_CANCER_PCTILE":"90","N_HI_DISABILITYPCT_PCTILE":"81","N_CG_LIMITEDBBPCT_PCTILE":"73","N_CG_NOHINCPCT_PCTILE":"79","N_CI_FLOOD_PCTILE":"72","N_CI_FLOOD30_PCTILE":"69","N_CI_FIRE_PCTILE":"91","N_CI_FIRE30_PCTILE":"87",
#   **       "stateAbbr":"OK","stateName":"OKLAHOMA","epaRegion":"6","totalPop":"214",
#   **       "geometry":{"spatialReference":{"wkid":4326},"x":-100.11715256086383,"y":36.650466248228547},
#   **       "statLayerCount":"4","statLayerZeroPopCount":"0","weightLayerCount":"181","timeSeconds":"0.0889917","distance":"10","unit":"9035","areaid":null,"areatype":null,"statlevel":"blockgroup","inputAreaMiles":"314.03" 
#         }
#     }
# }
#          ** Note these groups of outputs are repeated, in main and extras!
####################################################################################### # 
