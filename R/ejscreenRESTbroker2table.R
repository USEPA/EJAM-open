
#' Low-level utility to convert result of EJScreen API query into table format
#' 
#' Convert json output of [ejscreenRESTbroker()] to a simple 1-row data.frame
#' 
#' @details This function assumes we got the full results via the API
#'   using [ejscreenRESTbroker()]. 
#'   
#'   It handles all 300 or so indicators, including those categorized by API outputs as 
#'   main, extras, and demographics, with duplicated indicators removed.
#'   
#'   It drops redundant columns where the same numbers had been returned from API
#'   using the normal name and a synonym name, as with "TOTALPOP" and "totalPop" 
#'   
#'   [ejscreenapi1()] relies on this function and essentially does this:
#'   
#'     ejscreenRESTbroker2table(ejscreenRESTbroker()) 
#'   
#'   Drops the percent signs and makes those values numeric, 
#'   converting text like 45% to the number 45.
#'   
#'   To ensure there is one result per input point, even if some points are invalid, 
#'   for later steps like ejscreenapi1, ejscreenapi, ejscreenapi_plus, ejscreenit(), 
#'   this function will return a row with the right columns but all NA values if the input seems wrong.
#'   
#' @param brokeroutput the results of [ejscreenRESTbroker()]
#' @param getstatefromplacename set to FALSE if you need the exact output of API and
#'   TRUE if you want to try to extract ST abbrev and statename from the placename field,
#'   which is more likely to be correct than the stateAbbr and stateName fields in the API output.
#' @return a data.frame of 1 row (a site) and many columns (indicators)
#' 
#' @export
#' @keywords internal
#'
ejscreenRESTbroker2table <- function(brokeroutput, getstatefromplacename = TRUE) {
  
  # Check if valid input provided to this function ####
  ej.data <- brokeroutput
  if (!("content" %in% names(ej.data))) {
    warning("brokeroutput passed to ejscreenRESTbroker2table must have 'content' among its names - returning NA values")
    return(ejscreenRESTbroker2table_na_filler)
  }
  # Try parsing it ####
  ej.data <- try(jsonlite::fromJSON(rawToChar(ej.data$content)))
  
  # Check if valid parsed results ####
  if (inherits(ej.data, "try-error")) {
    warning("Problem reading API query response when attempted jsonlite::fromJSON(rawToChar(brokeroutput$content)) -- could not parse output of ejscreenRESTbroker(), returning NA values")
    return(ejscreenRESTbroker2table_na_filler) # see /data/ and /data-raw/
  }
  if ("error" %in% names(ej.data)) {
    warning("API returned an error code (typically because radius is small enough that this rural site has no block centroid within that radius), returning NA values")
    return(ejscreenRESTbroker2table_na_filler) # see /data/ and /data-raw/
  }
  if (!("data" %in% names(ej.data))) {
    warning("Unexpected JSON response -- could not find 'data' among names of parsed API response, returning NA values")
    return(ejscreenRESTbroker2table_na_filler)
  }
  if (!all(c("demographics", "main", "extras") %in% names(ej.data$data))) {
    warning("Unexpected JSON response -- expected demographics, main, extras as 3 named lists in data from API. Returning NA values")
    return(ejscreenRESTbroker2table_na_filler) # see /data/ and /data-raw/
  }
  ej.data <- ej.data$data
  
  # Compile as one table ####
  # which the API provides in 3 tables:
  
  m1 <- ej.data[["demographics"]] # a named list of indicator values, character, like RAW_D_INCOME etc.
  m2 <- ej.data[["main"]] # some like areaid are null and geometry is a list itself, with  geometry$spatialReference$wkid, geometry$x, geometry$y 
  m3 <- ej.data[["extras"]] # same. RAW_HI_LIFEEXP etc. areaid is NULL. geometry is here too.
  
  demographics    <- as.data.frame(m1, stringsAsFactors = FALSE)
  main   <- as.data.frame(t(unlist(m2))) # that is a bit awkward 
  extras <- as.data.frame(t(unlist(m3))) # that is a bit awkward 
  
  # that is a bit awkward #  code above should catch all problems
  #   as.data.frame( ej.data[[3]], stringsAsFactors = FALSE)
  # another way to clean up instead of as.data.frame(t(unlist(m2))) , but would drop  geometry$spatialReference$wkid, geometry$x, geometry$y 
  # main <- as.data.frame(m2[(sapply(m2, function(x) !is.null(x) & !is.list(x)))]) # drops null or list like geometry
  # 
  #   leave these in the outputs:
  # > extras$geometry.x
  # [1] "-100"
  # > extras$geometry.y
  # [1] "44"
  # > extras$geometry.spatialReference.wkid
  # [1] "4326"
  
  results <- cbind(main, demographics, extras)
  
  # Drop cases where 2 of those 3 tables provided the same indicator using the same name ####
  results <- results[, !duplicated(names(results))] # because extras and main duplicated some variables, like totalPop and areaid
  
  # Drop cases where the same indicator value is provided under the standard variable name but also under an alias, which is redundant ####
  synhere <- names(results)[!(names(results) %in% map_headernames$apiname) & (names(results) %in% map_headernames$api_synonym)]
  cnames_redundant <- synhere[fixcolnames(synhere, 'api_synonym', 'api') %in% names(results)]
  if (length(cnames_redundant) > 0) {
    colnumkeep <- which(!(names(results) %in% cnames_redundant))
    results <- results[ , colnumkeep] # data.frame here unlike similar code in ejscreenapi
  }
  
  # # Fix percent signs and N/A ####
  # ## Percentage signs in some indicators may cause problems in calculations or sorting so remove those
  results <- makenumericdfFORSHINY(results)
  
  # Will convert to data.table format later, in ejscreenapi(). now just leave it as a data.frame
  
  ################################################# #
  ## patch to use placename info (if possible) to obtain name of state and abbreviation of state:
  
  if ("stateAbbr" %in% names(results) && "stateName" %in% names(results)) {
    results$stateAbbr_from_api <- results$stateAbbr
    results$stateName_from_api <- results$stateName
  } else {
    # they must have been phased out
    results$stateAbbr_from_api <- NA
    results$stateName_from_api <- NA
  }
  if (getstatefromplacename) {
    ## instead of stateAbbr and stateName info directly from the API, which was sometimes wrong and may get phased out
    ## per EJScreen tech support suggestion 8/2024:
    # results$placename = "Beaver County, OK" # example
    if (!("placename" %in% names(results))) {
      warning("API did not return placename, so cannot infer stateAbbr and stateName from placename, so just returning whatever API provided")
    } else {
      stateAbbr_hopefully <- substr(results$placename, nchar(results$placename) - 1, nchar(results$placename)) # get last two characters which should be 2-char State abbreviation
      
      if (all(stateAbbr_hopefully %in% c(state.abb, "DC", "PR", "AS", "GU", "MP", "UM", "VI" ) )) {
        results$stateAbbr <- stateAbbr_hopefully
        results$stateName <- c(state.name, 
                               c("District of Columbia", "Puerto Rico", "American Samoa", "Guam", "Northern Mariana Islands", "U.S. Minor Outlying Islands", "U.S. Virgin Islands"
                               ))[match(results$stateAbbr, c(
                                 state.abb,
                                 "DC", "PR", "AS", "GU", "MP", "UM", "VI" ) )]
        # fips2statename(fips_state_from_state_abbrev(results$stateAbbr)) # get full state name from 2-char abbreviation, if relying on EJAM pkg
        
      } else {
        warning("Unable to infer valid state abbreviations from the placename field provided by the API, so just returning whatever API provided as stateAbbr aka ST and stateName aka statename")
      }
    }
    ################################################# #
    return(results)
  }
  
  ################################################################################################################### #
  # brokeroutput is results of ejscreenRESTbroker()
  
  
  # # new 2023-07 format of json results:
  # 
  # str(x)
  # List of 10
  # $ url        : chr "https://ejscreen.epa.gov/mapper/ejscreenRESTbroker1.aspx?namestr=&geometry={\"spatialReference\":{\"wkid\":4326"| __truncated__
  # $ status_code: int 200
  # $ headers    :List of 8
  # ..$ cache-control            : chr "private"
  # ..$ content-type             : chr "application/json; charset=utf-8"
  # ..$ server                   : chr "Microsoft-IIS/10.0"
  # ..$ x-aspnet-version         : chr "4.0.30319"
  # ..$ x-powered-by             : chr "ASP.NET"
  # ..$ strict-transport-security: chr "max-age=31536000; includeSubDomains; preload"
  # ..$ date                     : chr "Mon, 24 Jul 2023 23:10:05 GMT"
  # ..$ content-length           : chr "9660"
  # ..- attr(*, "class")= chr [1:2] "insensitive" "list"
  # $ all_headers:List of 1
  # ..$ :List of 3
  # .. ..$ status : int 200
  # .. ..$ version: chr "HTTP/1.1"
  # .. ..$ headers:List of 8
  # .. .. ..$ cache-control            : chr "private"
  # .. .. ..$ content-type             : chr "application/json; charset=utf-8"
  # .. .. ..$ server                   : chr "Microsoft-IIS/10.0"
  # .. .. ..$ x-aspnet-version         : chr "4.0.30319"
  # .. .. ..$ x-powered-by             : chr "ASP.NET"
  # .. .. ..$ strict-transport-security: chr "max-age=31536000; includeSubDomains; preload"
  # .. .. ..$ date                     : chr "Mon, 24 Jul 2023 23:10:05 GMT"
  # .. .. ..$ content-length           : chr "9660"
  # .. .. ..- attr(*, "class")= chr [1:2] "insensitive" "list"
  # $ cookies    :'data.frame':	0 obs. of  7 variables:
  #   ..$ domain    : logi(0) 
  # ..$ flag      : logi(0) 
  # ..$ path      : logi(0) 
  # ..$ secure    : logi(0) 
  # ..$ expiration: 'POSIXct' num(0) 
  # ..$ name      : logi(0) 
  # ..$ value     : logi(0) 
  # $ content    : raw [1:9660] 7b 0d 0a 20 ...
  # $ date       : POSIXct[1:1], format: "2023-07-24 23:10:05"
  # $ times      : Named num [1:6] 0 0.115 0.127 0.139 2.687 ...
  # ..- attr(*, "names")= chr [1:6] "redirect" "namelookup" "connect" "pretransfer" ...
  # $ request    :List of 7
  # ..$ method    : chr "GET"
  # ..$ url       : chr "https://ejscreen.epa.gov/mapper/ejscreenRESTbroker1.aspx?namestr=&geometry={\"spatialReference\":{\"wkid\":4326"| __truncated__
  # ..$ headers   : Named chr "application/json, text/xml, application/xml, */*"
  # .. ..- attr(*, "names")= chr "Accept"
  # ..$ fields    : NULL
  # ..$ options   :List of 2
  # .. ..$ useragent: chr "libcurl/7.84.0 r-curl/5.0.0 httr/1.4.6"
  # .. ..$ httpget  : logi TRUE
  # ..$ auth_token: NULL
  # ..$ output    : list()
  # .. ..- attr(*, "class")= chr [1:2] "write_memory" "write_function"
  # ..- attr(*, "class")= chr "request"
  # $ handle     :Class 'curl_handle' <externalptr> 
  #   - attr(*, "class")= chr "response"
  
  
  ####################################################################################### # 
  
  #  just the data part:
  
  # Note it is slightly different if you request info on a blockgroup not a circular buffer, for example. 
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
  
}
