#' Read Facility Registry Service (FRS) dataset of EPA-regulated sites
#' @details 
#' This is just a helper function used to create the dataset for use in EJAM
#' 
#' \preformatted{
#'  Uses data.table::fread() 
#'  
#'  More than 4 million rows of data.
#'  
#'  See [frs_get()]  for more details on which fields might be useful.
#'  
#'  Default is just the most useful columns:
#'  
#'  [1] "REGISTRY_ID"             "PRIMARY_NAME"            "PGM_SYS_ACRNMS"         
#'  [4] "INTEREST_TYPES"          "NAICS_CODES"             "NAICS_CODE_DESCRIPTIONS"
#'  [7] "SIC_CODES"               "SIC_CODE_DESCRIPTIONS"   "LATITUDE83"             
#'  [10] "LONGITUDE83" 
#'  
#'  Full set of fields would be these:
#'  
#' [1] "FRS_FACILITY_DETAIL_REPORT_URL" "REGISTRY_ID"                    "PRIMARY_NAME"                  
#' [4] "LOCATION_ADDRESS"               "SUPPLEMENTAL_LOCATION"          "CITY_NAME"                     
#' [7] "COUNTY_NAME"                    "FIPS_CODE"                      "STATE_CODE"                    
#' [10] "STATE_NAME"                     "COUNTRY_NAME"                   "POSTAL_CODE"                   
#' [13] "FEDERAL_FACILITY_CODE"          "FEDERAL_AGENCY_NAME"            "TRIBAL_LAND_CODE"              
#' [16] "TRIBAL_LAND_NAME"               "CONGRESSIONAL_DIST_NUM"         "CENSUS_BLOCK_CODE"             
#' [19] "HUC_CODE"                       "EPA_REGION_CODE"                "SITE_TYPE_NAME"                
#' [22] "LOCATION_DESCRIPTION"           "CREATE_DATE"                    "UPDATE_DATE"                   
#' [25] "US_MEXICO_BORDER_IND"           "PGM_SYS_ACRNMS"                 "INTEREST_TYPES"                
#' [28] "NAICS_CODES"                    "NAICS_CODE_DESCRIPTIONS"        "SIC_CODES"                     
#' [31] "SIC_CODE_DESCRIPTIONS"          "LATITUDE83"                     "LONGITUDE83"                   
#' [34] "CONVEYOR"                       "COLLECT_DESC"                   "ACCURACY_VALUE"                
#' [37] "REF_POINT_DESC"                 "HDATUM_DESC"                    "SOURCE_DESC" 
#'  
#' }
#'  
#' @param fullpath path to output of frs_unzip
#' @param only_essential_cols whether to keep only a few columns needed for EJAM package (see source code)
#' 
#' @seealso [frs_update_datasets()] which uses [frs_get()] The main functions that get updates of the data for this package.
#' @return A data.table with columns as noted in details.
#'
frs_read <- function(fullpath= 'NATIONAL_SINGLE.csv', only_essential_cols = TRUE) {
  cat('This takes something like 30 seconds. please wait.\n')
  if (only_essential_cols) {
    # my_col_types <- readr::cols_only( # that was too slow
    my_col_types <-  list(
      REGISTRY_ID = 'c',
      PRIMARY_NAME = 'c',
      PGM_SYS_ACRNMS = 'c',  # csv like  AIR:AK0000000201000026, AIRS/AFS:0201000026, NPDES:AK0020630, RCRAINFO:AK6690360312, RCRAINFO:AKR000206516"
      INTEREST_TYPES = 'c', # csv like "AIR SYNTHETIC MINOR, ICIS-NPDES NON-MAJOR, UNSPECIFIED UNIVERSE"
      NAICS_CODES = 'c',  # csv of NAICS
      NAICS_CODE_DESCRIPTIONS = 'c', # csv?
      SIC_CODES = 'c',  #csv
      SIC_CODE_DESCRIPTIONS = 'c',  #csv
      LATITUDE83  = 'd',   ###
      LONGITUDE83 = 'd'
    )
    
  } else {
    
    my_col_types <- list(   # ****  fix this if using data.table::fread() not readr::read_csv()
      FRS_FACILITY_DETAIL_REPORT_URL = 'c',
      REGISTRY_ID = 'c',
      PRIMARY_NAME = 'c',
      LOCATION_ADDRESS = 'c',
      SUPPLEMENTAL_LOCATION = 'c',
      CITY_NAME = 'c',
      COUNTY_NAME = 'c',
      FIPS_CODE = 'c',   # can preserve leading zeroes but wastes space
      STATE_CODE = 'c',
      STATE_NAME = 'c',
      COUNTRY_NAME = 'c',
      POSTAL_CODE = 'c',
      FEDERAL_FACILITY_CODE = 'c',
      FEDERAL_AGENCY_NAME = 'c',
      TRIBAL_LAND_CODE = 'c',
      TRIBAL_LAND_NAME = 'c',
      CONGRESSIONAL_DIST_NUM = 'i',
      CENSUS_BLOCK_CODE = 'c', # can preserve leading zeroes but wastes space
      HUC_CODE = 'c',
      EPA_REGION_CODE = 'i',
      SITE_TYPE_NAME = 'c',  # not a huge percent of facilities have this code at all
      LOCATION_DESCRIPTION = 'c',
      
      CREATE_DATE = 'date', # readr::col_date(format = '%d-%b-%y'),
      UPDATE_DATE = 'date', # readr::col_date(format = '%d-%b-%y'),
      
      US_MEXICO_BORDER_IND = 'c',
      PGM_SYS_ACRNMS = 'c', # csv like  AIR:AK0000000201000026, AIRS/AFS:0201000026, NPDES:AK0020630, RCRAINFO:AK6690360312, RCRAINFO:AKR000206516"
      INTEREST_TYPES = 'c', # csv like "AIR SYNTHETIC MINOR, ICIS-NPDES NON-MAJOR, UNSPECIFIED UNIVERSE"
      NAICS_CODES = 'c',    # csv of NAICS
      NAICS_CODE_DESCRIPTIONS = 'c',   # csv?
      SIC_CODES = 'c',  #csv
      SIC_CODE_DESCRIPTIONS = 'c',  #csv
      LATITUDE83 = 'd',  ### ***         ###
      LONGITUDE83 = 'd', ### ***         ###
      CONVEYOR = 'c',
      COLLECT_DESC = 'c',
      ACCURACY_VALUE = 'd',
      REF_POINT_DESC = 'c',
      HDATUM_DESC = 'c',
      SOURCE_DESC = 'l'
    )
  }
  # fix these because when I wrote them out I used the shorthand readr::read_csv uses, but now using data.table::fread
  mynames <- names(my_col_types)
  my_col_types <-   gsub('^d$','double', 
                         gsub('^c$','character',
                                   gsub('^i$','integer',
                                        gsub('^l$','logical', my_col_types))))
 
  names(my_col_types) <- mynames # for fread, select param accepts a named vector specifying types
   frs <- data.table::fread(file = fullpath, select = my_col_types)
   REGISTRY_ID <- NULL # to prevent warnings it does not exist
  data.table::setkey(frs, REGISTRY_ID)
  cat('Finished reading file.\n')
  return(frs)
}
