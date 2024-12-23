#' Download, unzip, read, clean the Facility Registry Service dataset
#' @details Used by [frs_update_datasets()]
#' 
#'  Uses [frs_download()], [frs_unzip()], [frs_read()], [frs_clean()]
#' 
#'  **See examples for how package maintainer might use this.**
#'  
#'  See source code of this function for more notes.
#'  For a developer updating the frs datasets in this package, 
#'  see [frs_update_datasets()]  
#' 
#'  frs_get() invisibly returns the table of data, as a data.table.
#'  It will download, unzip, read, clean, and set metadata for the data. 
#' 
#'  This function gets the whole thing in one file from
#'  
#'  NATIONAL_SINGLE.CSV from 
#'  <https://ordsext.epa.gov/FLA/www3/state_files/national_single.zip>
#'  
#'  Other files and related information: 
#'  - <https://www.epa.gov/frs/frs-data-resources>
#'  - <https://www.epa.gov/frs/geospatial-data-download-service>
#'  - <https://www.epa.gov/frs/epa-frs-facilities-state-single-file-csv-download>
#'  - Also could download individual files from ECHO for parts of the info:
#'    <https://echo.epa.gov/tools/data-downloads/frs-download-summary> 
#'   for a description of other related files available from EPA's ECHO.
#'  
#'  This function creates the following:
#'
#' \preformatted{
#' 
#'  > head(frs_by_programid)
#'            lat        lon  REGISTRY_ID   program   pgm_sys_id
#'    1: 44.13415 -104.12563 110012799846     STATE        #5005
#'    2: 41.16163  -80.07847 110057783590 PA-EFACTS         ++++
#'    3: 41.21463 -111.96224 110020117862       CIM            0
#'    4: 29.62889  -83.10833 110040716473 LUST-ARRA            0
#'    5: 40.71490  -74.00316 110019246163       FIS 0-0000-01097
#'    6: 40.76395  -73.97037 110019163359       FIS 0-0000-01103
#'    
#'    > frs_by_naics[1:2, ]
#'            lat        lon  REGISTRY_ID NAICS
#'    1: 30.33805  -87.15616 110002524055     0
#'    2: 48.77306 -104.56154 110007654038     0
#'    
#'    > names(frs)
#'    "lat"    "lon"   "REGISTRY_ID" "PRIMARY_NAME" "NAICS" "PGM_SYS_ACRNMS"
#'    
#'     > head(frs[,1:4]) # looks something like this:
#'            lat       lon  REGISTRY_ID                    PRIMARY_NAME
#'    1: 18.37269 -66.14207 110000307695      xyz CHEMICALS INCORPORATED
#'    x: 17.98615 -66.61845 110000307784                         ABC INC
#'    x: 17.94930 -66.23170 110000307800                   COMPANY QRSTU
#'    
#'    
#'   **WHICH SITES ARE ACTIVE VS INACTIVE SITES**
#'  
#'  See frs_active_ids() or frs_inactive_ids()
#'  
#'  Approx 4.6 million rows total 10/2022.
#'  
#'  table(is.na(frs$lat))
#'  table(is.na(frs$NAICS))
#'  
#'  It is not entirely clear how to simply identify 
#'  which ones are active vs inactive sites. 
#'  See inst folder for notes on that. 
#'  This as of 2/10/23 is not exactly how ECHO/OECA defines "active" 
#'  
#'   **WHICH SITES HAVE LAT LON INFO**
#'   
#'  As of 2022-01-31:  Among all including inactive sites, 
#'  
#'   1/3 have no latitude or longitude.
#'   Even those with lat lon have some problems:
#'     Some are are not in the USA.
#'     Some have errors in country code.
#'     Some use alternate ways of specifying USA.
#'   
#'   **WHICH SITES HAVE NAICS OR SIC INDUSTRY CODES**
#'   
#'  Only 1/4 have both location and some industry code (27%)
#'  
#'  2/3 lack industry code (have no NAICS and no SIC).  
#'     NAICS vs SIC codes:
#'  11 percent have both NAICS and SIC, 
#'  9.5 percent have just NAICS = 
#'     (21 percent have NAICS). 
#'  12.5 percent have just SIC. 
#'  2/3 have neither NAICS nor SIC.
#'  
#'  
#'   **WHICH COLUMNS TO IMPORT AND KEEP**
#'  
#'  approx 39 columns if all are imported, but most useful 10 is default.
#'  
#'  [1] "REGISTRY_ID"             "PRIMARY_NAME"        "PGM_SYS_ACRNMS"         
#'  [4] "INTEREST_TYPES"    "NAICS_CODES"       "NAICS_CODE_DESCRIPTIONS"
#'  [7] "SIC_CODES"       "SIC_CODE_DESCRIPTIONS"  "LATITUDE83"             
#'  [10] "LONGITUDE83" 
#'  
#'       
#'   Some fields are csv lists actually, to be split into separate rows
#'    to enable queries on NAICS code or program system id:
#'       
#'   PGM_SYS_ACRNMS = 'c', # csv format like AIR:AK999, AIRS/AFS:123,
#'      NPDES:AK0020630, RCRAINFO:AK6690360312, RCRAINFO:AKR000206516"
#'   INTEREST_TYPES = 'c', # eg "AIR SYNTHETIC MINOR, ICIS-NPDES NON-MAJOR"
#'       NAICS_CODES = 'c',  # csv of NAICS
#' }
#'
#' @param only_essential_cols TRUE by default. used in frs_read()
#' @param folder NULL by default which means it downloads to and unzips in a temporary folder
#' @param downloaded_and_unzipped_already If set to TRUE, looks in folder for csv file
#'    instead of trying to download/unzip. Looks in working directory if folder not specified.
#' @param zfile filename, just use default unless EPA changes it 
#' @param zipbaseurl url, just use default unless EPA changes it 
#' @param csvname name of csv file. just use default unless EPA changes it 
#' @param date default is Sys.Date() which is today, but this is used as 
#'   an attribute assigned to the results, 
#'   representing the vintage, such as the date the frs was downloaded, obtained.
#' @seealso  [frs_update_datasets()] [frs_read()] [frs_clean()] frs_by_naics [frs_active_ids()]
#'   [frs_drop_inactive()] [frs_make_programid_lookup()] [frs_make_naics_lookup()]
#' @examples \dontrun{
#'   # These steps in the examples are all done by frs_update_datasets() 
#'     (a function not exported by the package)
#'   # Note these take a long time to run, for downloads and processing.
#'   frs <- frs_get() 
#'   # keep only if an active site, or unclear whether active. Remove clearly inactive ones.
#'   closedidlist <- frs_inactive_ids()
#'   frs <- frs_drop_inactive(frs, closedid = closedidlist) 
#'   frs_by_programid <- frs_make_programid_lookup(x = frs) # another super slow step 
#'   frs_by_naics     <- frs_make_naics_lookup(    x = frs) #  NAs introduced by coercion
#'   usethis::use_data(frs,              overwrite = TRUE)
#'   usethis::use_data(frs_by_programid, overwrite = TRUE)
#'   usethis::use_data(frs_by_naics,     overwrite = TRUE)
#' }
#' @importFrom utils download.file unzip
#'
frs_get <- function(only_essential_cols=TRUE, folder=NULL, downloaded_and_unzipped_already=FALSE,
                    zfile = 'national_single.zip', zipbaseurl = 'https://ordsext.epa.gov/FLA/www3/state_files/', 
                    csvname = 'NATIONAL_SINGLE.CSV', date = Sys.Date()) {
  # Note you can download individual files that have parts of the info but this function gets the whole thing in one file.
  # browseURL("https://echo.epa.gov/tools/data-downloads/frs-download-summary")
  #  describes the files available for download from EPA.
  # For example, Facilities (FRS_FACILITIES.csv) has these fields:
  #   Element Name	Data Type	Length	Long Name
  #   FAC_NAME	Char	200	Facility name
  #   FAC_STREET	Char	200	Street address
  #   FAC_CITY	Char	100	City
  #   FAC_STATE	Char	2	State abbreviation
  #   FAC_ZIP	Char	10	Postal ZIP code
  #   REGISTRY_ID	Char	36	FRS Registry ID [Primary Key]
  #   FAC_COUNTY	Char	100	County name
  #   FAC_EPA_REGION	Char	2	EPA Region Code
  #   LATITUDE_MEASURE	Num	22	Latitude Coordinate
  #   LONGITUDE_MEASURE	Num	22	Longitude Coordinate
  
  #    Download, unzip, read FRS from temporary files  ####
  if (!downloaded_and_unzipped_already) {
    if (is.null(folder)) {folder <- tempdir()} # keep track of where it is 
    # frsurl <- paste0(zipbaseurl, zfile) # 'https://ordsext.epa.gov/FLA/www3/state_files/national_single.zip'
    
    print(Sys.time()) 
    cat('Downloading... ')
    fullpath_zip <- frs_download(folder = folder, zipbaseurl = zipbaseurl, zfile = zfile)
    # cat("done downloading. ", fullpath_zip,' downloaded\n')
    print(Sys.time())
    
    cat('Unzipping...')
    fullpaths_extracted_to <- frs_unzip(zfile = zfile, folder = folder) # ???  fullpath_zip #
    if (!(fs::path_tidy(file.path(folder, csvname)) %in% fs::path_tidy(fullpaths_extracted_to) )) {
      stop(paste0(fullpaths_extracted_to, ' was result of unzip but expected ', file.path(folder, csvname)))
    }
  } else {
    if (is.null(folder)) {folder <- getwd()} # assume it is here if not specified
  }
  cat('done unzipping\n')
  print(Sys.time()) 
  
  cat('Reading... ')
  frs <- frs_read(fullpath = file.path(folder, csvname), only_essential_cols = only_essential_cols)
  # cat('done reading \n')
  print(Sys.time()) 
  
  cat('Cleaning via frs_clean()...  ')
  frs <- frs_clean(frs)  # drop if no latitude info, convert to data.table not tibble or simple data.frame
  # cat('cleaned\n')
  print(Sys.time()) 
  
  # add metadata as attributes
  attr(frs, 'download_date') <- date
  attr(frs, 'released') <- date
  # cat('\nTo use in package,  usethis::use_data(frs, overwrite=TRUE)  \n')
  # cat('Also see frs_make_naics_lookup() and frs_make_programid_lookup()\n')
  invisible(frs) 
  return(frs) # should not get here
  ######################################################################################################## #
  
  
  ######################################################################################################## #
  # notes:
  ######################################################################################################## #
  
  # system.time({myfrs3 <- data.table::fread(file = csvpath)}) # 26 seconds user. a data.table
  # system.time({myfrs1 <- frs_read(csvpath)}) #  50 seconds user. a tibble, so may want setDF(as.data.frame())
  # system.time({myfrs2 <- read.csv(csvpath)}) # 130 seconds user. a data.frame, so may want setDF( )
  
  # should not get here
  stop('see source code ')
  
}

