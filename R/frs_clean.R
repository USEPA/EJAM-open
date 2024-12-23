#' Clean Facility Registry Service (FRS) dataset when updating copy for use in EJAM
#' @details Used by [frs_get()]
#'   This renames some columns (lat, lon, NAICS are new names)
#'   and it drops rows lacking lat/lon location info in the LATITUDE83 or LONGITUDE83 columns
#' @param frs data.table that is the output of frs_read()
#' @param usefulcolumns optional, drops all columns except those in this vector of character colnames
#' @seealso   [frs_update_datasets()]
#' @return data.table with columns as defined by usefulcolumns parameter like REGISTRY_ID, and some renamed 
#'   to for example lat, lon, NAICS, SIC
#' 
frs_clean <- function(frs, usefulcolumns=c(
  'LATITUDE83', 'LONGITUDE83', 'REGISTRY_ID', 'PRIMARY_NAME', 'NAICS_CODES', 'SIC_CODES','PGM_SYS_ACRNMS'
)) {
  
  # REGISTRY_ID = 'c',
  # PRIMARY_NAME = 'c',   #   # frs$PRIMARY_NAME <- NULL # save space ?  825 MB VS 870 MB
  # PGM_SYS_ACRNMS = 'c',  # csv like  AIR:AK0000000201000026, AIRS/AFS:0201000026, NPDES:AK0020630, RCRAINFO:AK6690360312, RCRAINFO:AKR000206516"
  # INTEREST_TYPES = 'c', # csv like "AIR SYNTHETIC MINOR, ICIS-NPDES NON-MAJOR, UNSPECIFIED UNIVERSE"
  # NAICS_CODES = 'c',  # csv of NAICS
  # NAICS_CODE_DESCRIPTIONS = 'c', # csv?
  # SIC_CODES = 'c',  #csv
  # SIC_CODE_DESCRIPTIONS = 'c',  #csv
  # LATITUDE83 = 'd',   ###
  # LONGITUDE83 = 'd'
  
  frs <- frs[ , ..usefulcolumns]
  
  # Rename to column names used by this package ####
  setnames(frs, 'LATITUDE83', 'lat')
  setnames(frs, 'LONGITUDE83', 'lon')
  setnames(frs, 'NAICS_CODES', 'NAICS')
  setnames(frs, 'SIC_CODES', 'SIC')
  
  cat('Finished cleaning file.\n')
  commas <- function(x) {prettyNum(x,big.mark = ",")}
  # DROP ROWS LACKING lat/lon  ?####
  cat('Total rows: ', commas(NROW(frs)), '\n')  # >4 mill.
  # about 25% of all rows (870 MB vs 1160 MB)
  frs <- frs[!is.na(frs$lat), ]  
  cat('Rows with lat/lon: ', commas(NROW(frs)), '\n') # 3.4mill
  
  #   drop rows that are clearly INACTIVE sites 
  
  # done by frs_get()?
  # REGISTRY_ID <- NULL # to get rid of warning??
  data.table::setkey(frs, REGISTRY_ID)
  return(frs)
}
