#' Download huge national COMBINED file and find which IDs are ACTIVE sites in FRS
#' @param active optional. If TRUE, default, returns the registry IDs of sites that seem to be active,
#'   or not obviously inactive, at least, based on closecodes. If FALSE, returns
#'   IDs of sites that are inactive.
#' @param closecodes optional, vector of values of ACTIVE_STATUS field assumed 
#'   to mean site is inactive
#' @param zfile optional Default is national_combined.zip
#'   which contains NATIONAL_ENVIRONMENTAL_INTEREST_FILE.CSV
#' @param zipbaseurl optional Default is <https://ordsext.epa.gov/FLA/www3/state_files> 
#'
#' @return vector of FRS IDs that seeem to be active (actually, not clearly inactive sites)
#'   assuming parameter active=TRUE, which is the default
#' @seealso [frs_inactive_ids()]  [frs_update_datasets()]
#'
#' @examples \dontrun{ # This takes a while for the download.
#'   # frs <- frs_get()
#'   # closedid <-  frs_active_ids(active=FALSE) # <- frs_inactive_ids()  
#'   # frs <- frs_drop_inactive(frs, closedid = closedid)
#'   # usethis::use_data(frs, overwrite = TRUE)
#'   }
frs_active_ids <- function(active=TRUE, closecodes = c(
  "CLOSED", 'PERMANENTLY CLOSED', 'PERMANENTLY SHUTDOWN', 'INACTIVE','TERMINATED','N',
  "RETIRED", "OUT OF SERVICE – WILL NOT BE RETURNED", "CANCELED, POSTPONED, OR NO LONGER PLANNED"),
  zfile = "national_combined.zip", zipbaseurl = 'https://ordsext.epa.gov/FLA/www3/state_files') {
  
  cat('Downloading national dataset to temp folder... Takes a couple of minutes! \n')
  mypath = frs_download(zfile = zfile, zipbaseurl = zipbaseurl)
  
  td = tempdir()
  utils::unzip(mypath, exdir = td)
  cat('Reading unzipped file...\n')
  y <- data.table::fread(file.path(td, 'NATIONAL_ENVIRONMENTAL_INTEREST_FILE.CSV'))
  commas <- function(x) {prettyNum(x,big.mark = ",")}
  REGISTRY_ID <- NULL # to get rid of warnings from RStudio and check package
  cat(paste0('Complete list of unique ids is ', commas(data.table::uniqueN(y$REGISTRY_ID)), ' out of ', commas(NROW(y)), ' rows of data.\n'))
  
  totalcount <- y[,.N]
  uniquecount <- data.table::uniqueN(y[,REGISTRY_ID])
  if (active) {
    ids <- data.table::unique(y[!(y$ACTIVE_STATUS %in% closecodes), REGISTRY_ID])
    activecount <- length(ids)
    inactivecount <- uniquecount - activecount
  } else {
    ids <- unique(y[ (y$ACTIVE_STATUS %in% closecodes), REGISTRY_ID])
    inactivecount <- length(ids)
    activecount <- uniquecount - inactivecount
  }
  cat(paste0('Count of    all REGISTRY_ID rows:   ', commas(totalcount), '\n'))
  cat(paste0('Count of unique REGISTRY_ID values: ', commas(uniquecount), '\n'))
  cat(paste0('  Clearly inactive unique IDs:      ', commas(inactivecount), '\n')) 
  cat(paste0('  Assumed   active unique IDs:      ', commas(activecount), '\n\n'))
  cat('Codes assumed to mean site is closed: \n' )
  cat(paste0(closecodes, collapse = ' \n '),'\n\n')
  
  return(ids)
}
####################################################################################### # 


#' Download national file and find which IDs are the INACTIVE sites in the FRS
#' @param active If FALSE, default, returns the registry IDs of sites that seem to be inactive,
#'   based on closecodes. 
#' @param ... passed to frs_active_ids()
#' @seealso [frs_active_ids()] [frs_update_datasets()]
#'
#' @return vector of FRS IDs that are the clearly inactive sites -- or all other sites -- 
#'   depending on value of active 
#'
frs_inactive_ids <- function(active=FALSE, ...) {
  frs_active_ids(active = active, ...)
}
