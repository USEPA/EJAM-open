#' Download Facility Registry Service (FRS) file national_single.zip
#' @details  
#'   See <https://www.epa.gov/frs/epa-frs-facilities-state-single-file-csv-download>
#'   
#'   and <https://echo.epa.gov/tools/data-downloads/frs-download-summary>
#' @param folder path Default is NULL which means it is downloaded to a temp folder.
#' @param zfile filename
#' @param zipbaseurl url
#' @return The full path and file name of the downloaded zip file
#'
#' @seealso Used by [frs_update_datasets()] which uses [frs_get()]  
#' 
frs_download <- function(folder=NULL, 
                         zfile = 'national_single.zip', 
                         zipbaseurl = 'https://ordsext.epa.gov/FLA/www3/state_files/') {
  
  options(timeout = max(300, getOption("timeout")))
  ##   Complete FRS file is described here:
  ##   https://www.epa.gov/frs/epa-frs-facilities-state-single-file-csv-download
  if (is.null(folder)) {folder <- tempdir()} 
  fullpath_zip <- file.path(folder, zfile)
  cat('Trying to download from ', file.path(zipbaseurl, zfile), ' to save as ', fullpath_zip, '\n')
  problemcodes <- download.file(url = file.path(zipbaseurl, zfile), destfile = fullpath_zip)
  if (problemcodes == 0) {
    cat('Finished download to ', fullpath_zip, '\n')
    return(fullpath_zip)
  } else {
    stop(paste0('download failed with code', problemcodes))
  }
}
