#' Unzip Facility Registry Service dataset
#' @details helper function used by frs_get() to create dataset for EJAM
#'
#' @param zfile zipfile obtained via frs_download
#' @param folder optional, folder to look in for zfile
#' @param ... passed to unzip
#'
#' @seealso [frs_upate_datasets()] which uses [frs_get()] which uses [frs_download()] [frs_unzip()] [frs_read()] [frs_clean()]
#'
#'
frs_unzip <- function(zfile='national_single.zip', folder='.', ...) {
  if (!(file.exists(file.path( folder, zfile) ))) {
    stop(paste0('cannot find ', file.path( folder, zfile)))
  }
  fullpaths_extracted_to <- utils::unzip( zipfile = file.path( folder, zfile), exdir = folder, ...)
  return(fullpaths_extracted_to)
}
