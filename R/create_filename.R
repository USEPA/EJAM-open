
#' create_filename - construct custom filename for downloads in EJAM app
#' @description Builds filename from file description, analysis title, buffer distance, and site selection method. Values are pulled from Shiny app if used there.
#' @param file_desc file description, such as "short report", "long report", "results_table"
#' @param title analysis title
#' @param buffer_dist buffer distance
#' @param site_method site selection method, such as NAICS, FRS, SHP, latlon
#' @param with_datetime boolean to include date and time
#' @param ext optional file extension. will check for '.' and add if not provided
#' @return Returns string of pasted filename with specified components
#'
#' @examples 
#' # specify all arguments
#' create_filename(file_desc = 'results_table', title = 'My Title', buffer_dist = 1, site_method = 'NAICS', with_datetime=TRUE, ext = '.xlsx')
#' # specify title only
#' create_filename(title = 'Summary of EJ Analysis')
#'
create_filename <- function(file_desc = '', title = '', 
                            buffer_dist = 0, site_method = '',
                            with_datetime = TRUE,
                            ext = NULL){
  
  ## separate character
  sep <- '_'
  ## base filename
  fname <- 'EJAM'
  
  ## add file description
  if (!is.null(file_desc) && nchar(file_desc) > 0) {
    fname <- paste(fname, gsub(' ', sep, file_desc), sep = sep)
  }
  ## add analysis title
  if (!is.null(title) && nchar(title) > 0) {
    fname <- paste(fname, gsub(' ', sep, title), sep = sep)
  }
  ## add buffer distance, if applicable
  if (!is.null(buffer_dist) && !is.na(buffer_dist) && buffer_dist > 0 ) {
    fname <- paste(fname, buffer_dist, 'Miles_from', sep = sep)
  }
  ## add site selection method
  if (!is.null(site_method) && nchar(site_method) > 0) {
    fname <- paste(fname, 'places_by', site_method, sep = sep)
  }
  
  ## add date and time of download
  if (with_datetime && !isTRUE(getOption("shiny.testmode"))) {
  fname <- paste(fname, format(Sys.time(),'%Y%m%d_%H%M%S'),
                 sep = sep)
  }
  ## add file extension
  if (!is.null(ext) && nchar(ext) > 0) {
    ## check for missing . at beginning
    if (!grepl('\\.', ext)) {
      fname <- paste0(fname,'.', ext)
    } else {
      fname <- paste0(fname, ext)
    }
  }
  
  return(fname)
}
  
