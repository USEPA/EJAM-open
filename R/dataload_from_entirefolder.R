
#' dataload_from_entirefolder
#' 
#' Loads into global environment all .rda files found in specified folder
#' @param folder path
#'
#' @return nothing. just loads to global envt
#' 
#' @keywords internal
#'
dataload_from_entirefolder <- function(folder="./data") {
  
  for (fname in list.files(folder, pattern = ".rda")) {
    cat("loading ", file.path(folder, fname), '\n')
    if (!exists(gsub(".rda", "", fname), envir = globalenv())) {
      load(file.path(folder, fname), envir = globalenv())
    }
  }
}
