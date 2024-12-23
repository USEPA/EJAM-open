

#' utility to show dir_tree of available files in testdata folders
#' See list of samples of input files to try in EJAM, and output examples from EJAM functions
#'
#' @param installed If you are a developer who has the local source package, 
#' you can set this parameter to FALSE if you want to work with the
#' local source package version of the testdata folders
#' rather than the locally installed version.
#'
#' @return path to local testdata folder comes with the EJAM package
#' 
#' @keywords internal
#' @export
#'
testdata <- function(installed = TRUE) {
  
  if (installed) {
    # testdata_folder <- system.file('testdata', package = 'EJAM')
    # text_to_print <- "system.file('testdata', package = 'EJAM')" # redundant
  } 
  
  if (!installed & file.exists("DESCRIPTION")) {
    # testdata_folder <- "./inst/testdata"
    # text_to_print <-  '"./inst/testdata"' # redundant
  }
  if (!installed & !file.exists("DESCRIPTION")) {
    warning('testdata(installed = F) can only be used while working directory is the root of a source package - showing testdata(installed = F) instead')
    return(testdata(TRUE))
  }
  
  # get path, but side effect is printing path in 3 formats, and prefer to show that after the tree
  cat('\n')
  info_to_print <- capture.output({
    testdata_folder <- testdatafolder(installed = installed)
  })
  
  # show the full tree folders structure:
  fs::dir_tree(testdata_folder, 1)
  cat("\n")
  
  # show the info captured (path shown 3 ways)
  cat(paste0(info_to_print, collapse = "\n"), '\n')
  # cat(text_to_print, '\n') # redundant
  
  # open file explorer to view the folder  
  browseURL(testdata_folder)
  
  return(testdata_folder)
}
######################################################### #

#' utility to show path to testdata folders
#' see folder that has samples of input files to try in EJAM, and output examples from EJAM functions
#' 
#' @param installed If you are a developer who has the local source package, 
#' you can set this parameter to FALSE if you want to work with the
#' local source package version of the testdata folders
#' rather than the locally installed version.
#'
#' @return path to local testdata folder comes with the EJAM package
#' 
#' @keywords internal
#' @export
#'
testdatafolder = function(installed = TRUE) {
  
  if (installed) {
    testdata_folder_shortcode_text <- "system.file('testdata', package = 'EJAM')"
    testdata_folder_shortcode_sourceable      <- "system.file('testdata', package = 'EJAM')"
  } else {
    testdata_folder_shortcode_text <- "'./inst/testpath'" # shortest 
    testdata_folder_shortcode_sourceable      <- "file.path(getwd(), 'inst/testdata')" # or  "path.expand('./inst/testdata')" # just so source() returns './inst/testdata' )
  }
  
  testdata_folder <- source_this_codetext(testdata_folder_shortcode_sourceable)
  rpath <- gsub('\\\\', '/', normalizePath(testdata_folder)) # only needed if !installed but ok if installed
  
  cat('\n')
  cat('#  code that returns the path \n')
  cat(testdata_folder_shortcode_text, '\n')
  cat('\n')
  
  cat('#  the path as formatted by normalizePath() \n')
  print(normalizePath(testdata_folder))
  cat("\n")
  
  cat('#  the path in R format (also returned invisibly), shown here unquoted \n')
  cat(rpath, '\n')
  cat('\n')
  
  invisible(testdata_folder)
}
######################################################### #
