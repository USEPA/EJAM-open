

#' utility to show dir_tree of available files in testdata folders
#' See list of samples of input files to try in EJAM, and output examples from EJAM functions
#'
#' @param installed If you are a developer who has the local source package,
#' you can set this parameter to FALSE if you want to work with the
#' local source package version of the testdata folders
#' rather than the locally installed version.
#' @param pattern optional query expression, used as regexp passed to [fs::dir_ls()]
#' @param quiet set TRUE if you want to just get the path
#'   without seeing all the info in console and without browsing to the folder
#' @return path to local testdata folder comes with the EJAM package
#' @examples
#' testdata('shapes', quiet = T)
#' x <- testdata('shape', quiet = TRUE)
#' x[fs::is_file(x)]
#'
#' testdata('fips', quiet = T)
#' testdata('registryid', quiet = T)
#' testdata("address", quiet = T)
#'
#' # datasets as lazyloaded objects vs. files installed with package
#' 
#' topic = "fips"  # or "shape" or "latlon" or "naics" or "address" etc.
#' 
#' # datasets / R objects
#' cbind(data.in.package  = sort(grep(topic, EJAM:::datapack()$Item, value = T)))
#' 
#' # files
#' cbind(files.in.package = sort(basename(testdata(topic, quiet = T))))
#' 
#' @keywords internal
#' @export
#'
testdata <- function(pattern = NULL, installed = TRUE, quiet = FALSE) {

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
  info_to_print <- capture.output({
    testdata_folder <- testdatafolder(installed = installed)
  })
  if (!quiet && interactive()) {

    # show the full tree folders structure:
    # want to show tree of relevant folders only, using regex if relevant
    cat('\n')
    fs::dir_tree(testdata_folder, recurse = 1, regex = pattern)
    # ... gets passed to dir_ls() which has a param recurse = FALSE DEFAULT!
    # recurse=1 means go down only 1 level, type can be "any", "file", "directory", glob or regexp can be used too
    cat("\n")

    # show the info captured (path shown 3 ways)
    cat(paste0(info_to_print, collapse = "\n"), '\n')
    # cat(text_to_print, '\n') # redundant

    # open file explorer to view the (overall) folder
    browseURL(normalizePath(testdata_folder[1]))
  }

  # filter to show folder(s) that matched? not the files themselves?
  if (!is.null(pattern)) {
    matches <- fs::dir_ls(testdata_folder, regexp = pattern, ignore.case = TRUE, recurse = 1)
    # matches <- list.files(testdata_folder, pattern = pattern,
    #                       full.names = TRUE, recursive = TRUE, include.dirs = TRUE, ignore.case = TRUE)
  } else {
    matches <- testdata_folder
  }
  # show the matching folder(s) only...
  # matches is a vector of folders and or paths to files


  return(matches)
}
######################################################### #

#' utility to show path to testdata folders
#' see folder that has samples of input files to try in EJAM, and output examples from EJAM functions
#'
#' @param installed If you are a developer who has the local source package,
#' you can set this parameter to FALSE if you want to work with the
#' local source package version of the testdata folders
#' rather than the locally installed version.
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
