
#' Download latest versions of arrow datasets if user doesn't have them already
#'
#' Used when EJAM package is attached
#' @details
#'   Checks to see what is the latest version of datasets available according to a repository's latest release tag.
#'   Compares that to what version was last saved locally (as stored in the installed package's 
#'   ejamdata_version.txt file). 
#'   
#'   Relies on [piggyback::pb_releases()] to download data files from a specific release (version) of the package.
#' 
#' @param varnames use defaults, or vector of names like "bgej" or use "all" to get all available
#' @param repository repository name such as "USEPA/ejamdata"
#' 
#' @keywords internal
#' @export
#'
download_latest_arrow_data <- function(
    varnames = c(
      "blockpoints", 
      "blockwts", 
      "quaddata",
      "bgej",
      "bgid2fips",
      "blockid2fips",
      "frs",
      "frs_by_mact",
      "frs_by_naics",
      "frs_by_programid",
      "frs_by_sic"
    ), 
    repository = 'USEPA/ejamdata') {
  
  installed_data_folder <- app_sys('data') # same as system.file('data', package = "EJAM") unless name of package changes.
  
  latestArrowVersion <- piggyback::pb_releases(
    repo = repository,
    .token = ""
  )[1, "tag_name"]
  ejamdata_version_fpath <- paste0(installed_data_folder,"/ejamdata_version.txt")
  if (!file.exists(ejamdata_version_fpath)) {
    usersArrowVersions <- NULL
  } else {
    usersArrowVersions <- readLines(ejamdata_version_fpath)
  }
  filenames <- paste0(varnames, ".arrow")
  
  # if user has latest release, check if any requested files are missing
  # if so, need to re-download (default to all files). Otherwise, all set
  if (isTRUE(usersArrowVersions == latestArrowVersion)) {
    message("Arrow-format datasets (blocks, etc.) are up-to-date -- locally-installed and latest-released data repository versions match.")
    
    full_paths <- file.path(installed_data_folder, filenames)
    
    missing_files <- filenames[!file.exists(full_paths)]
    if (length(missing_files) == 0) {
      return(NULL)
    }
    message(paste0("However, some files are missing. Downloading them from the data repository at ", repository))
  } else {
    message(paste0("Arrow-format datasets (blocks, etc.) are out-of-date. Newer versions are available at ", repository))
    missing_files <- filenames
  }
  
  # otherwise, download the data from EJAM package's release assets
  piggyback::pb_download(
    file = missing_files,
    dest = installed_data_folder,
    repo = repository, 
    tag = "latest",
    use_timestamps = FALSE,
    .token = ""
  )
  
  message("Finished downloading. Updating stored local version.")
  # update user's arrowversion
  writeLines(latestArrowVersion, ejamdata_version_fpath)
  message("Finished updating stored local version.")
}
