
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
#' @param envir if needed to specify environment other than default, e.g., globalenv() or parent.frame()
#' 
#' @keywords internal
#' @export
#'

download_latest_arrow_data <- function(
  varnames = .arrow_ds_names,
  repository = 'USEPA/ejamdata',
  envir = globalenv()
) {
  installed_data_folder <- app_sys('data')
  
  # Check if dataset(s) already loaded
  files_not_loaded <- sapply(varnames, function(v) !exists(v, envir = envir))
  if(!all(files_not_loaded)) return(NULL)
  
  message("Getting latest arrow release")
  github_token <- Sys.getenv("GITHUB_PAT", unset = Sys.getenv("GITHUB_TOKEN", unset = ""))

  latestArrowVersion <- piggyback::pb_releases(
    repo = repository,
    .token = github_token
  )[1, "tag_name"]
  ejamdata_version_fpath <- paste0(installed_data_folder,"/ejamdata_version.txt")
  if (!file.exists(ejamdata_version_fpath)) {
    usersArrowVersions <- NULL
  } else {
    usersArrowVersions <- readLines(ejamdata_version_fpath)
  }
  filenames <- paste0(varnames, ".arrow")
  full_paths <- file.path(installed_data_folder, filenames)
  missing_files <- filenames[!file.exists(full_paths)]
  
  # if user has latest release, check if any requested files are missing
  # if so, need to re-download (default to all files). Otherwise, all set
  if (isTRUE(usersArrowVersions == latestArrowVersion)) {
    if (length(missing_files) == 0) {
      message("Arrow-format datasets (blocks, etc.) are up-to-date -- locally-installed and latest-released data repository versions match.")
      return(NULL)
    } else {
      message("One or more arrow-format datasets (blocks, etc.) are missing. Downloading latest version from ", repository)
    }
  } 
  # If user installs for the first time, they won't have any arrow datasets or
  # the txt with the version, which is added at the end of this program
  else if(is.null(usersArrowVersions)) {
    message("Downloading latest arrow-format datasets (blocks, etc.)")
  } else {
    message(paste0("Arrow-format datasets (blocks, etc.) are out-of-date. Downloading latest versions from ", repository))
  }
  
  # otherwise, download the data from EJAM package's release assets
  piggyback::pb_download(
    file = missing_files,
    dest = installed_data_folder,
    repo = repository, 
    tag = "latest",
    use_timestamps = FALSE,
    .token = github_token
  )
  
  message("Finished downloading. Updating stored arrow version.")
  # update user's arrowversion
  writeLines(latestArrowVersion, ejamdata_version_fpath)
  message("Finished updating stored local version.")
}
