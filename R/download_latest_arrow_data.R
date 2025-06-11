
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
#' @param repository repository name such as `r EJAM:::repo_from_desc('github.io', get_full_url = T)` or "XYZ/ejamdata"
#'   (wherever the ejamdata repo is hosted, as specified in the DESCRIPTION file of this package)
#' @param envir if needed to specify environment other than default, e.g., globalenv() or parent.frame()
#' 
#' @keywords internal
#' @export
#'
download_latest_arrow_data <- function(
    varnames = .arrow_ds_names, 
    repository = NULL,
    envir = globalenv()
) {
  
  if (missing(repository) || is.null(repository)) {
    repository <- as.vector(desc::desc(file = system.file("DESCRIPTION", package = "EJAM"))$get("ejam_data_repo"))
  }
  installed_data_folder <- app_sys('data')
  filenames <- paste0(varnames, ".arrow")
  full_paths <- file.path(installed_data_folder, filenames)
  missing_files <- filenames[!file.exists(full_paths)]
  
  # Check if dataset(s) already loaded
  files_not_loaded <- sapply(varnames, function(v) !exists(v, envir = envir))
  if (!all(files_not_loaded)) return(NULL)
  
  if (offline_cat()) {
    # message("No internet connection seems to be available!")
    if (length(missing_files) == 0) {
      warning("Arrow-format datasets (blocks, etc.) files were all found, but no internet connection seems to be available, so cannot check if files are the latest version available online!")
    } else {
      warning("One or more arrow-format datasets (blocks, etc.) are missing, but no internet connection seems to be available, so cannot downloading missing files!")
    }
    return(NULL)
  }
  
  # get arrow data version in repo vs. user's version
  github_token <- Sys.getenv("GITHUB_PAT", unset = Sys.getenv("GITHUB_TOKEN", unset = ""))
  
  # check that it's valid
  if (nzchar(github_token)) {
    token_is_valid <- tryCatch(
      {
        gh::gh("GET /user", .token = github_token)
        message("\u2705 Token is valid!")
        TRUE
      },
      error = function(e) {
        message("\u274C Token is invalid or expired. Resetting...")
        FALSE
      }
    )
    if (!token_is_valid) github_token = ""
  }
  
  # get latest release to determine if user has latest versions
  
  latestArrowVersion <- tryCatch({piggyback::pb_releases(
    repo = repository,
    .token = github_token
  )[1, "tag_name"]},
  error = function(e) {
    message(paste0("\u274C Failed trying to get info from github repository ", repository, " about latest release..."))
    FALSE
  })
  
  ejamdata_version_fpath <- paste0(installed_data_folder,"/ejamdata_version.txt")
  
  if (!file.exists(ejamdata_version_fpath)) {
    usersArrowVersions <- NULL
  } else {
    usersArrowVersions <- readLines(ejamdata_version_fpath)
  }
  
  # if user has latest release, check if any requested files are missing
  # if so, need to re-download (default to all files). Otherwise, all set
  if (isTRUE(usersArrowVersions == latestArrowVersion)) {
    # filenames <- paste0(varnames, ".arrow")
    # full_paths <- file.path(installed_data_folder, filenames)
    # missing_files <- filenames[!file.exists(full_paths)]
    
    if (length(missing_files) == 0) {
      message("Arrow-format datasets (blocks, etc.) are up-to-date -- locally-installed and latest-released data repository versions match.")
      return(NULL)
    } else {
      message("One or more arrow-format datasets (blocks, etc.) are missing. Downloading latest version from this github repository: ", repository)
    }
  } else {
    # If user installs for the first time, they won't have any arrow datasets or
    # the txt with the version, which is added at the end of this program
    
    missing_files <- varnames
    if (is.null(usersArrowVersions)) {
      message("Downloading latest arrow-format datasets (blocks, etc.)")
    } else {
      message(paste0("Arrow-format datasets (blocks, etc.) are out-of-date. Downloading latest versions from this github repository: ", repository))
    }
  }
  
  # otherwise, download the data from EJAM package's release assets
  tried <- tryCatch({
    piggyback::pb_download(
      file = missing_files,
      dest = installed_data_folder,
      repo = repository, 
      tag = "latest",
      use_timestamps = FALSE,
      .token = github_token
    )},
    error = function(e) {
      message(paste0("\u274C Failed trying to get datasets from github repository ", repository, " about latest release..."))
      FALSE
    }
  )
  message(paste0("Finished downloading latest versions of datasets."))
  
  # update user's arrowversion
  message("Writing updated info about what versions of arrow datasets are saved locally...")
  tried <- tryCatch({
    writeLines(latestArrowVersion, ejamdata_version_fpath)},
    error = function(e) {
      message(paste0("\u274C Failed to write (updated info about what versions of arrow datasets are saved locally) to file ", ejamdata_version_fpath, " -- check permissions..."))
      FALSE
    }
  )
  return(NULL)
}
