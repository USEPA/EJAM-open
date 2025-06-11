
#' Utility to download / load datasets 
#'
#' @details
#'   First checks memory, then installed package's data folder
#'   When package is first loaded, all arrow files are downloaded from a repository called ejamdata
#'   into package's data directory
#'
#' @param varnames character vector of names of R objects to get from board,
#'   or set this to "all" to load all of them
#' @param envir if needed to specify environment other than default, e.g., globalenv() or parent.frame()
#' @param folder_local_source path of local folder to
#'   look in for locally saved copies
#' @param silent set to TRUE to suppress cat() msgs to console
#' @param return_data_table whether the [read_ipc_file()] should return a data.table (T, the default), or arrow (F). Passed to [dataload_from_local()]
#' @param onAttach Indicates whether the function is being called from onAttach. IF so, it will download all arrow files if necessary
#'
#' @return
#'
#'   returns vector of names of objects now in memory in specified envir, either because
#'
#'   1) already in memory or
#'
#'   2) loaded from local disk or
#'
#'   3) successfully downloaded.
#'
#' @export
#'
dataload_dynamic <- function(
    varnames = .arrow_ds_names[1:3],
    envir = globalenv(),
    folder_local_source = NULL, # './data/', # or "~/../Downloads"
    silent = FALSE,
    return_data_table = TRUE,
    onAttach = FALSE) {
  
  message(paste0("Loading arrow datasets: ", paste(varnames, collapse = ", ")))
  
  ####################################################### #
  # make sure varnames are specified correctly
  if (!all(is.character(varnames))) {
    ok = FALSE
    varnames = deparse(substitute(varnames))
    if (all(sapply(varnames, exists))) {
      if (interactive()) {
        ok <- askYesNo(paste0("looks like you provided unquoted object names ... do you mean '", varnames[1],"' etc.?"))
        if (is.na(ok)) {ok <- FALSE} # clicked cancel
        warning("varnames must be a character vector of quoted names of objects like c('x', 'y') \n")
        if (!ok) {return(NULL)}
      }}
    if (!ok) {
      stop("varnames must be a character vector of quoted names of objects like c('x', 'y') ")
    }}
  
  if ('all' %in% tolower(varnames)) {
    varnames <- .arrow_ds_names
  }
  
  ####################################################### #
  # try downloading datasets ####
  # download all if loading EJAM, otherwise only those requested
  # the download function will first check if they're already downloaded.
  
  if (onAttach) message("Finding or downloading all arrow files: ", paste0(if (onAttach) .arrow_ds_names else varnames, collapse = ", "))
  
  download_latest_arrow_data(
    varnames = if (onAttach) .arrow_ds_names else varnames,
    envir = envir
  )
  
  ####################################################### #
  # make output in console easier to read:
  if (length(varnames) > 1) {
    widest <- max(nchar(varnames))
  } else {
    widest <- max(10, nchar(varnames))
  }
  spacing <- sapply(1:length(varnames), function(x) {
    paste0(rep(" ", widest - nchar(varnames[x])), collapse = '')
  })

  ####################################################### #
  # first change varnames if requesting arrow version, rather than DT
  if (!return_data_table) varnames <- paste0(varnames, "_arrow")
  
  ####################################################### #
  # check memory
  message(paste0("looking for ", paste(varnames, collapse = ', '), " in memory..."))
  files_loaded <- sapply(varnames, function(v) exists(v, envir = envir))
  if (all(files_loaded)) return(NULL)
  
  ####################################################### #
  # get files from installed package's data folder (where they were downloaded)
  files_not_loaded <- setdiff(varnames, files_loaded)
  dataload_from_local(
    files_not_loaded, 
    folder_local_source = folder_local_source, 
    envir = envir, 
    return_data_table = return_data_table,
    silent = silent
  )
  if (!silent) {cat("\n")}
  
  return(varnames)
}
