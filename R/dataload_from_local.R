
#' Load datasets from local disk folder
#'
#' Utility for analysts / developers to store large block /
#' other data locally instead of redownloading
#'
#' @details
#'
#'   See [dataload_dynamic()] also.
#'dataload_dynamic
#'   rm(bgid2fips, blockid2fips, blockpoints, blockwts, quaddata)
#'
#'   dataload_from_local(folder_local_source = '.')
#'
#' @param varnames use defaults, or vector of names like "bgej" or use "all" to get all available
#' @param envir  use defaults. see [dataload_dynamic()]
#' @param folder_local_source Your local folder path. see [dataload_dynamic()]
#' @param justchecking  use defaults. DEPRECATED
#' @param testing  use defaults
#' @param silent set to TRUE to stop cat() printing to console like when running tests
#' @param return_data_table whether the [read_ipc_file()] should return a data.table (T, the default), or arrow (F)
#' @return vector of paths to files (as derived from varnames) that were
#'   actually found in folder_local_source,
#'   but only for those not already in memory, so it is
#'   just the ones loaded from disk because not already in memory and found on disk locally.
#'
#' @export
#'
dataload_from_local <- function(varnames = .arrow_ds_names[1:3],
                                envir = globalenv(),  # should it be parent or global or package EJAM envt ??
                                folder_local_source = NULL, # './data/', # "~/../Downloads",
                                justchecking = FALSE,
                                testing = FALSE,
                                silent = FALSE,
                                return_data_table = TRUE) {

    if (is.null(folder_local_source)) {
      folder_local_source <- app_sys('data') # default for other development machines
    }

  if ('all' %in% tolower(varnames)) {
    varnames <- .arrow_ds_names
  }

  fnames     <- paste0(sub("_arrow","", varnames), ".arrow") # varnames are like bgid2fips, ext is .rda, fnames are like bgid2fips.rda
  
  localpaths  <- paste0(folder_local_source, '/', fnames)
  localpaths_found <- NULL
  # make output in console easier to read:
  if (length(varnames) > 1) {widest <- max(nchar(varnames))} else {widest <- max(10, nchar(varnames))}
  spacing <- sapply(1:length(varnames), function(x) paste0(rep(" ", widest - nchar(varnames[x])), collapse = ''))

  for (i in 1:length(fnames)) {

    varnames_i <- varnames[i]
    spacing_i  <-  spacing[i]

    if (!exists(varnames_i, envir = envir) ) {
      ################################################################ #
      # NOT in memory  ################################################################ #

      if (justchecking) {
        if (!silent) {cat(varnames_i, spacing_i,
            'NOT already in memory. ')}
      }

      if (file.exists(localpaths[i] )) {
        ##################### #
        #  ...FOUND on local drive ##################### #

        localpaths_found <- c(localpaths_found, localpaths[i])

        if (justchecking) {
          if (!silent) {cat(varnames_i, spacing_i,
              'is available locally on disk at', localpaths[i], '\n')}
        } else {
          # not justchecking
          if (!silent) {cat(varnames_i, spacing_i,
                            'is loading from local folder', '...')}
          # load it into the environment
          suppressWarnings(
            assign(
              varnames_i,
              arrow::read_ipc_file(
                file = localpaths[i],
                as_data_frame = return_data_table
              ),
              envir = envir
            )
          )
          suppressWarnings(
            eval(parse(text = paste0(varnames_i,'$metadata <- NULL')))
          )
          if (!exists(varnames_i, envir = envir)) {
            if (!silent) {cat(    "Problem - file found but failed to assign to memory/ envir: ", varnames_i, "\n")}
            warning("Problem - file found but failed to assign to memory/ envir: ", varnames_i)
          } else {
            if (!silent) {cat("done.\n")}
          }
        }
      } else {
        ##################### #
        # ...NOT on local disk ##################### #

        if (!silent) {cat(varnames_i, spacing_i,
            'is NOT found locally on disk at', localpaths[i], '\n')}
        next
      }

    } else {
      ################################################################ #
      #  FOUND in memory  ################################################################ #

      if (justchecking) {
        if (!silent) {cat(varnames_i, spacing_i,
            'was already in memory\n')} # redundant with similar lines in dataload_from_aws()

        if (file.exists(localpaths[i] )) {
          # in memory, AND on local disk ##################### #

          if (!silent) {cat(varnames_i, spacing_i,
              'was available locally on disk at', localpaths[i], '\n')}
        } else {
          # in memory, NOT on local disk ##################### #

          if (!silent) {cat(varnames_i, spacing_i,
              'is NOT found locally on disk at', localpaths[i], '\n')}
        }
        next
      } else {
        if (!silent) {cat(varnames_i, spacing_i,
            'was already in memory\n')}
      }

    }
    ################################################################ #
    ################################################################ #
  } # end of loop

  return(localpaths_found)
}
