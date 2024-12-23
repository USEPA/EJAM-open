
#' Load datasets from local disk folder
#'
#' Utility for analysts / developers to store large block /
#' other data locally instead of redownloading
#'
#' @details
#'
#'   See [dataload_from_pins()] also.
#'
#'   rm(bgid2fips, blockid2fips, blockpoints, blockwts, quaddata)
#'
#'   dataload_from_local(folder_local_source = '.')
#'
#' @param varnames use defaults, or vector of names like "bgej" or use "all" to get all available
#' @param ext  use defaults
#' @param fun  use defaults
#' @param envir  use defaults. see [dataload_from_pins()]
#' @param folder_local_source Your local folder path. see [dataload_from_pins()]
#' @param justchecking  use defaults. see [dataload_from_pins()]
#' @param testing  use defaults
#' @param silent set to TRUE to stop cat() printing to console like when running tests
#' @return vector of paths to files (as derived from varnames) that were
#'   actually found in folder_local_source,
#'   but only for those not already in memory, so it is
#'   just the ones loaded from disk because not already in memory and found on disk locally.
#'
#' @export
#'
dataload_from_local <- function(varnames = c(c('blockwts', 'blockpoints', "quaddata"),
                                             # load only if /when needed:
                                             'bgej',
                                             'bgid2fips', 'blockid2fips', 
                                             c('frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact")
                                             )[1:3],
                                ext = c(".arrow", ".rda")[1],
                                fun = c("arrow::read_ipc_file", "load")[1],
                                envir = globalenv(),  # should it be parent or global or package EJAM envt ??
                                folder_local_source = NULL, # './data/', # "~/../Downloads",
                                justchecking = FALSE,
                                testing = FALSE,
                                silent = FALSE) {

  if (substr(Sys.getenv("COMPUTERNAME"),1,6) == "LZ11MC") {
    # during development, when on this particular computer, use this particular local folder to look in for datasets:
    folder_local_source <- "~/../Downloads/EJAMbigfiles"
  } else {
    if (is.null(folder_local_source)) {
      folder_local_source <- EJAM:::app_sys('data') # default for other development machines
    }
  }

  if ('all' %in% tolower(varnames)) {
    varnames <- c(
      c('blockwts', 'blockpoints', 'blockid2fips', "quaddata"),
      'bgej',
      'bgid2fips',
      c('frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact")
    )
  }

  # if (interactive()) {
  if (!is.character(fun)) {
    warning('must specify function in fun parameter as a quoted character string')
    return(NULL)
  }
  if (length(ext) > 1)    {
    warning('must specify only one file extension for all the files')
    return(NULL)
  }
  if (ext == "arrow") ext <- ".arrow"
  if (ext == "rda")   ext <- ".rda"
  if ((ext == '.arrow') & missing(fun)) {fun <- "arrow::read_ipc_file"}

  fnames     <- paste0(varnames, ext) # varnames are like bgid2fips, ext is .rda, fnames are like bgid2fips.rda
  # objectnames <- paste0(mybucketfolder,      '/', fnames) # EJAM/bgid2fips.rda
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

          if (ext == '.rda') {
            if (!silent) {cat(varnames_i, spacing_i,
                 'is loading from', localpaths[i],'...')}
            load(localpaths[i], envir = envir)
            if (!exists(varnames_i, envir = envir)) {
              if (!silent) {cat(    "Problem - file found but failed to assign to memory/ envir: ", varnames_i, "\n")}
              warning("Problem - file found but failed to assign to memory/ envir: ", varnames_i)
            } else {
              if (!silent) {cat("done.\n")}
            }
            next
          } else {
            if (ext == '.arrow') {
              if (!silent) {cat(varnames_i, spacing_i,
                   'is loading from', localpaths[i],'...')}
              assign(varnames_i, arrow::read_ipc_file(file = localpaths[i]), envir = envir)
              if (!exists(varnames_i, envir = envir)) {
                if (!silent) {cat(    "Problem - file found but failed to assign to memory/ envir: ", varnames_i, "\n")}
                warning("Problem - file found but failed to assign to memory/ envir: ", varnames_i)
              } else {
                if (!silent) {cat("done.\n")}
              }
            } else {
              warning('ext must be .arrow or .rda')
              return(NULL)
            }
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

  # if (!silent) {cat("\n")}
  return(localpaths_found)
  # } else {
  # message("Must be in interactive mode not on server to load from local disk using dataload_from_local()")
  # }
}
