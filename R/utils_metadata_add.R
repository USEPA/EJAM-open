
#' utility to see all attributes other than row.names
#'
#' @param x R object
#'
#' @return attributes of x, excluding row.names
#' @export
#' @keywords internal
#'
attributes2 = function(x) {
  att = attributes(x)
  return(att[names(att) != "row.names"])
}
#################################################### #

# helper function to update 1 attribute such as ONLY the EJAM VERSION metadata for ALL datasets in EJAM/data/
# AND NOT ARROW FILES AND NOT TXT FILES - ONLY THE .RDA FILES

metadata_update_attr <- function(x = datapack('EJAM')$Item, attr_name = "ejam_package_version", newvalue = desc::desc_get("Version")) {

  # x param is vector of names of data objects to update in the source package
  fnamesimplied = tolower(paste0(x, ".rda"))
  fnamesfound = tolower(dir("./data"))
  whichisrdafound = which(fnamesimplied %in% fnamesfound)
  x = x[whichisrdafound]
  # that should exclude ejamdata_version.txt
  #  and excludes all the .arrow files ! those should get metadata updated separately ?

  cat("newvalue is: \n")
  print(newvalue)

  for (i in 1:length(x))  {
    src = paste0("attr(", x[i], ", '", attr_name, "') <- newvalue")
    # e.g.,   attr(avg.in.us, "ejam_package_version") <- desc::desc_get("Version")
    print(src)
    eval(parse(text = src))
    src = paste0("usethis::use_data(", x[i], ", overwrite = TRUE)")
    print(src)
    eval(parse(text = src))
  }
  cat("datasets have been updated\n")
  return(NULL)
}

## important - unlike metadata_add or metadata_check, this has no output --
## it changes the data in the source folder
##
## to use it:

#   metadata_update_attr()
#################################################### #

#' helper function for package to set metadata attributes of a dataset
#'
#' @details
#' to update only the ejam_package_version attribute of every data item:
#'
#'   metadata_update_attr()
#'
#' @description Together with the metadata_mapping script, this can be used
#'  annually to update the metadata for datasets in a package.
#'  It just makes it easier to set a few metadata attributes similarly
#'  for a number of data elements, for example,
#'  to add new or update existing attributes.
#' @details This utility would be used in scripts in EJAM/data-raw/ to
#'   add metadata to objects like x before use_data(x, overwrite=T)
#' @param x dataset (or any object) whose metadata (stored as attributes) you want to update or create
#'  EJAM, EJScreen, and other dataset versions and release dates are tracked in DESCRIPTION
#'  @param update_date_saved_in_package set to FALSE to avoid changing this attribute
#' @seealso metadata_check()
#'
#' @return returns x but with new or altered attributes
#' @examples
#'   # metadata_check() # internal function
#'   x <- data.frame(a=1:10,b=1001:1010)
#'   # x <- metadata_add(x) # internal function
#'   attributes(x)
#'
#' @keywords internal
#'

# source("R/metadata_mapping.R")  # this already would get loaded via devtools::load_all() or library(EJAM)
# rstudioapi::documentOpen("./R/metadata_mapping.R")

metadata_add <- function(x, update_date_saved_in_package = TRUE) {

  metadata <- get_metadata_mapping(deparse(substitute(x)))
  if (is.null(metadata)) {
    txt <- paste0(paste0(names(metadata), "=", unlist(metadata)), collapse = ", ")
    message("metadata not specified, so used defaults from source code of this function: ", txt, "\n")
    # print(cbind(attributes = metadata))
    metadata <- get_metadata_mapping("default")
  }
  if (!is.list(metadata)) {stop('metadata has to be a named list')
    # return(NULL)
  }
  if(update_date_saved_in_package) {
    metadata$date_saved_in_package <- as.character(Sys.Date())
  }
  for (i in seq_along(metadata)) {
    attr(x, which = names(metadata)[i]) <- metadata[[i]]
  }

  invisible(x)
}
#################################################### #


#' helper function in updating the package metadata
#'
#' @description Quick and dirty helper during development, to check all the
#'   attributes of all the data files in relevant packages.
#'   It loads unloaded packages as needed, which you might not want it to do,
#'   but it is not coded to be able to check attributes without doing that.
#'
#' @param packages Optional. e.g. 'EJAMejscreendata', or can be a vector of character strings,
#'   and if not specified, default is to report on EJAM::ejampackages.
#'   If set to NULL, it only reports on objects already attached.
#'
#' @param which Optional vector (not list) of strings, the attributes.
#'   Default is some typical ones used in EJAM-related packages currently.
#' @param datasets optional, "all" means all data objects exported.
#'   Can be a vector of character names of the ones to check like c("bgpts", "blockpoints")
#' @param grepdatasets optional, if set to TRUE, datasets should be a query to use
#'   via grep to identify which datasets to check. It always uses ignore.case=TRUE for this.
#' @param loadifnotloaded Optional to control if func should temporarily attach packages not already loaded.
#' @seealso [functions_in_pkg()]
#' @examples
#'   # tail(metadata_check( ))
#'   metadata_check(packages = NULL)
#'
#'   x <- metadata_check("EJAM")
#'   x[x$has_metadata == TRUE, ]
#'   table(x$has_metadata)
#'
#' @keywords internal
#'
metadata_check <- function(packages = EJAM::ejampackages,
                           datasets = "all",
                           which = c(
                             "ejam_package_version",
                             "date_saved_in_package",
                             "date_downloaded",
                             "ejscreen_version",
                             "ejscreen_releasedate",
                             "acs_releasedate",
                             "acs_version",
                             "census_version"
                           ),
                           grepdatasets = FALSE,
                           loadifnotloaded = TRUE) {

  # > dput(default_metadata)
  # list(
  #   ejam_package_version = c(Version = "2.32.0"),
  #   ejscreen_version     = c(EJScreenVersion = "2.32"),
  #   ejscreen_releasedate = c(EJScreenReleaseDate = "2024-08-12"),
  #   acs_releasedate      = c(ACSReleaseDate = "2023-12-07"),
  #   acs_version          = c(ACSVersion = "2018-2022"),
  #   census_version       = c(CensusVersion = "2020")
  # )

  # utility to check if year attribute is set on each data file
  # does it really need to lazy load all these to check their attributes? that would be slow for big datasets, right?
  get1attribute <- function(x, which, dates_as_text=FALSE) {
    gotten <- try(get(x), silent = TRUE)
    if (inherits(gotten, "try-error")) {
      message("did not find ", x)
      attribute1 <- NA
    } else {
      attribute1 <- try(attr(gotten, which = which), silent = TRUE)
      if (is.null(attribute1) || inherits(attribute1, "try-error")) {
        attribute1 <- NA
      }
    }
    attribute1 <- as.vector(attribute1) # remove names
    if (dates_as_text && "Date" %in% class(attribute1)) {attribute1 <- as.character((attribute1))}
    return(attribute1)
  }
  ########################################### #   ########################################### #
  if (is.null(packages) || length(packages) == 0) {
    # if null or empty is specified, report on attached objects not what is in packages.

    if (datasets[1] == "all") {
      datasets <- ls(envir = globalenv())
    } else {
      if (grepdatasets) {
        datasets <- grep(datasets, ls(envir = globalenv()), ignore.case = TRUE, value = TRUE)
      } else {
        if (!(all(datasets %in% ls(envir = globalenv())))) {warning("Showing only the datasets found - some were not in globalenv()")}
        datasets <- datasets[datasets %in% ls(envir = globalenv())]
      }
    }
    # print(datasets); print('are the specified objects that were found in ls()')
    results <- data.frame(package = NA,
                          item = datasets,
                          matrix(NA, ncol = length(which), nrow = length(datasets)),
                          has_metadata = NA)
    colnames(results) <- c("package",
                           "item",
                           which,
                           "has_metadata")
    for (dn in seq_along(datasets)) {
      gotten <- try(get(datasets[dn]), silent = TRUE)
      if (inherits(gotten, "try-errror")) {
        whichall <- NA
      } else {
        whichall <- try(attributes(gotten), silent = TRUE)
        if (inherits(whichall, "try-error")) {
          whichall <- NA
        }
      }
      whichfound <- intersect(which, names(whichall))
      results[results$item == datasets[dn], whichfound]                <- unlist(whichall[whichfound])
      results[results$item == datasets[dn], "has_metadata"] <- any(!is.na(unlist(whichall[whichfound])))
    }
    return(results)
  }
  ########################################### #   ########################################### #
  allresults <- list()
  ii <- 0
  for (pkg in packages) {

    ii <- ii + 1
    if (!(pkg %in% as.vector(installed.packages()[, "Package"]))) {  # installed.packages() is slow but comprehensive
      cat(paste0(pkg, ' package not installed\n'))
      # packages <- packages[packages != pkg]
      # return a 1row data.frame with NA values, using the attributes listed in which as the colnames:
      #If package doesnt exist, return dataframe with appropriate NA values as per the test
      results <- data.frame(
        package = pkg,
        item = NA,
        matrix(NA, ncol = length(which), nrow = 1),
        has_metadata = FALSE
      )
      colnames(results) <- c("package", "item", which, "has_metadata")
      return(results)
    }
    ############################################### #
    # which datasets to check ####

    ## also see
    # EJAM:::functions_in_pkg(pkg = pkg, internal_included = TRUE, exportedfuncs_included = TRUE, data_included = TRUE)
    ## and
    # rdafiles <- datapack(pkg = pkg)$Item  # same thing as data(package = pkg)$results[ , "Item"]

    ## see also EJAM ::: #  datapack()
    # were_attached <- .packages()
    # were_loaded <- loadedNamespaces()

    rdafiles <- data(package = pkg)$results[ , "Item"]
    # problem: "ejamdata_version" %in% rdafiles TRUE but it is a .txt not .rda file, which causes error later
    if (datasets[1] != "all") {
      if (grepdatasets) {
        rdafiles <- grep(datasets, rdafiles, ignore.case = TRUE, value = TRUE)
      } else {
        if (!(all(datasets %in% rdafiles))) {warning("not all specified datasets were found")}
        rdafiles <- datasets[datasets %in% rdafiles]
      }
    }

    if (!isNamespaceLoaded(pkg) & loadifnotloaded) {
      wasnotloaded <- pkg
      cat(paste0(pkg, ' package was not loaded, loading and attaching now\n'))
      attachNamespace(pkg, include.only = rdafiles)   # library(pkg, character.only = TRUE)
    } else {
      wasnotloaded <- NULL
    }
    ############################################### #

    if (length(which) == 1) {    # this case was not working yet

      results <- cbind(sapply(rdafiles, FUN = get1attribute, which))
      colnames(results) <- which

      results$has_metadata <- FALSE
      rownames(results) <- "package not installed"
      allresults[[ii]] <- results
      allresults[[ii]] <- data.frame(package = pkg, item = rownames(results), allresults[[ii]])
      rownames(allresults[[ii]]) <- NULL

    } else {

      if (!isNamespaceLoaded(pkg) & loadifnotloaded) {
        wasnotloaded <- pkg
        cat(paste0(pkg, ' package was not loaded, loading and attaching now\n'))
        attachNamespace(pkg, include.only = rdafiles)   # library(pkg, character.only = TRUE)
      } else {
        wasnotloaded <- NULL
      }

      if (length(which) == 1) {

        results <- cbind(sapply(rdafiles, FUN = get1attribute, which, dates_as_text = TRUE))
        colnames(results) <- which

      } else {

        results <- list()
        for (i in 1:length(which)) {
          results[[i]] <- cbind(sapply(rdafiles, FUN = get1attribute, which[i], dates_as_text = TRUE))
        }
        results <- do.call(cbind, results)
        colnames(results) <- which
      }

      if (!is.null(wasnotloaded)) {
        unloadNamespace(asNamespace(wasnotloaded))
      }

      allresults[[ii]] <- results

      some <- as.vector(apply(allresults[[ii]], 1, function(z) !all(is.na(z))))
      allresults[[ii]] <- cbind(allresults[[ii]], has_metadata = FALSE)
      allresults[[ii]][some, "has_metadata"] <- TRUE
      allresults[[ii]] <- data.frame(package = pkg, item = rownames(allresults[[ii]]), allresults[[ii]])
      rownames(allresults[[ii]]) <- NULL
    }
  }

  names(allresults) <- packages

  cat(
    '\n
Also see

    x = EJAM:::functions_in_pkg(pkg = "EJAM",
      internal_included = FALSE, exportedfuncs_included = FALSE, data_included = TRUE)$object
    x[!grepl("^name", x)] \n
Also see \n
    y = datapack(pkg = "EJAM", simple = F)
    rdafiles =  y$Item   # same thing as data(package = pkg)$results[ , "Item"]
    \n'
  )

  # replace the NULL values with NA values,
  # and make each column just a vector instead of a list

  allresults <- do.call(rbind, allresults)

  for (mycol in 1:NCOL(allresults)) {
    allresults[, mycol] <- as.vector(unlist(allresults[, mycol] ))
  }

  rownames(allresults) <- NULL

  return(allresults)
}
