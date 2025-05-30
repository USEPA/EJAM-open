
#' UTILITY - DRAFT - See names and size of data sets in installed package(s) - internal utility function
#'
#' Wrapper for data() and can get memory size of objects
#' @details do not rely on this much - it was a quick utility.
#'   It may create and leave objects in global envt - not careful about that.
#' @param pkg a character vector giving the package(s) to look in for data sets
#' @param len Only affects what is printed to console - specifies the
#'   number of characters to limit Title to, making it easier to see in the console.
#' @param sortbysize if TRUE (and simple=F),
#'   sort by increasing size of object, within each package, not alpha.
#' @param simple FALSE to get object sizes, etc., or
#'    TRUE to just get names in each package, like
#'    `data(package = "EJAM")$results[, c("Package", 'Item')]`
#' @return If simple = TRUE, data.frame with colnames Package and Item.
#'   If simple = FALSE, data.frame with colnames Package, Item, size, Title.Short
#' @examples
#'  # see just a vector of the data object names
#'  data(package = "EJAM")$results[, 'Item']
#'
#'  # not actually sorted within each pkg by default
#'  datapack()
#'  # not actually sorted by default
#'  datapack("EJAM")$Item
#'  ##datapack("MASS", simple=T)
#'
#'  # sorted by size if simple=F
#'  ##datapack("datasets", simple=F)
#'  x <- datapack(simple = F)
#'  # sorted by size already, to see largest ones among all these pkgs:
#'  tail(x[, 1:3], 20)
#'
#'  # sorted alphabetically within each pkg
#'  x[order(x$Package, x$Item), 1:2]
#'  # sorted alphabetically across all the pkgs
#'  x[order(x$Item), 1:2]
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
#'
datapack <- function(pkg = 'EJAM', len=30, sortbysize=TRUE, simple = TRUE) {

  if (simple) {
    cat('Get more info with datapack(simple = FALSE)\n\n')
    out <- data.frame(data(package = pkg)$results[, c('Package', 'Item')])
    if (sortbysize) {cat('ignoring sortbysize because simple=TRUE \n\n')}
    return(out)
  }

  n <- length(pkg)
  ok <- rep(FALSE, n)
  for (i in 1:n) {
    ok[i] <- 0 != length(find.package(pkg[i], quiet = TRUE))
  }
  pkg <- pkg[ok] # available in a library but not necessarily attached ie on search path, in print(.packages()) or via search()
  # pkg_on_search_path <- pkg %in%  .packages() # paste0('package:', pkg) %in% search()
  were_attached <- .packages()
  were_loaded <- loadedNamespaces()

  ## commented out the on.exit section as it was causing namespace/search path issues with some of the EJAM dependencies
  # on.exit({
  #   #  get it back to those which had been attached and or loaded before we started this function
  #   wasnotherebefore <- setdiff(.packages(), were_attached)
  #   for (this in wasnotherebefore) {
  #     # for (this in pkg[!(pkg %in% were_attached)]) {
  #     detach(paste0("package:", this), unload  = FALSE, force = TRUE, character.only = TRUE)
  #   }
  #   wasnotloadedbefore <- setdiff(loadedNamespaces(), were_loaded)
  #   for (this in wasnotloadedbefore) {
  #     unloadNamespace(this)
  #   }
  # })

  if (length(pkg) == 0) {
    return(NULL)
  } else {

    zrows <- as.data.frame(data(package = pkg)$results) # this works for multiple packages all at once

    # THIS PART ONLY WORKS ON ONE PACKAGE AT A TIME I THINK

    cat('Loading all packages .... please wait... \n')
    for (this in pkg) {
      library(this, character.only = TRUE, quietly = TRUE)
      # otherwise they are not attached/on search path and cannot use get() to check object size, etc.
    }

    zrows$size = sapply(
      #objects(envir = as.environment( paste0("package:", pkg) )),
      zrows$Item,
      function(object_x) {

        # for each item in any package:
        thispkg <- zrows$Package[match(object_x, zrows$Item)]
        if (!exists(object_x, envir = as.environment(paste0("package:", thispkg) ) )) {
          cat('cannot find ', object_x, ' in ', thispkg, ' via exists() so trying to use data() \n')
          #return(0)
          data(object_x, envir = as.environment(paste0("package:", thispkg) ) )
        } # in case supposedly data but not lazy loaded per DESCRIPTION
        if (!exists(object_x, envir = as.environment(paste0("package:", thispkg) ) )) {
          cat('tried loading  ', object_x,' via data() but failed \n')
          subpart = gsub(' .*', '', object_x)
          bigpart = gsub(".*\\((.*)\\)", '\\1', object_x)
          cat('so trying to load the overall item ', bigpart, ' that contains object ', subpart, '\n')
          data(bigpart,  envir = as.environment(paste0("package:", thispkg) ))
          object_x = subpart
          #return(0)
        }

        xattempt <- try(
          get(object_x, envir = as.environment(paste0("package:", thispkg) ) ),
          silent = TRUE
        )
        if (inherits(xattempt, "try-error")) {
          cat("Error in trying to use get(", object_x, ", envir = as.environment(", paste0('package:', thispkg), ")) \n")
          xattempt <- NULL
        }

        format(object.size(
          xattempt
        ),
        # maybe since already attached do not need to do all this to specify where it is
        units = "MB", digits = 3, standard = "SI")
      } # end function(object_x)
    ) # end of sapply loop over all zrows$Item
    ############################## #
    cat('\n\n')

    zrows <- zrows[ , c("Package", "Item", "size", "Title")]
    sizenumeric =   as.numeric(gsub("(.*) MB", "\\1", zrows$size))
    zrows$sizen <- sizenumeric

    if (sortbysize) {
      zrows <- zrows[order(sizenumeric), ]
      rownames(zrows) <- NULL
    }

    ############################## #
    # show sorted rounded largest ones only, with shortened titles
    zrows_narrow <- zrows
    zrows_narrow$Title.Short  <- substr(zrows_narrow$Title, 1, len)
    zrows_narrow$Title <- NULL
    sizenumeric =   as.numeric(gsub("(.*) MB", "\\1", zrows_narrow$size))
    if (sum(sizenumeric >= 1)  == 0) {
      cat("None are > 1 MB in size: \n\n")
      rounding = 3
    } else {
      cat('The ones at least 1 MB in size: \n\n')
      zrows_narrow <- (zrows_narrow[sizenumeric >= 1, ])
      rounding = 0
    }
    sizenumeric =   as.numeric(gsub("(.*) MB", "\\1", zrows_narrow$size))
    zrows_narrow$size <-  paste0(round(sizenumeric, rounding), ' MB')
    print(zrows_narrow)

    ############################## #

    invisible(zrows)
  }
}
