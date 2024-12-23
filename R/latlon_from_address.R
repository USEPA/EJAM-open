

####################################################################### #
#  library(AOI) # needs tidygeocoder, fipio, and others not otherwise in EJAM:
### Imports: datasets, dplyr, fipio, htmlwidgets, jsonlite, leaflet,
### leaflet.extras, rnaturalearth, rvest, sf, shiny, terra,
### tidygeocoder, units
####################################################################### #

#                Functions here

# latlon_from_address_table()
# latlon_from_address()

# address_from_table()
# address_from_table_goodnames()

# fixcolnames_infer()

##################### #

##     test data:

# test_address_table_9
# test_addresses_9
# test_address_parts1
# test_addresses2
# test_address_table
# test_address_table_withfull
# test_address_table_goodnames

####################################################################### #


#' get lat,lon from table that contains USPS addresses
#'
#' @param x data.frame or can be missing if interactive
#'
#' @return same as output of [latlon_from_address()]
#'
#' @examples
#' address_from_table(test_address_table)
#'
#' ## fname <- system.file("testdata/address/street_address_9.xlsx", package = "EJAM")
#' ## test_addresses_9b <- address_from_table(fname)
#' \dontrun{
#' 
#' # This requires first attaching the AOI package.
#' 
#' pts <- latlon_from_address(test_addresses_9[1:2])
#' ## out <- ejamit(pts, radius = 1)
#' ## ejam2report(out)
#'
#' latlon_from_address_table(test_address_table)
#' latlon_from_address_table(test_address_table_withfull)
#' ## *** NOTE IT FAILS IF A COLUMN WITH STREET NAME ONLY IS CALLED "address"
#' ##   instead of that storing the full address.
#' }
#' fixcolnames_infer(currentnames = test_address_parts1)
#' fixcolnames_infer(currentnames = names(test_address_table))
#'
#' @export
#'
latlon_from_address_table <- function(x) {
  
  if (missing(x) && interactive()) {
    x <- rstudioapi::selectFile(caption = "Select .csv or .xlsx with addresses")
    if(is.null(x)){
      warning('No value provided for argument "x".')
      return(NULL)
    }
  } else if(missing(x)){
    
    warning('No value provided for argument "x".')
    return(NULL)
  }else if (all(is.na(x)) | is.null(x)){
    warning('No value provided for argument "x".')
    return(NULL)
  }
  if (is.atomic(x) && all(is.character(x)) && length(x) == 1) {
    if (file.exists(x)) {
      x <- read_csv_or_xl(x)
    }
  }
  
  latlon_from_address(
    address_from_table(x)
  )
}
####################################################################### #


#' get USPS addresses from a table of that info
#'
#' @param x data.frame with address info in column(s)
#'
#' @return vector of USPS addresses, 1 per row of x
#'
#' @export
#'
address_from_table <- function(x) {
  
  if (missing(x) && interactive()) {
    x <- rstudioapi::selectFile(caption = "Select .csv or .xlsx with addresses")
    if(is.null(x)){
      warning('No value provided for argument "x".')
      return(NULL)
    }
  } else if(missing(x)){
    
    warning('No value provided for argument "x".')
    return(NULL)
  }else if (all(is.na(x)) | is.null(x)){
    warning('No value provided for argument "x".')
    return(NULL)
  }
  if (all(is.character(x)) && is.atomic(x) && length(x) == 1) {
    if (file.exists(x)) {
      x <- read_csv_or_xl(x)
    }
  }
  
  names(x) <- fixcolnames_infer(names(x)) # see also fixnames_aliases() 
  addresses <- address_from_table_goodnames(x)
  return(addresses)
}
####################################################################### #


#' utility to get USPS addresses from a table that has correct colnames
#'
#' @param x a table with columns that overlap with colnames_allowed
#' @param colnames_allowed optional
#' @seealso [address_from_table()]
#' @return vector of USPS addresses
#'
#' @keywords internal
#' @export
#'
address_from_table_goodnames <- function(x, colnames_allowed = c('address', 'street', 'city', 'state', 'zip')) {
  
  # Get vector of addresses from a table.
  # Assumes "address" column has full address???
  # but that colname if not available,
  # seeks portions of address among columns defined by colnames_allowed
  # and pastes those together separated by a space.
  # i.e. columns called street, city, state, zip
  # or whichever of those are in the table as colnames.
  # Returns a vector of addresses.
  
  if (missing(x) && interactive()) {
    x <- rstudioapi::selectFile(caption = "Select .csv or .xlsx with addresses")
    if(is.null(x)){
      warning('No value provided for argument "x".')
      return(NULL)
    }
  } else if(missing(x)){
    
    warning('No value provided for argument "x".')
    return(NULL)
  }else if (all(is.na(x)) | is.null(x)){
    warning('No value provided for argument "x".')
    return(NULL)
  }
  if (is.atomic(x) && all(is.character(x)) && length(x) == 1) {
    if (file.exists(x)) {
      x <- read_csv_or_xl(x)
    }
  }
  stopifnot(is.atomic(colnames_allowed))
  colnamesfound <- intersect(names(x), colnames_allowed)
  if ("address" %in% colnamesfound && !("street" %in% colnamesfound) && !("city" %in% colnamesfound)) {
    # seems like address column must be the entire address with street, city, state all in it ?
    cols2use <- "address"
    # return(x[, cols2use])
  } else {
    if ("address" %in% colnamesfound && !("street" %in% colnamesfound) && ("city" %in% colnamesfound) ) {
      # seems like address column is probably the street in this case - even if state is missing?
    }
    cols2use <- colnames_allowed[colnames_allowed %in% colnamesfound]
    # return(apply(x[, cols2use], 1, paste, collapse = " "))
  }
  return(
    if (length(cols2use) == 1) {
      return(x[, cols2use])
    } else {
      apply(x[, cols2use], 1, paste, collapse = " ")
    }
  )
}
####################################################################### #


#' geocode, but only if AOI package is installed and attached
#'   and what it imports like tidygeocoder etc.
#' @details slow? about 100 per minute?
#' @param address vector of addresses
#' @param xy set it to TRUE if you want only x,y returned, see help for AOI pkg
#' @param pt  see help for AOI pkg, return geometry if set to TRUE, allowing map.
#'   param as provided is ignored and set to TRUE if aoimap=TRUE
#' @param aoimap  see help for AOI pkg, create map if set to TRUE
#' @param batchsize how many to request per geocode query, done in batches if necessary
#' @param ...  passed to geocode() see  `help(geocode, package = "AOI")`
#'
#' @return returns NULL if you have not installed and attached the AOI package.
#'   If AOI is attached via library() or require() or package imports,
#'   this returns a tibble table of x,y or lat,lon values or geometries.
#'   see the AOI package.
#' @examples
#'   # only works if AOI package installed already and attached too
#'   # #test_addresses2b <- c("1200 Pennsylvania Ave, NW Washington DC", "Research Triangle Park")
#'   # #x <- geocode(test_addresses2b)
#'   # out <- ejamit(x, radius = 3)
#'   # fname = system.file("testdata/address/street_address_9.xlsx", package="EJAM")
#'
#' #x1 <- read_csv_or_xl(fname)
#' #x2 <- latlon_from_anything(fname)
#' #names(x1)
#' #names(x2)
#'
#' @export
#'
latlon_from_address <- function(address, xy=FALSE, pt = FALSE, aoimap=FALSE, batchsize=25, ...) {
  
  stopifnot(is.atomic(address), is.character(address), length(address) <= 1000,
            is.atomic(xy),     is.logical(xy), 
            is.atomic(pt),     is.logical(pt), 
            is.atomic(aoimap), is.logical(aoimap),
            is.atomic(batchsize), is.numeric(batchsize), batchsize <= 100)
  if (offline()) {
    cat("NO INTERNET CONNECTION AVAILABLE - cannot use geocoding\n")
    warning("NO INTERNET CONNECTION AVAILABLE - cannot use geocoding")
    return(NULL)
  }
  ############################################## #
  # make AOI package only optional ####
  ### all these are imported by AOI pkg that were not yet needed by EJAM:
  #
  #   c("datasets", "fipio", "htmlwidgets", "jsonlite", "leaflet.extras",
  #   "rnaturalearth", "rvest", "shiny", "terra", "tidygeocoder", "units")
  #
  # if (!require("AOI")) {
  #   remotes::install_github("mikejohnson51/AOI") #
  # }
  
  x <- try(find.package("AOI"))
  if (inherits(x, "try-error")) {
    warning('AOI package not available. To install, run:
            devtools::install_github("https://github.com/mikejohnson51/AOI/", auth_token = NULL)')
    x <- NULL
    return(x)
    ############################################## #
  } else {
    geocode_function_attached <- (exists("geocode") && is.function(geocode))
    if (!geocode_function_attached) {
      warning('for this to work you would need to use library(', 'AOI', ') first\n')
      x <- NULL
      return(x)
      # how to make it attached or used without triggering renv or packrat to think we want to import or depend on it?      
    }
    
    # *** Also, clarify distinction between geocode() from the AOI package and geocode() from the tidygeocoder package -- AOI geocode() is described as a wrapper around the tidygeocoding and Wikipedia services.
    # x <- geocode(c("1200 Pennsylvania Ave, NW Washington DC", "Dupont Circle", "Research Triangle Park"))
    
    if (length(address) > batchsize) {
      message("only ", batchsize," max supported per batch in this function until decide if more ok")
      x <- latlon_from_address_batched(address = address, xy = xy, pt = aoimap, aoimap = FALSE, batchsize = batchsize, ...)
      if (aoimap) {x |> aoi_map()} # check if it works like this here
      return(x)
    }  
    
    if (aoimap) {
      x <- geocode(address, pt = TRUE, xy = xy, ...)   |> aoi_map()   # AOI:: # avoid making renv think we require it
    } else {
      x <- geocode(address, pt = pt, xy = xy, ...)
    }
    
    # geocode(xy=TRUE) does not work correctly for more than just 1 address, so fix output
    if (xy) {  # && length(address) > 1  ?? no
      x <- matrix(x, ncol = 2)
      x <- as.data.frame(x)
      colnames(x) <- c("x", "y")
    }
    x <- as.data.frame(x) # otherwise it is a tibble
    
    # convert x and y colnames to lon and lat
    names(x)[names(x) == "x"] <- "lon"
    names(x)[names(x) == "y"] <- "lat"
  }
  
  return(x)
}
####################################################################### #



latlon_from_address_batched = function(address, batchsize=25, ...) {
  
  out = list()
  # batchsize = 25
  
  # batches = length(address) %/% batchsize
  dividedby_canfithowmany = `%/%`
  dividedby_leaves = `%%`
  batches = dividedby_canfithowmany(length(address), batchsize)
  leftover = dividedby_leaves(length(address), batchsize)
  
  if (batches > 0) {
    for (i in 1:batches) {
      nstart = 1 + (i - 1) * batchsize
      out[[i]] <- latlon_from_address(address[nstart:(nstart + batchsize - 1)], ...)
      cat('Finished geocoding addresses', nstart, "-", (nstart + batchsize - 1), "out of", length(address), "\n")
    }
  } else {
    i = 0
    nstart = 1
    batchsize = 0
  }
  if (leftover > 0) {
    out[[i + 1]] <- latlon_from_address(address[(nstart + batchsize):length(address)])    
  }
  out = do.call(rbind, out)
  out
}
####################################################################### #
