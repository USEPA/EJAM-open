############################ ############################# #
##########   will   change everywhere from using what had been called 

#   ejamit_sitetype_check()

# to just using identical but renamed 

#   ejamit_sitetype_from_input()  

##  in ejamit()  in ejamit_compare_types_of_places()  in manual_nonalphabetical.R and testthat/test-ejamit_sitetype_check.R  
## and delete the file  ejamit_sitetype_check.R
## and do not have to update pkgdown yml file, since noRd and internal
############################ ############################# #


#' helper to infer what type of sites were analyzed by looking at params given as input to ejamit()
#'
#' @param sitepoints  parameter as was passed to [ejamit()]
#' @param fips  parameter as was passed to [ejamit()]
#' @param shapefile parameter as was passed to [ejamit()]
#'
#' @return either "latlon", "fips", or "shp",
#'   or errors if 2 or 3 types were specified at once
#' 
#' @keywords internal
#' @noRd
#'
ejamit_sitetype_check <- function(sitepoints, fips=NULL, shapefile=NULL) { 
  
  if (!is.null(shapefile)  ) {
    sitetype <- "shp"
  } else if (!is.null(fips)  ) {
    sitetype <- "fips"
  } else {
    sitetype <- "latlon" # if none of 3 is specified, tries to interactively select file of latlon
  }
  if (sum(!missing(sitepoints), !is.null(shapefile), !is.null(fips)) > 1) {
    stop("2 or more of the 3 parameters 'sitepoints', 'shapefile', 'fips' were provided, but must specify only 1 of the 3. ")
    ## or, if we want to warn instead of stop, could use only latlon when avail even if fips and/or shp was also erroneously specified, & use shp if only shp&fips specified.
    # if (!missing(sitepoints)) {sitetype <- "latlon"} 
    # warning("2 or more of the 3 parameters 'sitepoints', 'shapefile', 'fips' were provided, but must specify only 1 of the 3. Using sitepoints if provided. If not, ignoring fips and using shapefile.")
  }
  
  if (sitetype == "latlon" && missing(sitepoints) && interactive() && !isRunning()) {
    message("ejamit() will try to help select a latlon file")
  }
  return(sitetype)
}
############################ ############################# #
