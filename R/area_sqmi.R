

#' utility to get areas of places (points with radius at each, or polygons, or census units by FIPS)
#' @details
#'   Only one of the parameters can be specified at a time, and others must be NULL.
#'   If you provide a data.frame it tries to infer which info is in the table --
#'   radius.miles, shp, or fips.
#'
#'   Note this is slow for fips since it has to download boundaries. If you already have
#'   the shapefile of boundaries, provide that as the shp parameter instead of using fips.
#'
#'   Note: if you provide a single number for radius (a vector of length 1),
#'   this returns a single value for area. If you provide a vector of radius values,
#'   even if they are all the same number, this returns a vector as long as the input
#'   radius.miles or as long as NROW(df).
#' @param df optional data.frame, one place per row -
#'   This function tries to infer sitetype by seeing if df is 1) a
#'   spatial data.frame of class "sf", or 2) has a column that can be interpreted as
#'   an alias for fips, or as last resort, 3) a column that is an alias for radius.miles
#' @param radius.miles optional vector of distances from points defining circular buffers
#' @param shp optional spatial data.frame sf class object like from [shapefile_from_any()]
#' @param fips optional vector of character Census FIPS codes, with leading zeroes,
#'  2 digits for State, 5 for county, etc. If you already have the boundaries
#'  then provide that as shp instead of this parameter (much faster that way).
#'
#' @returns vector of numbers same length as length(radius.miles) or length(fips) or NROW(shp)
#'
#' @export
#' @keywords internal
#'
area_sqmi <- function(df = NULL, radius.miles = NULL, shp = NULL, fips = NULL) {

  # can use to add area to output of doaggregate   and thus ejamit etc.

  if (sum(is.null(df), is.null(radius.miles), is.null(shp), is.null(fips)) != 3) {
    stop("must provide 1 and only 1 of the parameters df, radius.miles, shp, fips")
  }

  if (!is.null(radius.miles)) {
    return(area_sqmi_from_pts(radius.miles = radius.miles))
  }
  if (!is.null(shp)) {
    return(area_sqmi_from_shp(shp))
  }
  if (!is.null(fips)) {
    return(area_sqmi_from_fips(fips))
  }
  if (!is.null(df)) {
    if (is.data.frame(df)) {
      return(area_sqmi_from_table(df))
    } else {
      if (is.vector(df)) {
        warning("df seems to be a vector, so treating it like a vector of radius.miles")
        return(area_sqmi_from_pts(radius.miles = df))
      } else {
        stop("cannot interpret df as radius information")
      }
    }
  }
  return(NULL) # should never occur
}
############################################################################### #

area_sqmi_from_table <- function(df) {

  if ("sf" %in% class(df)) {
    return(area_sqmi_from_shp(df))
  }
  suppressWarnings({
    fips <- fips_from_table(df)
  })
  if (!is.null(fips)) {
    message("ignoring any buffer/radius information for FIPS units")
    return(area_sqmi_from_fips(fips))
  }
  # find aliases of radius, radius.miles
  names(df) <- fixcolnames_infer(
    names(df), ignore.case = TRUE,
    alias_list = list(radius.miles = c('radius', 'radius_miles')))
  if ("radius.miles" %in% names(df)) {
    radius.miles <- df$radius.miles
    return(area_sqmi_from_pts(radius.miles = radius.miles))
  }
  warning("cannot determine types of locations to calculate areas")
  return(rep(NA, NROW(df)))
}
############################################################################### #
area_sqmi_from_pts <- function(radius.miles) {
  stopifnot(is.vector(radius.miles))
  pi * radius.miles^2
}
############################################################################### #
area_sqmi_from_shp <- function(shp, units_needed = "miles^2") {
  area <- sf::st_area(shp) # answer is in meters^2 or m^2
  #/ meters_per_mile^2
  units(area) <- units_needed
  area <- as.numeric(area) # allows math, comparisons like > 2, etc. but loses metadata about units being square miles or whatever
  return(area)
}
############################################################################### #
area_sqmi_from_fips <- function(fips) {
  shp <- shapes_from_fips(fips)
  return(area_sqmi_from_shp(shp))
}
############################################################################### #
