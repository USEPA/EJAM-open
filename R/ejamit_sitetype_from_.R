############################ ############################# #


#' helper to infer what type of sites were analyzed by looking at params given as INPUT to ejamit()
#' used by ejamit() and ejamit_compare_types_of_places()
#' @details Note `sitetype` is not quite the same as the `site_method` parameter used in building reports.
#'   `site_method` can be one of these: SHP, latlon, FIPS, NAICS, FRS, EPA_PROGRAM, SIC, MACT
#'   `sitetype` can be latlon, fips, or shp as returned by [ejamit()], but can include
#'      lowercase versions of site_method values too within server code and some function parameters!
#' @param sitepoints  parameter as was passed to [ejamit()]
#' @param fips  parameter as was passed to [ejamit()]
#' @param shapefile parameter as was passed to [ejamit()]
#'
#' @return either "latlon", "fips", or "shp",
#'   or errors if 2 or 3 types were specified at once
#'
#' @keywords internal
#'
ejamit_sitetype_from_input <- function(sitepoints = NULL, fips = NULL, shapefile = NULL) {

  if (!is.null(shapefile)) {
    sitetype <- "shp"
  } else if (!is.null(fips)) {
    sitetype <- "fips"
  } else if (!is.null(sitepoints)) {
    sitetype <- "latlon"
  } else {
    sitetype <- NULL # if none of 3 is specified, ejamit() tries to interactively select file (now not necessarily of latlon)
    #   # warning("None of 3 specified") # ok since it tries to allow selection interactively
  }
  if (sum(!is.null(sitepoints), !is.null(shapefile), !is.null(fips)) > 1) {
    stop("2 or more of the 3 parameters 'sitepoints', 'shapefile', 'fips' were provided, but must specify only 1 of the 3. ")
    ## or, if we want to warn instead of stop, could use only latlon when avail even if fips and/or shp was also erroneously specified, & use shp if only shp&fips specified.
    # if (!missing(sitepoints)) {sitetype <- "latlon"}
    # warning("2 or more of the 3 parameters 'sitepoints', 'shapefile', 'fips' were provided, but must specify only 1 of the 3. Using sitepoints if provided. If not, ignoring fips and using shapefile.")
  }

  # if (sitetype == "latlon" && missing(sitepoints) && interactive() && !shiny::isRunning()) {
  #   message("ejamit() will try to help select a latlon file")
  # }
  return(sitetype)
}
############################ ############################# #
############################ ############################# #


#' helper to infer what type of sites were analyzed by looking at OUTPUT of ejamit()
#'
#' @param out from ejamit()
#'
#' @return "latlon", "fips", or "shp"
#'
#' @keywords internal
#' @noRd
#'
ejamit_sitetype_from_output = function(out) {

  if ("sitetype" %in% names(out)) {
    return(out$sitetype)
    # implementing that via a change in ejamit()
    # but server would need to do it separately when ejamit not used there (fips may use ejamit in server but shp or latlon may not)
    # - cannot do in doaggregate() alone and cannot easily save in sites2blocks output of getblocks...
  }
  return(
    sitetype_from_dt(out$results_bysite)
  )
}
############################ ############################# #

# helper to infer what type of sites were analyzed by looking at 1 table like OUTPUT of ejamit()$results_bysite

sitetype_from_dt <- function(dt) {

  # used by ejamit_sitetype_from_output()

  ## dt could be e.g.,  ejamit()$results_bysite

  #   shp

  # if it is class "sf" like a spatial data.frame from [sf::st_as_sf()] or [shapefile_from_any()],
  # call it sitetype 'shp' even if there might be lat/lon and/or fips info there
  if ("sf" %in% class(dt)) {
    return('shp')
  }

  #   latlon

  # names(dt) <- fixcolnames(names(dt), 'long', 'r') # in case they were long column headers? like from one of the ejscreenapi functions?
  names(dt) <- fixcolnames_infer(names(dt))    ## or just  # names(dt) <- latlon_infer(names(dt))
  # if it has lat and lon columns, call it sitetype 'latlon' unless all coordinates are NA
  # *** This is no longer a way to be sure it was latlon type !
  #  ejamit(shapefile = shp)$results_bysite actually returns lat,lon of centroid of each polygon now!
  if ('lat' %in% names(dt) && 'lon' %in% names(dt)) {
    if (!all(is.na(dt$lat))) {
      return("latlon")
    }
  }

  #   fips

  # If not shp and not latlon, then
  # call it 'fips' if it has 'ejam_uniq_id' column that can be interpreted as FIPS,
  # as from output of ejamit(fips="040130610171")$results_bysite
  ## 1st, fips is assumed to be stored as mydf$ejam_uniq_id but
  ## can be provided here as a separate column or parameter
  if ('ejam_uniq_id' %in% names(dt)) {
    if (all(fips_valid(dt$ejam_uniq_id))) {
      return("fips")
    }
  }
  if ('fips' %in% fixnames_aliases(colnames(dt))) {
    fipsvalues = data.frame(dt)[, which(fixnames_aliases(names(dt)) %in% 'fips')[1]]
    if (all(fips_valid(fipsvalues))) {
      return("fips")
    }
  }

  warning("cannot determine valid sitetype")
  return(NA)
}
############################ ############################# #
