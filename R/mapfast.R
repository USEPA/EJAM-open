

#' Map - points - Create leaflet html widget map of points using EJAM results with EJ stats
#'
#' Like [mapfast()] but with column_names = "ej"
#'
#' @inheritParams mapfast
#' @inherit ejam2map examples
#' @return like what [mapfast()] returns
#' @export
#'
mapfastej <- function(mydf, radius = 3, column_names = 'ej', labels = column_names, launch_browser = FALSE, color = "#03F") {

  mapfast(mydf = mydf, radius = radius, column_names = column_names, labels = labels, launch_browser = launch_browser, color = color)
}
############################################################################ #


#' Map - points - Create leaflet html widget map of points using table with lat lon
#'
#' @param mydf Typically something like the output of ejamit()$results_bysite, but
#'   can also be the full output of [ejamit()] in which case this uses just the $results_bysite table,
#'   and in general mydf can be a data.frame or data.table that has a set of
#'   points or polygons or Census FIPS codes.
#'
#'   1) point data defined by columns named lat and lon, or columns that [latlon_infer()] can infer to be that,
#'   as from [sitepoints_from_any()] or [ejamit()]$results_bysite
#'   2) polygon data in a spatial data.frame that has a geometry column of polygons, as from [shapefile_from_any()], or
#'   3) Census units defined by FIPS codes in a column called "ejam_uniq_id"
#'   (not fips), where those fips are for States, Counties, Tracts, Blockgroups,
#'   or cities/towns/Census Designated Places (7 digits including any leading zeroes),
#'   e.g., as from \code{names2fips('DE')} or \code{ejamit(fips='01')$results_bysite}.
#'
#' @param radius in miles, converted to meters and passed to leaflet::addCircles() if appropriate
#' @param column_names If "ej" then nice popup made based on just key EJScreen
#'   indicators. If "all" then every column in the entire mydf table is shown
#'   in the popup. If a vector of colnames, only those are shown in popups.
#' @param labels The labels used before the column_names, for map popups,
#'   like  label: column_name  (ignored if column_names is ej or all)
#' @param launch_browser optional logical, set to TRUE if you want the function to
#'   launch a default browser window to show the map
#'   and print the temp filepath and filename in the console.
#'   Normally the map would be shown in the default RStudio viewer pane.
#' @param color color of circles or polygons
#' @seealso [ejam2map()] [popup_from_any()] [mapfastej()]
#' @return plots a leaflet map with popups with all the columns from mydf,
#'   and returns html widget
#' @inherit ejam2map examples
#'
#' @export
#'
mapfast <- function(mydf, radius = 3, column_names='all', labels = column_names, launch_browser = FALSE, color = "#03F") {

  x <- NULL
  fromejam <- FALSE
  # if the whole list from ejamit(), not a data.frame, was provided, use just the results_bysite table
  if (!is.data.frame(mydf) && is.list(mydf) && 'results_bysite' %in% names(mydf)) {
    message("mydf seems to be a list of tables such as output from ejamit() so using just the results_bysite table here")
    sitetype <- ejamit_sitetype_from_output(mydf) # works if mydf is either output of ejamit() or ejamit()$results_bysite
    mydf <- mydf$results_bysite
    fromejam <- TRUE
  } else {
    if ("ejam_uniq_id" %in% names(mydf) && "radius.miles" %in% names(mydf) && "pop" %in% names(mydf)) {
      fromejam <- TRUE
    }
    sitetype <- sitetype_from_dt(mydf)
  }
  # if data.table was provided
  if (data.table::is.data.table(mydf)) {mydf <- as.data.frame(mydf)} # in case it was a data.table. note this could be slow as it makes a copy, but setDF(mydf) could alter mydf by reference in the calling envt.

  # fixcolnames() ####
  # use standardized format of indicator names in case they are the long versions of ejamit variable names
  names(mydf) <- fixcolnames(names(mydf), "long", "r") # if already r, this does nothing. if long, it makes them r format so popup_from_ejscreen() will work
  ##
  # I think this is ok, since it is just for the popups to work right if mydf was output of analysis but used the long form variable names as headers
  # popup_from_ejscreen() code was written to assume rnames (as from ejscreenapi_plus) not longnames (as from ejscreenit),
  # so try to accomodate that here if user provided output of ejscreenit() or long names in general
  # popup_from_ejscreen() needs to flexibly allow long format names as input.
  # ejscreenit() and app_server already handle this issue by renaming to rnames before calling popup_from_ejscreen()

  ######################################################### #

  # popup text ####

  if (column_names[1] == 'ej') {

    ejcols <- c(names_ej, names_ej_state, names_ej_supp, names_ej_supp_state)
    if (!all(ejcols %in% names(mydf))) {
      warning('Not all summary index columns found. Using NA values for all summary indexes in map popups.')
      ejna <- data.frame(matrix(ncol = length(ejcols), nrow = NROW(mydf)))
      names(ejna) <- ejcols
      mydf <- cbind(mydf, ejna)
    }

    mypop <- popup_from_ejscreen(sf::st_drop_geometry(mydf))

  } else if (column_names[1] == 'all') {
    mypop <- popup_from_df(sf::st_drop_geometry(mydf))
  } else {
    if (!all(column_names %in% names(sf::st_drop_geometry(mydf)))) {
      warning('Not all column_names found. Using actual colnames of table.')
      mypop <- popup_from_df(sf::st_drop_geometry(mydf))
    } else {
      mypop <- popup_from_df(sf::st_drop_geometry(mydf), column_names = column_names, labels = labels)
    }
  }
  ######################################################### #

  # sitetype ####

  if (!is.null(sitetype)) {
    if (!sitetype %in% c('shp', 'latlon', 'fips')) {
      warning('sitetype cannot be interpreted as shp, latlon, or fips')
      sitetype <- 'none'
      xok <- FALSE
    } else {
      xok <- TRUE
    }
  } else {
    # keep figuring out the type
    sitetype <- sitetype_from_dt(mydf)
    if (is.na(sitetype)) {
      sitetype <- 'none'
      xok <- FALSE
    }
  }
  ######################################################### #

  # *SHP ####
  ## map polygons
  # ignore latlon and FIPS if shapefile was provided

  if (sitetype == 'shp') {

    # if sitetype is CORRECTLY reported as shp here, then expect these possible cases:

    # - it is a NON spatial df of results from an EJAM analysis of POLYGONS! -- NOT needing circles added!
    # - it is a spatial df of points NOT from EJAM -- so NOT requiring circular buffers added? ignore radius?
    # - it is a spatial df of polygons (not from EJAM) -- so NOT requiring circular buffers added.
    #   (sitetype "shp" means it is NOT a table of points from an EJAM analysis of POINTS, needing circular buffers added... )

    if (!inherits(mydf, "sf") && "lat" %in% names(mydf) && "lon" %in% names(mydf)) {

      # it is from an EJAM analysis of polygons
      #   if polygons were analyzed, (and radius might have been nonzero), we do not want to add circular buffers!

      if (!fromejam) {
        # it is not from EJAM, so assume it is a table of points needing circular buffers added

      # If mydf is not spatial/geom, but do have lat,lon,radius,
      # handle that via adding buffer and then mapping as shapefile
      ## example:
      # mydf <- ejamit(shapefile = testshapes_2, radius = 0)$results_bysite ; mapfast(mydf)

      shp <- ejam2shapefile(mydf, save = FALSE) # has all columns but they get ignored in next step
      x <- map_ejam_plus_shp(shp = shp, out = mydf, radius_buffer = radius, launch_browser = launch_browser)
      xok <- TRUE
      } else {
        # has latlon but reported as "shp"

      }


    } else {

      # it is not from EJAM, and sitetype "shp" due to class sf; no buffer need be added -- ignore the radius parameter

      if (!("POINT" %in% sf::st_geometry_type(mydf, by_geometry = FALSE))) {
        # it seems to be polygons, so map as that
        ## example:
        ##   mydf <- shapefile_from_any(testdata('portland', quiet=TRUE)[1]) ; mapfast(mydf)
        x <- map_shapes_leaflet(mydf, popup = mypop, color = color)
        xok <- TRUE

      } else {
        # it seems to be NOT polygons, but is spatial df of points, and not from EJAM output
        # so no buffer need be added -- but use the radius parameter to add circles? to make points easier to see

        # if no lat/lon columns or polygons, & only points were provided
        ## example:
        ##   mydf <- shapefile_from_any(testdata('stations.zip', quiet=TRUE)) ; mapfast(mydf)

        ## mapview could map it but not map_shapes_leaflet
        # x <- map_shapes_mapview(mydf) # must have mapview package attached for this to work

        ## or can do this:
        # assume no lat,lon columns are also there if coordinates exist in geometry column?
        if ("lat" %in% names(mydf) && "lon" %in% names(mydf)) {
          # already had lat,lon columns! make it nonspatial to let it plot as point data
          mydf <- sf::st_drop_geometry(mydf)
        } else {
          latlon <- data.frame(sf::st_coordinates(mydf)); names(latlon) <- c("lon", "lat")
          mydf <- data.frame(mydf, latlon) # makes it nonspatial but ok since have latlon now
        }
        ## then
        # return(mapfast(mydf, radius = radius))
        ## or let it plot below as if it were just latlon data provided
        sitetype <- 'latlon'
        xok <- TRUE
      }
    }
    # cat('For analysis and map of shapefile data, you can try something like this:
    #
    #       shp <- shapefile_from_any(testshapes_2)
    #       m <- map_shapes_leaflet(shp)
    #       # or
    #       out <- ejamit(shapefile = shp)
    #       ejam2map(ejamit(shapefile = shp)) # does not draw polygons, since they were not provided
    #       ejam2map(ejamit(testpoints_10, radius = 10)) # does work since it can show circles at points
    #       # or
    #        mapfast(out)
    #       # or MAYBE
    #       shp <- ejam2shapefile(out, save = FALSE, shp = shp)
    #       map_shapes_leaflet(shp, popup = popup_from_ejscreen(out))
    #
    #       ')

    # # for now, assume if most of the colnames in mydf are found in ejamit output table, treat popups like from ejamit
    # if (length(intersect(names(mydf), names(testoutput_ejamit_10pts_1miles$results_overall))) > length(names(mydf)) / 2 ) {
    #   pop <- popup_from_ejscreen(sf::st_drop_geometry(mydf))
    # } else {
    #   pop <- popup_from_any(sf::st_drop_geometry(mydf))
    # }
  }

  ######################################################### #

  # *LATLON not shp (ignore FIPS) ####

  if (sitetype == 'latlon') {

    radius.meters <- radius * meters_per_mile # data loaded by pkg
    # units are meters for addCircles, and pixels for addCircleMarkers

    ## infer lat lon cols (if they exist) ####
    # use new names for lat and lon just to check those values and to use this as data sent to leaflet, but show fixcolnames for names in popup
    # Give leaflet a temp version of mydf where column names are sure to include "lat" and "lon" here:
    #  Temporarily rename mydf to ensure colnames "lat" and "lon" even if aliases of those had been used in mydf
    mydf_names_now <- names(mydf)
    mydf_names_with_latlon <- latlon_infer(names(mydf))
    names(mydf) <- mydf_names_with_latlon

    x <- leaflet::leaflet(data = mydf) |> leaflet::addTiles() |>
      leaflet::addCircles(lng = ~lon, lat = ~lat, radius = radius.meters, color = color,
                          popupOptions = list(maxHeight = 400, maxWidth = 850),
                          popup = mypop) |>
      leaflet.extras2::addEasyprint( ) # button to print or print to pdf and save
    # now x is a Leaflet map widget using the htmlwidgets package
    xok <- TRUE

    # now change colnames(mydf) back to what you want to see, what they had been
    names(mydf) <- mydf_names_now
  }
  ######################################################### #

  # *FIPS not shp not latlon ####

  if (sitetype == 'fips') {

    if ('ejam_uniq_id' %in% colnames(mydf) && all(fips_valid(mydf$ejam_uniq_id))) {
      fips <- mydf$ejam_uniq_id
    } else {
      fips <- data.frame(mydf)[, which(fixnames_aliases(names(mydf)) %in% 'fips')[1]]
      # if (all(fips_valid(fipsvalues))) {
      #   # should already be true since checked type earlier using aliases etc.
      # }
    }
    ftype <- fipstype(fips)

    ######################### #
    # _States  ####
    # get boundaries

    if (all(ftype %in% 'state')) {
      # fips <- mydf$ejam_uniq_id
      shp <- shapes_from_fips(fips) #  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      x <- map_shapes_leaflet(shp, popup = mypop, color = color)
      xok = TRUE
    }
    ######################### #
    # _Counties  ####
    # get boundaries

    if (all(ftype %in% 'county')) {
      ## maybe could use:
      # fips <- mydf$ejam_uniq_id
      ### also see  shapes_counties_from_countyfips()
      # shp <- shapes_from_fips(fips) #  # <<<<<<<<<<<<
      # x <- map_shapes_leaflet(shp, popup = mypop, color = color)
      # xok = TRUE
      ## *** handle specified color here...
      x <- mapfastej_counties(mydf, colorvarname = color) # handles popups, ignores params above, assumes mydf$ejam_uniq_id is fips
      xok <- TRUE
    }
    ######################### #
    # _City/CDPs  ####
    # get boundaries

    if (all(ftype %in% 'city')) {
      # fips <- mydf$ejam_uniq_id
      # shp <- shapes_places_from_placefips(fips)
      shp <- shapes_from_fips(fips) #  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      x <- map_shapes_leaflet(shp, popup = mypop, color = color)
      xok = TRUE
    }
    ######################### #
    # _Tracts ####
    if (all(ftype %in% 'tract')) {
      # fips <- mydf$ejam_uniq_id
      shp <- shapes_from_fips(fips) #  SLOW if many, like > 20  #  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      x <- map_shapes_leaflet(shp, popup = mypop, color = color)
      xok <- TRUE
    }
    ######################### #
    # _Blockgroups ####

    if (all(ftype %in% 'blockgroup')) {
      # fips <- mydf$ejam_uniq_id
      # shp <- shapes_blockgroups_from_bgfips(fips)
      shp <- shapes_from_fips(fips) #  SLOW if many, like > 20  #  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      x <- map_shapes_leaflet(shp, popup = mypop, color = color)
      xok <- TRUE

      ######################### #
      ## for adding to an existing map, was using something like this addGeoJSON() approach...
      #
      # map_blockgroups = function(fips, mypop = NULL) {
      #   #example# fips <- blockgroupstats[ST %in% 'DE', bgfips][1:3]
      #   shp <- shapes_blockgroups_from_bgfips(fips) #    SLOW if many, like > 20
      #   # mapview::mapview(shp) # easiest way, but requires mapview attached
      #   bb <- as.vector(sf::st_bbox(shp))
      #   mymap <- leaflet::leaflet() %>%
      #     leaflet::addGeoJSON(geojsonio::geojson_json(shp), color = "blue", group = "Blockgroups", data = shp) %>%
      #     addTiles() %>% fitBounds(bb[1], bb[2], bb[3], bb[4])   %>%
      #      addPopups(popup = mypop) # %>%
      #   # leaflet::addLayersControl(overlayGroups = "Blockgroups")
      #   return(mymap)
      # }
      ######################### #
      # x <- map_blockgroups(fips) %>% addPopups(popup = mypop)
      # xok <- TRUE
    }
    ######################### #

  }
  ######################################################## #

  # *CANT MAP ####
  #  because not SHP, no latlon, and no usable FIPS.
  #
  # And for other kinds of places that are based on points like via naics,sic,mact,regid, or even street addresses,
  #  those would already have been converted to latlon, handled outside this function.

  if (!xok) {
    warning('no valid lat lon values to map')
    return(NA)
  }

  ######################################################## #

  # see in browser ####

  if (launch_browser && !shiny::isRunning()) {
    # map2browser() would do the same
    fname <- tempfile("mapfast_", fileext = ".html")
    htmlwidgets::saveWidget(x, file = fname)
    fname <- normalizePath(fname) # helps it work on MacOS
    # htmltools::save_html  # *** might work also?
    browseURL(fname)
    cat(fname, "\n")
  }

  return(x)
}
############################################################################ #
