
# SEVERAL FUNCTIONS ARE IN THIS FILE ####
#
# See source document outline for list of functions defined here ####

#  seealso mapfast() mapfastej() ejam2map()

########################### # ########################### # ########################### # ########################### #


############################ #

#' helper - map the merge of ejamit(shapefile = x) output and the shapefile x
#' @details used by server, [ejam2map()], and [mapfast()]
#' @param shp spatial data.frame
#' @param out output of ejamit()
#' @param radius_buffer optional but can be obtained from out
#' @param circle_color optional
#' @param launch_browser set TRUE to have it launch browser to show map.
#' @examples
#' \dontrun{
#' shp1 <- shapefile_from_any(file.path(
#'   testdatafolder(), "shapes/portland_shp.zip"))
#' ejam2map(ejamit(shapefile = shp1, radius = 0), shp = shp1)
#' shp2 <- testshapes_2
#' ejam2map(ejamit(shapefile = shp2, radius = 0), shp = shp2)
#' ejam2map(ejamit(shapefile = shp2, radius = 2), shp = shp2)
#'
#' }
#'
#' @returns map html widget
#'
#' @keywords internal
#'
map_ejam_plus_shp <- function(shp, out, radius_buffer = NULL, circle_color = '#000080', launch_browser = FALSE) {

  ## to use it in shiny app:
  # shp <- data_uploaded()  # reactive in shiny app already has ejam_uniq_id but outside shiny shp might lack that
  # out <- data_processed() # results of ejamit-like analysis, with ejam_uniq_id assigned before invalid rows were dropped
  # radius_buffer <- sanitized_bt_rad_buff()
  # mymap <- map_ejam_plus_shp(out = out, shp = shp, radius_buffer = radius_buffer)
  ### see also ejam2map()
  ### add popups parameter maybe

  sitetype = ejamit_sitetype_from_output(out)

  if (!("results_bysite" %in% names(out))) {
    # maybe were given the table not the whole list
    if ("ejam_uniq_id" %in% names(out)) {
      # yes
      out <- list(results_bysite = out) # make it look like the full list from ejamit()
    } else {
      stop("out needs to be a list with results_bysite in it, as from ejamit()")
    }
  }

  if ("ejam_uniq_id" %in% names(shp)) {
    # Assume we are in server, or at least assume that invalids were dropped after id assigned
    # If this function is used in the middle of server, is dealing with shp that already had ejam_uniq_id assigned after which invalids dropped.
  } else {
    # If this function is used outside shiny, shp probably does not have ejam_uniq_id assigned and can have invalid rows
    # Should clean shp like ejamit() would do...
    # i.e. give it NEW ejam_uniq_id 1:NROW and then doesn't matter if we drop invalid or empty here since
    # that happens next based on the valid flag out$results_bysite$valid, merged on ejam_uniq_id
    # This just adds id, may change crs, and
    #  drops polygons if not valid, but may leave empty. see shapefile_clean()
    shp <- shapefile_from_any(shp, cleanit = TRUE) # this might not be exactly what happens in server or ejamit?
  }

  shp <- shp[, c("ejam_uniq_id", "geometry")] # the "valid" column is in out$results_bysite, not in shp, and shp should have even invalid rows.
  # in FIPS case, ejam_uniq_id is fips not 1:N, so merge() here would fail.
  # but if we assume same NROW and same sort for shp and out#results_bysite, then we can just use the row number to match them up, or use cbind(shp, out$results_bysite)
  if (sitetype == "fips") {
    shpout <- cbind(shp, out$results_bysite)
  } else {
    shpout <- merge(shp, out$results_bysite,
                    by = "ejam_uniq_id",
                    all.x = FALSE, all.y = TRUE)
  }
  # as long as inputs are correct, matching on id should work and then we can drop invalid polygons to avoid mapping them.
  # and all.x or all.y should not matter
  # but we are about to drop all invalid ones to avoid mapping them.
  message("There were ", sum(!shpout$valid, na.rm = TRUE), " invalid polygons." )
  if ("valid" %in% names(shpout)) {
    shpout <- shpout[shpout$valid == TRUE, ] # Drop invalid polygons, dont try to map
  }
  popup_labels <- fixcolnames(namesnow = setdiff(names(shpout), c('geometry', 'valid', 'invalid_msg')), oldtype = 'r', newtype = 'shortlabel')
  pops <- popup_from_ejscreen(
    shpout %>% sf::st_drop_geometry()
  )
  ## previously had been something like this:
  # pops <- popup_from_df(
  #   shpout %>% sf::st_drop_geometry(),
  #   labels = popup_labels
  # )

  if (is.null(radius_buffer)) {
    radius_buffer <- out$results_bysite$radius.miles[1]
  }
  if (!is.na(radius_buffer) && radius_buffer > 0) {
    shpout <- sf::st_buffer(shpout, # was "ESRI:102005" but want 4269
                            dist = units::set_units(radius_buffer, "mi"))
  } else {
    shpout <- shpout %>%
      sf::st_zm() %>% sf::as_Spatial()
  }

  mymap <- leaflet::leaflet(shpout, width = if (isTRUE(getOption("shiny.testmode"))) 1000 else NULL) %>%
    leaflet::addTiles()  %>%
    leaflet::addPolygons(color = circle_color,
                         popup = pops,
                         popupOptions = leaflet::popupOptions(maxHeight = 200))


  # see in browser ####

  if (launch_browser && !shiny::isRunning()) {
    # map2browser() would do the same
    fname <- tempfile("map_", fileext = ".html")
    htmlwidgets::saveWidget(mymap, file = fname)
    fname <- normalizePath(fname) # helps it work on MacOS
    # htmltools::save_html  # *** might work also?
    browseURL(fname)
    cat(fname, "\n")
  }

  return(mymap)
}
########################### # ########################### # ########################### # ########################### #


#' Map - points - Update leaflet map of points, in shiny app
#'
#' @description update a leaflet map within the EJAM shiny app with uploaded points such as facilities
#' @param mymap, leafletProxy map object to be added to
#' @param rad, a size for drawing each circle (buffer search radius)
#' @param highlight, a logical for whether to highlight overlapping points (defaults to FALSE)
#' @param clustered, a vector of T/F values for each point, indicating if they overlap with another
#' @param popup_vec, a vector of popup values to display when points are clicked. Length should match number of rows in the dataset.
#' @param use_marker_clusters, boolean for whether to group points into markerClusters. Uses logic from shiny app to only implement when n > 1000.
#' @return a leaflet map with circles, circleMarkers, and basic popup
#'
#' @keywords internal
#'
map_facilities_proxy <- function(mymap, rad = 3, highlight = FALSE, clustered = FALSE,
                                 popup_vec = NULL, use_marker_clusters = FALSE) {

  ## map settings
  base_color      <- '#000080'
  cluster_color   <- 'red'
  circleweight <- 4

  ## if checkbox to highlight clusters is checked
  if (highlight == TRUE) {
    ## compare latlons using is_clustered() reactive
    circle_color <- ifelse(clustered == TRUE, cluster_color, base_color)
  } else {
    circle_color <- base_color
  }

  if (use_marker_clusters == FALSE) {
    ## add to leafletProxy call from Shiny app
    mymap <- mymap %>%
      leaflet::clearShapes() %>%
      leaflet::addCircles(
        radius = rad * meters_per_mile,
        color = circle_color, fillColor = circle_color,
        fill = TRUE, weight = circleweight,
        group = 'circles',
        popup = popup_vec
      ) %>%
      leaflet.extras::addFullscreenControl()
  } else {
    ## add to leafletProxy call from Shiny app
    mymap <- mymap %>%
      leaflet::clearShapes() %>%
      leaflet::clearMarkerClusters() %>%
      leaflet::addCircles(
        radius = rad * meters_per_mile,
        color = circle_color, fillColor = circle_color,
        fill = TRUE, weight = circleweight,
        group = 'circles',
        popup = popup_vec
      ) %>%
      leaflet::addCircleMarkers(
        radius = 0,
        color = circle_color, fillColor = circle_color,
        fill = TRUE, weight = circleweight,
        clusterOptions = leaflet::markerClusterOptions(),
        popup = popup_vec
      ) %>%
      leaflet::groupOptions(group = 'markers', zoomLevels = 1:6) %>%
      leaflet::groupOptions(group = 'circles', zoomLevels = 7:20) %>%
      leaflet.extras::addFullscreenControl()
  }
  ## return map
  mymap
}
########################### # ########################### # ########################### # ########################### #


#' Basic map of county outlines within specified state(s)
#' Not used by shiny app
#' @param ST a vector of one or more state abbreviations, like
#'
#'   ST = "ME"  or  ST = c("de", "RI"), or
#'
#'   \code{ST = fips2state_abbrev(fips_state_from_statename(c("Rhode Island", "district of columbia")))}
#'
#'   or e.g., all counties in EPA Region 1:
#'
#'   \code{ST = stateinfo$ST[stateinfo$REGION == 1]}
#'
#' @param colorcolumn name of column to use in setting colors of counties on map,
#'   but must be one returned by [shapes_counties_from_countyfips()] like "STATE_NAME"
#' @param type must be "leaflet" or can be "mapview" if installed and loaded
#' @examples
#' \donttest{
#' map_counties_in_state(ST = c('id', 'mt'))
#' map_counties_in_state(ST = c('id', 'mt'),
#'   colorcolumn = "STATE_NAME")
#'
#' map_counties_in_state(ST = c('id', 'mt'), type = "mapview")
#' map_counties_in_state(ST = c('id', 'mt'), type = "mapview",
#'   colorcolumn = "STATE_NAME")
#'
#'  map_counties_in_state(
#'   ST = c( 'md', 'pa'),
#'    type = "mapview", colorcolumn = "POP_SQMI")
#' }
#' @return a map
#'
#' @export
#'
map_counties_in_state <- function(ST = "DE", colorcolumn = c('pop', "NAME", "POP_SQMI", "STATE_NAME")[1],
                                  type = c("leaflet", "mapview")[1]) {

  cshapes <- shapes_counties_from_countyfips(fips_counties_from_state_abbrev(ST))

  # area_sqmi_from_shp <- function(shp) {sf::st_area(shp) / meters_per_mile^2}
  cshapes$area_sqmi <- round(area_sqmi(shp = cshapes), 0)

  countypops <- blockgroupstats[substr(bgfips, 1, 5) %in% cshapes$FIPS,
                                .(countypop = sum(pop, na.rm = T)),
                                by = .(countyfips = substr(bgfips, 1, 5))]
  cshapes$pop <- countypops$countypop[match(cshapes$FIPS, countypops$countyfips)]
  cshapes$CountyPopulation <- prettyNum(cshapes$pop, big.mark = ",")

  if (is.null(colorcolumn) || !(colorcolumn %in% names(cshapes))) {
    if ("pop" %in% names(cshapes)) {
      colorcolumn <- "pop"
    } else {
      colorcolumn <- names(cshapes)[1]
    }}
  colorscore = as.vector(unlist(sf::st_drop_geometry(cshapes[, colorcolumn])))

  if (type == "leaflet") {

    if (length(unique(colorscore)) > 10 & is.numeric(colorscore)) {
      # continuous ramp of map colors
      vpal <- leaflet::colorNumeric("viridis", domain = NULL)
      x = map_shapes_leaflet(cshapes,
                             color = ~vpal(colorscore))
    } else {
      # bins of map color
      vpal <- leaflet::colorFactor('viridis', domain = NULL)
      x = map_shapes_leaflet(cshapes,
                             color = ~vpal(factor(colorscore))  ) }
  }
  if (type == "mapview") {
    if (length(unique(colorscore))  > 10 && !is.numeric(colorscore)) {
      x = map_shapes_mapview(cshapes,
                             zcol = colorcolumn, legend = FALSE,
                             col.regions = mapview::mapviewGetOption("raster.palette"))
      # col.regions = colorscore

    } else {
      x = map_shapes_mapview(cshapes,
                             zcol = colorcolumn, legend = TRUE
                             , col.regions = mapview::mapviewGetOption("raster.palette"))
      #col.regions = colorscore)
    }
  }
  # print(x)
  return(x)
}
########################### # ########################### # ########################### # ########################### #


#' Map - County polygons / boundaries - Create leaflet or static map of results of analysis
#'
#' @param mydf something like  ejamit(fips = fips_counties_from_statename("Kentucky"), radius = 0)$results_bysite
#' @param colorvarname colname of indicator in mydf that drives color-coding
#'   (or alternatively, colorvarname = "green" means a single specific color for all, like "green")
#' @param static_not_leaflet set TRUE to use [map_shapes_plot()] instead of [map_shapes_leaflet()]
#' @param main title for map
#' @param colorfills = c('yellow', 'orange', 'red')
#' @param colorlabels = c(80, 90, 95)
#' @param colorbins =  c(80, 90, 95)
#' @param colorpalette = c("yellow", "yellow", "orange", "red")
#' @param ... passed to map_shapes_plot() if relevant
#'
#' @details THIS ASSUMES THAT mydf$ejam_unique_id is the county FIPS codes
#'
#' @seealso [mapfastej()] [map_shapes_leaflet()]
#' @return leaflet html widget (but if static_not_leaflet=T,
#'   returns just shapes_counties_from_countyfips(mydf$ejam_uniq_id))
#' @examples \donttest{
#'  fips_ky <- fips_counties_from_statename("Kentucky")
#'  x <- ejamit(fips = fips_ky, radius = 0)
#'  mapfastej_counties(x$results_bysite)
#'  }
#'  # map_shapes_leaflet(shapes = shapes_counties_from_countyfips(fips_ky))
#'
#' @export
#'
mapfastej_counties <- function(mydf, colorvarname = "pctile.Demog.Index.Supp",
                               colorfills = c('yellow', 'orange', 'red'),
                               colorlabels = c(80, 90, 95),
                               colorbins =  c(80, 90, 95),
                               colorpalette = c("yellow","yellow", "orange", "red"),
                               static_not_leaflet = FALSE, main = "Selected Counties",
                               ...) {

  # *** CANNOT HANDLE colorvarname = ANYTHING ELSE BESIDES THOSE SCALED 0 TO 100, SO FAR
  if (!(colorvarname %in% names(mydf))) {
    if ( (colorvarname[1] %in% colors()) | substr(colorvarname[1], 1, 1) == "#") {
      # try to interpret colorvarname as a single R color name like "red" or as hex code of color
      if (length(colorvarname) != 1) {
        warning('using only first colorvarname')
        colorvarname = colorvarname[1]
      }
      colorfills <- colorvarname
      colorlabels <- "CountyMap"
      colorbins <- 100
      colorpalette <- rep(colorvarname, 2)
      mydf$countymap <- 100 # colorvarname
      colorvarname <- 'countymap'
    } else {
      warning('Selected value for "colorvarname" not found. Please try a different indicator.')
      return(NULL)
    }
  } else {
    if (missing(colorfills) & missing(colorlabels) & missing(colorbins) & missing(colorpalette)) {
      if (!grepl('pctile', colorvarname)) {
        # it is not one of the standard percentile variables so it might not vary 0:100 as defaults assume it does
        setDF(mydf)
        scores <- mydf[ , colorvarname]
        colorfills = colorfills
        probs = c(0.80, 0.90, 0.95)
        colorbins = quantile(scores, probs = probs) # quantiles of provided values not of all in some universe overall
        colorlabels = paste0(round(colorbins,2), " (", 100*probs, "%ile of these)")
        colorpalette = colorpalette
      }
    }
  }

  mymapdata <- shapes_counties_from_countyfips(mydf$ejam_uniq_id)

  setDT(mydf)
  ## see color-coding of one percentile variable:
  pal <- leaflet::colorBin(
    palette = colorpalette, # c("yellow","yellow", "orange", "red"),
    domain = NULL,
    bins = colorbins # 80:100
  )
  shading <- pal(as.vector(unlist(mydf[ , ..colorvarname])))

  if (static_not_leaflet) {

    map_shapes_plot(mymapdata, main = main, ...) # or just # plot(mymapdata)
    plot(mymapdata, col = shading, add = TRUE)
    # to color code and flag the max value county:
    # flagged <- which.max(df[ , ..colorvarname])
    # plot(mymapdata[flagged, ], col = "purple", add = TRUE)
    mymap <- mymapdata # if ggplot, youd return the plot object but with plot() you cannot I think do that
    legend("topright",
           fill = colorfills, # c("yellow", "orange", "red"),
           legend = colorlabels, # c(80, 90, 100),
           title = fixcolnames(colorvarname, 'rname', 'shortlabel'))

  } else {

    myindicators <- c(colorvarname, names_d_ratio_to_state_avg, names_d_subgroups_ratio_to_state_avg)
    myindicators <- c(names(mydf)[1:9], myindicators)
    popindicators <- mydf[ , ..myindicators]
    popindicators <- table_round(popindicators) # decimal places set
    countynames <- fips2countyname(mydf$ejam_uniq_id)
    popindicators <- cbind(County = countynames, popindicators)
    poplabels <- fixcolnames(names(popindicators), 'r', 'shortlabel') # friendly labels for indicators
    popup2 <- popup_from_any(popindicators, labels = poplabels)

    mymap <- map_shapes_leaflet(mymapdata, popup = popup2, color = shading)
    mymap <- mymap %>% leaflet::addLegend(
      colors = colorfills, # c("yellow", "orange", "red"),
      labels = colorlabels, # c(80, 90, 100),
      title = fixcolnames(colorvarname, 'rname', 'shortlabel'))
  }
  return(mymap)
}
########################### # ########################### # ########################### # ########################### #


#' Map - Blockgroup polygons / boundaries near 1 site - Create leaflet map
#'
#' Overlay blockgroups near 1 site, after plotblocksnearby(returnmap = TRUE)
#'
#' @param y  output of [plotblocksnearby()] but with returnmap = TRUE
#'
#' @return leaflet map widget
#' @seealso [plotblocksnearby()]  [map_shapes_mapview()]  [map_shapes_leaflet()]  [map_shapes_plot()]
#' @examples
#'  y <- plotblocksnearby(testpoints_10[5,],
#'         radius = 3,
#'         returnmap = TRUE)
#'  map_blockgroups_over_blocks(y)
#'
#' @export
#'
map_blockgroups_over_blocks <- function(y) {

  # y is output of plotblocksnearby(returnmap = TRUE)
  if ("leaflet" %in% class(y)) {
    # This is to extract bgids from the output of the leaflet htmlwidget map object y,
    #   as from  y = plotblocksnearby(testpoints_10[1,], returnmap = TRUE)
    bgids <-  unique(as.vector(sapply( y$x$calls[[2]]$args[[7]], function(z)   gsub(   ".*bgid: ([0-9]*)<.*", "\\1", z))))
  } else {
    # can we still work with y if it was created with returnmap = FALSE ?
    # bgids <- unique(y$bgid)
    stop('y must be output of something like plotblocksnearby(testpoints_10[1,], returnmap = TRUE)')
  }

  if (!exists("bgid2fips_arrow")) {
    dataload_dynamic("bgid2fips", return_data_table = FALSE)
  }


  bgids_arrow <- arrow::Array$create(bgids)


  bgfips <- bgid2fips_arrow %>%
    mutate(bgid = cast(.data$bgid, arrow::string())) %>%
    filter(.data$bgid %in% bgids) %>%
    select(bgfips) %>%
    collect() %>%
    pull(bgfips)

  x <- shapes_blockgroups_from_bgfips(bgfips) # but not for 60+ fips!  SLOW
  # add those FIPS shapes to the leaflet htmlwidget map
  mymap <-   y %>%
    leaflet::addGeoJSON(geojsonio::geojson_json(x), color = "green", group = "Blockgroups", data = x) %>%
    leaflet::addLayersControl(overlayGroups = "Blockgroups")
  cat("Turn off the blockgroup boundaries layer using the map layer control button, to enable popup info for each block point.\n")
  return(mymap)
}
########################### # ########################### # ########################### # ########################### #


#' Map - polygons - Use base R plot() to map polygons
#'
#' @param shapes like from shapes_counties_from_countyfips(fips_counties_from_state_abbrev("DE"))
#' @param main title for map
#' @param ... passed to plot()
#'
#' @return Just draws map using plot()
#'
#'
map_shapes_plot <- function(shapes, main = "Selected Census Units", ...) {

  plot(sf::st_combine(shapes), main = main, ...)
}
########################### # ########################### # ########################### # ########################### #


#' Map - polygons - Create leaflet map from shapefile, in shiny app
#'
#' @param shapes like from shapes_counties_from_countyfips(fips_counties_from_state_abbrev("DE")),
#'   or at least a data.frame that can be interpreted as indicating points via [shapefile_from_sitepoints()]
#' @param color passed to leaflet::addPolygons()
#' @param popup  passed to leaflet::addPolygons()
#'
#' @return html widget from leaflet::leaflet()
#' @examples
#' out = testoutput_ejamit_10pts_1miles
#' out$results_bysite = out$results_bysite[1:2,]
#' map_shapes_leaflet(
#'   ejam2shapefile(out, save=F),
#'   popup = popup_from_ejscreen(out$results_bysite)
#' )
#'
#' @export
#'
map_shapes_leaflet <- function(shapes, color = "green", popup = NULL) {

  # check if spatial class
  if (!inherits(shapes, "sf")) {
    # maybe it is just points? no radius available
    shapes <- try(shapefile_from_sitepoints(shapes), silent = TRUE)
    if (inherits(shapes, "try-error")) {
      warning("shapes must be a simple features object (sf) or data.frame of points")
      return(NULL)
    } else {
      # points but without area, radius
      shapes$area_sqmi <- 0
    }
  } else {
    area_sqmi_from_shp <- function(shp) {sf::st_area(shp) / meters_per_mile^2}
    shapes$area_sqmi <- round(area_sqmi_from_shp(shapes), 0)
  }

  if ("FIPS" %in% names(shapes) & !("pop" %in% names(shapes))) {
    # if it already has "pop" then dont bother with this sometimes slow way of getting pop counts:
    shapes$Population_ACS <- fips2pop(shapes$FIPS)
  }

  if (is.null(popup)) {
    # if all but 3 colnames are in both, looks like results of ejamit(), so use that type of popup formatting
    if (length(setdiff2(names(shapes), names(testoutput_ejamit_10pts_1miles$results_overall))) < 3) {
      popup = popup_from_ejscreen(sf::st_drop_geometry(shapes))
    } else {
      popup <- popup_from_any(sf::st_drop_geometry(shapes))
    }
  }

  mymap <- leaflet::leaflet(shapes) %>%
    leaflet::addPolygons(color = color, popup = popup, popupOptions = leaflet::popupOptions(maxHeight = 200)) %>%
    leaflet::addTiles()
  return(mymap)
}
########################### # ########################### # ########################### # ########################### #

#' Map - polygons - Update leaflet map by adding shapefile data, in shiny app
#'
#' @param mymap map like from leafletProxy()
#' @param shapes like from shapes_counties_from_countyfips(fips_counties_from_state_abbrev("DE"))
#' @param color passed to leaflet::addPolygons()
#' @param popup passed to leaflet::addPolygons()
#'
#' @return html widget like from leaflet::leafletProxy()
#'
#' @export
#'
map_shapes_leaflet_proxy <- function(mymap, shapes, color = "green", popup = shapes$NAME)  {
  # *** need to confirm this default for popup is right -
  # compare to the one now in map_shapes_leaflet()
  # in RStudio console, can do  map_shapes_leaflet(shapes)
  mymap <- mymap %>%
    leaflet::addPolygons(data = shapes, color = color,  popup = popup, popupOptions = leaflet::popupOptions(maxHeight = 200)) %>%
    leaflet::addTiles()
  return(mymap)
}
########################### # ########################### # ########################### # ########################### #


#' Map - polygons - Use mapview package if available
#'
#' @param shapes like from shapes_counties_from_countyfips(fips_counties_from_state_abbrev("DE"))
#' @param col.regions passed to [mapview::mapview()]
#' @param map.types  passed to  [mapview::mapview()]
#' @param ... passed to mapview
#' @return like output of mapview function [mapview::mapview()],
#'   if mapview package is installed,
#'   when used with an input that is a spatial object as via [sf::read_sf()]
#' @examples
#'  \donttest{
#'   map_shapes_mapview(
#'     shapes_counties_from_countyfips(fips_counties_from_state_abbrev("DE"))
#'   )
#' }
#'
#' out = ejamit(testpoints_10[1,], radius = 20)
#' map_shapes_mapview(
#'   ejam2shapefile(out, save=F),
#'   popup = popup_from_ejscreen(out$results_bysite)
#' )
#'
#' @export
#'
map_shapes_mapview <- function(shapes, col.regions = "green", map.types = "OpenStreetMap", ...) {

  if (!"package:mapview" %in% search()) {
    message("this function is a nice way to map counties etc. but requires the mapview package, which EJAM does not load")
    warning("mapview package would be needed and is not attached - checking if installed")
    junk <- try(find.package("mapview"), silent = TRUE)
    if (inherits(junk, "try-error")) {
      warning("mapview package does not appear to be installed")
      return(NULL)
    } else {
      warning("mapview package appears to be installed but not attached. Try using library(mapview) or require(mapview)")
      return(NULL)
    }
  } else {
    mapview::mapview(shapes, col.regions = col.regions, map.types = map.types, ...)
  }
}
########################### # ########################### # ########################### # ########################### #


#' Map - points - ggplot2 map of points in the USA - very basic map
#'
#' @param mydf data.frame with columns named lat and lon
#' @param dotsize optional, size of dot representing a point
#' @param ptcolor optional, color of dot
#' @param xlab optional, text for x label
#' @param ylab optional, text for y label
#' @param ... optional, passed to [ggplot2::labs()]
#'
#' @return a ggplot() object
#'
#' @examples \donttest{
#'   mapfast_gg(EJAM::testpoints_10)
#'
#'   pts <- read.table(textConnection(
#'   "lat lon
#'   39.5624775 -119.7410994
#'   42.38748056 -94.61803333"
#'   ),
#'   header = TRUE,
#'   as.is = TRUE
#'   )
#'   mapfast_gg(pts)
#'   # str(pts) # lon, not long
#'   }
#'
#' @export
#'
mapfast_gg <- function(mydf=data.frame(lat = 40, lon = -100)[0,],
                       dotsize = 1, ptcolor = "black",
                       xlab = "Longitude", ylab = "Latitude", ...) {

  plotout <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = ggplot2::map_data("usa"),
                          # Note the ggplot2 "usa" dataset
                          # longitude is called "long" but mydf calls it "lon"
                          ggplot2::aes(x = long, y = lat, group = group), fill = "gray", alpha = 0.75) +
    ggplot2::geom_point(  data = mydf,
                          ggplot2::aes(x = lon, y = lat), color = ptcolor, size = dotsize) +
    ggplot2::labs(x = xlab, y = ylab, ...)
  return(plotout)
}
############################ #


# color coded map by FIPS code
#
# state resolution map example is from  https://leafletjs.com/examples/choropleth/
#
# var map = L.map('map').setView([37.8, -96], 4);
#
# var tiles = L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
#   maxZoom: 19,
#   attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
# }).addTo(map);
#
# L.geoJson(statesData).addTo(map);
#
############################ #


#' Map - Open Google Maps in browser
#'
#' @param lat - Anything that can be handled by [sitepoints_from_any()].
#'   Leave unspecified to interactively browse to a .xlsx file that has lat,lon columns,
#'   or lat can be a data.frame with lat,lon column names in which case longitude should not be provided,
#'   such as \code{lat = testpoints_10[1,]}, or lat and lon can be separately provided as vectors.
#' @param lon longitude, or omit this parameter to provide points as the first parameter.
#' @param point logical optional, passed to [url_map_google()]
#' @param zoom zoomed out value could be 3 or 5, zoomed in default is 12
#' @param launch_browser logical, whether to launch browser
#' @return opens a browser window with Google Maps centered on the specified lat, lon
#' @examples # map_google(testpoints_10[1,])
#'
#' @export
#'
map_google <- function(lat, lon, zoom = 12, point = TRUE, launch_browser = TRUE) {

  urls = url_map_google(lat = lat, lon = lon, zoom = zoom, point = point)
  if (launch_browser && !shiny::isRunning()) {
    if (length(urls) > 1) {
      message("browsing to only the first URL out of ", length(urls))
    }
    browseURL(urls[1])
  }
  urls
}
########################################### #
