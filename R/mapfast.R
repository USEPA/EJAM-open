

#' Map - points - Create leaflet html widget map of points using EJAM results with EJ stats
#'
#' Like [mapfast()] but with column_names = "ej"
#'
#' @inheritParams mapfast
#' @inherit ejam2map examples
#' @return like what [mapfast()] returns
#' @export
#'
mapfastej <- function(mydf, radius = 3, column_names = 'ej', labels = column_names, browse = FALSE, color = "#03F") {
  
  mapfast(mydf = mydf, radius = radius, column_names = column_names, labels = labels, browse = browse, color = color)
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
#'   e.g., as from [names2fips('DE')] or ejamit(fips='01')$results_bysite.
#'
#' @param radius in miles, converted to meters and passed to leaflet::addCircles()
#' @param column_names If "ej" then nice popup made based on just key EJScreen
#'   indicators. If "all" then every column in the entire mydf table is shown
#'   in the popup. If a vector of colnames, only those are shown in popups.
#' @param labels The labels used before the column_names, for map popups,
#'   like  label: column_name  (ignored if column_names is ej or all)
#' @param browse optional logical, set to TRUE if you want the function to
#'   launch a default browser window to show the map
#'   and print the temp filepath and filename in the console.
#'   Normally the map would be shown in the default RStudio viewer pane.
#' @param color color of circles, default is "#03F"
#' @seealso [ejam2map()] [popup_from_any()] [mapfastej()]
#' @return plots a leaflet map with popups with all the columns from mydf,
#'   and returns html widget
#' @import leaflet
#' @import leaflet.extras2
#' @inherit ejam2map examples
#'
#' @export
#'
mapfast <- function(mydf, radius = 3, column_names='all', labels = column_names, browse = FALSE, color = "#03F") {
  
  # if the whole list from ejamit(), not a data.frame, was provided
  if (is.list(mydf) && 'results_bysite' %in% names(mydf)) {
    warning("mydf seems to be a list of tables such as output from ejamit() so using just the results_bysite table here")
    if ("sitetype" %in% names(mydf)) {
      sitetype <- mydf$sitetype
    } else {sitetype <- NULL}
    mydf <- mydf$results_bysite
  } else {sitetype <- NULL}
  # if data.table was provided
  if (data.table::is.data.table(mydf)) {mydf <- as.data.frame(mydf)} # in case it was a data.table. note this could be slow as it makes a copy, but setDF(mydf) would alter mydf by reference in the calling envt.
  
  # colnames ####
  ## infer lat lon cols (if they exist) ####
  # use new names for lat and lon just to check those values and to use this as data sent to leaflet, but show fixcolnames for names in popup
  renamed <- mydf 
  names(renamed) <- latlon_infer(names(renamed))
  ## fixcolnames() ####
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
      warning('Not all EJ index columns found. Using NA values for all EJ indexes in map popups.')
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
    
    x <- map_shapes_leaflet(mydf, popup = mypop)
    xok = TRUE
    
    # cat('For analysis and map of shapefile data, you can try something like this:
    #   
    #       shp <- shapefile_from_any()
    #       m <- map_shapes_leaflet(shp)
    #       # or
    #       out <- ejamit(shapefile = shp)
    #       ejam2map(out)
    #       # or
    #        mapfast(out)
    #       # or
    #       shp <- ejam2shapefile(out, save = FALSE)
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
    
    x <- leaflet::leaflet(data = renamed) |> leaflet::addTiles() |>
      leaflet::addCircles(lng = ~lon, lat = ~lat, radius = radius.meters, color = color,
                          popupOptions = list(maxHeight = 400, maxWidth = 850),
                          popup = mypop) |>
      leaflet.extras2::addEasyprint( ) # button to print or print to pdf and save
    xok = TRUE
    # now x is a map
  }
  ######################################################### #
  
  # *FIPS not shp not latlon ####
  
  if (sitetype == 'fips') {
    
    ftype <- fipstype(mydf$ejam_uniq_id)
    
    ######################### #
    # _States  ####
    # get boundaries  
    
    if (all(ftype %in% 'state')) {
      fips <- mydf$ejam_uniq_id
      shp <- shapes_from_fips(fips) #  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      x <- map_shapes_leaflet(shp, popup = mypop)
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
      # x <- map_shapes_leaflet(shp, popup = mypop)
      # xok = TRUE
      
      x <- mapfastej_counties(mydf) # handles popups, ignores params above, assumes mydf$ejam_uniq_id is fips
      xok <- TRUE
    }
    ######################### #
    # _City/CDPs  #### 
    # get boundaries 
    
    if (all(ftype %in% 'city')) {
      fips <- mydf$ejam_uniq_id
      # shp <- shapes_places_from_placefips(fips)
      shp <- shapes_from_fips(fips) #  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      x <- map_shapes_leaflet(shp, popup = mypop)
      xok = TRUE
    }
    ######################### #
    # _Tracts ####
    if (all(ftype %in% 'blockgroup')) {
      fips <- mydf$ejam_uniq_id
      shp <- shapes_from_fips(fips) #  SLOW if many, like > 20  #  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      x <- map_shapes_leaflet(shp, popup = mypop)
      xok <- TRUE
    }
    ######################### #
    # _Blockgroups ####
    
    if (all(ftype %in% 'blockgroup')) {
      fips <- mydf$ejam_uniq_id
      # shp <- shapes_blockgroups_from_bgfips(fips)
      shp <- shapes_from_fips(fips) #  SLOW if many, like > 20  #  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      x <- map_shapes_leaflet(shp, popup = mypop)
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
  
  if (browse) {
    # map2browser() would do the same
    htmlwidgets::saveWidget(x, file = fname <- tempfile("mapfast_", fileext = ".html"))
    # htmltools::save_html(x, file = fname <- tempfile("mapfast_", fileext = ".html"))  # might work also?
    browseURL(fname)
    cat(fname, "\n")
  }
  
  return(x)
}
############################################################################ #
