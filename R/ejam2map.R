

#' Show EJAM results as a map of points
#' @description Takes the output of ejamit() and uses [mapfastej()] to 
#' create a map of the points.
#' @details Gets radius by checking ejamitout$results_overall$radius.miles
#' You can use browse=TRUE to save it as a shareable .html file
#' and see it in your web browser.
#' @param ejamitout output of ejamit()
#' @param radius radius in miles
#' @param column_names can be "ej", passed to [mapfast()]
#' @param launch_browser logical optional whether to open the web browser to view the map
#' @param shp shapefile it can map if analysis was for polygons, for example
#' @return like what [mapfastej()] returns
#' @examples
#' pts = testpoints_100
#' mapfast(pts)
#' 
#' # out = ejamit(pts, radius = 1)
#' out = testoutput_ejamit_100pts_1miles
#' 
#' # See in RStudio viewer pane
#' ejam2map(out, launch_browser = FALSE)
#' mapfastej(out$results_bysite[c(12,31),])
#' \donttest{
#' 
#' # See in local browser instead
#' ejam2map(out)
#' 
#' # Open folder where interactive map
#' #  .html file is saved, so you can share it:
#' x = ejam2map(out)
#' fname = map2browser(x)
#' # browseURL(normalizePath(dirname(fname))) # to open the temp folder
#' # file.copy(fname, "./map.html") # to copy map file to working directory
#' 
#' out <- testoutput_ejscreenapi_plus_5
#' mapfastej(out)
#' }
#' @export
#'
ejam2map <- function(ejamitout, radius = NULL, column_names = "ej", launch_browser = TRUE, shp = NULL) {
  # mydf, radius = 3, column_names='all', labels = column_names, 
  
  if (is.data.frame(ejamitout)) {
    # if it's a data.frame not the whole list output of ejamit(), assume it's the results_bysite, so make it look like we expected
    if (!("pop" %in% names(ejamitout))) {stop('ejamitout as passed to ejam2map should be either output of ejamit() or results_bysite element (table) from that output')}
    ejamitout <- list(results_bysite = ejamitout)
  }
  
  if (is.null(radius)) {
    radius <- ejamitout$results_bysite$radius.miles[1]
  }
  
  if (!is.null(shp)) {
    map_ejam_plus_shp(shp = shp,
                      out = ejamitout,
                      radius = radius,
                      launch_browser = launch_browser)
  } else {
    
    mapfast(mydf = ejamitout$results_bysite,
          radius = radius,
          column_names = column_names,
          launch_browser = launch_browser
          )
  }
}
############################################################################ #


#' quick way to open a map html widget in local browser (saved as tempfile you can share)
#'
#' @param x output of [ejam2map()] or [mapfastej()] or [mapfast()]
#'
#' @return launches local browser to show x, but also returns
#'   name of tempfile that is the html widget
#' @inherit ejam2map examples
#' 
#' @export
#' 
map2browser = function(x) {
  
  if (!interactive()) {
    stop("must be in interactive mode in R to view html widget this way")
  }
  mytempfilename = file.path(tempfile("map", fileext = ".html"))
  htmlwidgets::saveWidget(x, file = mytempfilename)
  mytempfilename <- normalizePath(mytempfilename) # helps it work in MacOS
  browseURL(mytempfilename)
  cat("HTML interactive map saved as in this directory:\n",
      dirname(mytempfilename), "\n",
      "with this filename:\n", 
      basename(mytempfilename), "\n",
      "You can open that folder from RStudio like this:\n",
      paste0("browseURL('", dirname(mytempfilename), "')"), "\n\n")
  return(mytempfilename)
}
############################################################################ #
