

#' Show EJAM results as a map of points
#' @description Takes the output of ejamit() and uses [mapfastej()] to 
#' create a map of the points.
#' @details Gets radius by checking ejamitout$results_overall$radius.miles
#' You can use browse=TRUE to save it as a shareable .html file
#' and see it in your web browser.
#' @param ejamitout output of ejamit()
#' @param radius radius in miles
#' @param column_names can be "ej", passed to [mapfast()]
#' @param browse logical optional whether to open the web browser to view the map
#' @return like what [mapfastej()] returns
#' @examples
#' pts = testpoints_100
#' mapfast(pts)
#' 
#' # out = ejamit(pts, radius = 1)
#' out = testoutput_ejamit_100pts_1miles
#' 
#' # See in RStudio viewer pane
#' ejam2map(out)
#' mapfastej(out$results_bysite[c(12,31),])
#' \dontrun{
#' 
#' # See in local browser instead
#' ejam2map(out, browse = T)
#' 
#' # Open folder where interactive map
#' #  .html file is saved, so you can share it:
#' x = ejam2map(out)
#' fname = map2browser(x)
#' # browseURL(dirname(fname)) # to open the temp folder
#' # file.copy(fname, "./map.html") # to copy map file to working directory
#' 
#' out <- testoutput_ejscreenapi_plus_5
#' mapfastej(out)
#' }
#' @export
#'
ejam2map <- function(ejamitout, radius = NULL, column_names = "ej", browse = FALSE) {
  # mydf, radius = 3, column_names='all', labels = column_names, 
  if (is.null(radius)) {
    radius <- ejamitout$results_bysite$radius.miles[1]
  }
  mapfast(mydf = ejamitout$results_bysite,
          radius = radius,
          column_names = column_names,
          browse = browse
          )
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
