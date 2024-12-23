
#' Utility to get quick view in RStudio viewer of datatable of ejscreenapi results
#' 
#' @details Also see functions in the EJAM package.
#' @param x output from ejscreenit(), not just the table element of that output,
#'   so it easier to do something like in the example.
#' @return output of DT::datatable()
#' @examples
#'   # out <- ejscreenit(testpoints_5, radius = 1)
#'   out <- testoutput_ejscreenit_5
#'   ejscreenit_see_table(out)
#'   out$map
#'   
#' @export
#'   
ejscreenit_see_table <- function(x) {
  
  y <- x$table
  y <- as.data.frame(y)
  radiuscolname <- ifelse("radius.miles" %in% colnames(y), "radius.miles", fixcolnames("radius.miles", "r", "long"))
  if (radiuscolname %in% colnames(y)) {radius <- y[1, radiuscolname]} else {radius <- NA}
  y$`EJScreen Report` <- url_ejscreen_report(lon = x$table$lon, lat = x$table$lat, radius = radius, 
                                             # areaid = "",
                                               as_html = TRUE, linktext = "Report")
  y$`EJScreen Map` <- url_ejscreenmap(lon = x$table$lon, lat = x$table$lat,
                                      #  areaid = "", # not a parameter
                                      as_html = TRUE, linktext = "EJScreen Map")

  names(y) <- fixcolnames(names(y), 'r', 'long')  # in case currently r format
  DT::datatable(y, escape = FALSE) # escape FALSE ensures URLs work as links in RStudio viewer
}
