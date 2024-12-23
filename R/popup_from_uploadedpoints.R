#' Map popups - make simple popups for map to show info about uploaded points
#'
#' @param mypoints data.frame (or tibble?) with lat and lon columns preferably
#' @param n Show the first n columns of mypoints, in popup. "all" means all of them.
#' @seealso [popup_from_any()]
#' @return popups vector to be used in leaflet maps
#' 
#' @keywords internal
#' @export
#'
popup_from_uploadedpoints <- function(mypoints, n="all") {
  
  if (n == 'all' | n > NCOL(mypoints)) {
    return(popup_from_df(mypoints))
  } else {
    return(popup_from_df(mypoints[ , 1:n]))
  }
}
############################################################################## #


