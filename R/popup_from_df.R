

#' Map popups - Simple map popup from a data.frame, one point per row
#' MAY DEPRECATE - See popup_from_any() for newer more general replacement for this.
#' 
#' @details Each popup is made from one row of the data.frame. 
#'   Each popup has one row of text per column of the data.frame
#' @param x data.frame with info to be shown in map popups
#' @param n Show the first n columns of mypoints, in popup. "all" means all of them.
#' @param labels default is colnames(x) - vector used to label 
#'   the elements in the popup. Must be same length as column_names
#' @param column_names default is all, or a vector of column names from x to use 
#'
#' @return A vector of strings, one per row or map point, 
#'   with a line break separating column elements
#'
#' @examples  
#'  df <- structure(list(
#'    RegistryId = c("110071102551", "110015787683"),
#'    FacilityName = c("USDOI FWS AK MARITIME NWR etc", "ADAK POWER PLANT"),
#'    LocationAddress=c("65 MI W. OF ADAK NAVAL FACILITY","100 HILLSIDE BLVD"),
#'    CityName = c("ADAK", "ADAK"),
#'    CountyName = c("ALEUTIAN ISLANDS", "ALEUTIANS WEST"),
#'    StateAbbr = c("AK", "AK"),
#'    ZipCode = c("99546", "99546"),
#'    FIPSCode = c("02010", "02016"),
#'    lat = c(51.671389,51.8703), lon = c(-178.051111, -176.659),
#'    SupplementalLocation = c(NA_character_,NA_character_)),
#'    row.names = 1:2, class = "data.frame")
#'  leaflet::leaflet(df) |> leaflet::addTiles() |> 
#'    leaflet::addPopups(popup = popup_from_df(df))
#'
#' @export
#' 
popup_from_df <- function(x, column_names=names(x), labels=column_names, n="all") {
  
  if (data.table::is.data.table(x)) x <- as.data.frame(x) # need to confirm/check needed/works with this change
  if (!(is.data.frame(x))) {x <- as.data.frame(x)}
  if (n == 'all' | n > NCOL(x)) {
    # x <- x
  } else {
    x <- x[ , 1:n, drop = FALSE]  #  drop = FALSE  is in case only 1 column specified
  }
  if (missing(column_names)) {column_names <- names(x)}
   if (any(!(column_names %in% names(x)))) {
     stop("some column_names not found in x for popup")
     }
  
  
  if (length(labels) != length(column_names)) {labels = column_names; warning('column_names and labels must be same length. Using column_names as labels.')}
  # x <- x[ , names(x) %in% column_names]
  x <- x[ ,  column_names, drop = FALSE] # , drop = FALSE  is in case only 1 column specified
  
  for (i in 1:NCOL(x)) { x[,i] <- paste(labels[i] , x[,i], sep = ': ' )}
  
  as.vector(apply(x, MARGIN = 1, FUN = function(thisrow) paste(thisrow, collapse = '<br>')) )
}
############################################################################## #
