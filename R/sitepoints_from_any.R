

#' Get lat/lon flexibly - from file, data.frame, data.table, or lat/lon vectors
#' Like latlon_from_anything() but this also adds a ejam_uniq_id column
#' @aliases sitepoints_from_anything
#' @param anything see [latlon_from_anything()], which this is passed to
#' @param lon_if_used see [latlon_from_anything()], which this is passed to
#' @param invalid_msg_table set to TRUE if you want columns "valid" and "invalid_msg" also 
#' @param set_invalid_to_na used by latlon_df_clean()
#' @param interactiveprompt passed to [latlon_from_any()]
#' 
#' @return data.frame with lat,lon, and ejam_uniq_id as colnames, one row per point
#' @examples
#'  sitepoints_from_any(testpoints_10)
#'  sitepoints_from_any(lon_if_used = testpoints_10$lon, anything = testpoints_10$lat)
#'  sitepoints_from_any(testpoints_10$lat, testpoints_10$lon)
#'  pts = c("33,-100", "32,-101")
#'  sitepoints_from_any(pts)
#'  pts = data.frame(Longitude = testpoints_10$lon, Latitude = testpoints_10$lat)
#'  sitepoints_from_any(pts)
#'  pts = data.table(Lat = testpoints_10$lat, Long = testpoints_10$lon)
#'  sitepoints_from_any(pts)
#'  
#'  sitepoints_from_anything(testpoints_bad, set_invalid_to_na = F, invalid_msg_table = T)
#'  
#'  \dontrun{
#'  if (interactive()) {
#'    pts <- sitepoints_from_any()
#'  }}
#'  \dontrun{
#'  pts = system.file("testdata/latlon/testpoints_10.xlsx", package = "EJAM")
#'  sitepoints_from_any(pts)
#'  }
#' @export
#'
sitepoints_from_any <- function(anything, lon_if_used, invalid_msg_table = FALSE, set_invalid_to_na = TRUE, interactiveprompt = TRUE) {

  # note this overlaps or duplicates code in ejamit() and app_server.R
  #   for data_up_latlon() around lines 81-110 and data_up_frs() at 116-148
  
  # Doing these steps here too, even though ejamit() has the same code, 
  # so it won't have to happen once per loop on radius in ejamit_compare_distances_fulloutput() or ejamit_compare_distances()
  
  # However, for multiple site types as in ejamit_compare_types_of_places()...? 
  
  
  # If user entered a table, path to a file (csv, xlsx), or whatever can be handled -- see latlon_from_anything() --
  # read it to get the lat lon values from there
  #  by using sitepoints <- latlon_from_anything() which also gets done by getblocksnearby()

  # can add columns "valid" and "invalid_msg" 
  sitepoints <- latlon_from_anything(anything, lon_if_used, invalid_msg_table = invalid_msg_table, set_invalid_to_na = set_invalid_to_na, interactiveprompt = interactiveprompt)

  stopifnot(
    is.data.frame(sitepoints), 
    "lat" %in% colnames(sitepoints), "lon" %in% colnames(sitepoints), 
    NROW(sitepoints) >= 1, is.numeric(sitepoints$lat)
  )
  
  ## check for ejam_uniq_id column;  add if not present
  if ('ejam_uniq_id' %in% names(sitepoints)) {
    if (!isTRUE(all.equal(sitepoints$ejam_uniq_id, seq_along(sitepoints$ejam_uniq_id)))) {
      message("Note that ejam_uniq_id was already in sitepoints, and might not be 1:NROW(sitepoints), which might cause issues")
    }
  }
  if (!("character" %in% class(sitepoints)) && !'ejam_uniq_id' %in% names(sitepoints)) {
    # message('sitepoints did not contain a column named ejam_uniq_id, so one was added')
    sitepoints$ejam_uniq_id <- seq.int(length.out = NROW(sitepoints))
  }
  # if (invalid_msg_table) {
  #   sitepoints <- latlon_is.valid(lat = sitepoints$lat, lon = sitepoints$lon, invalid_msg_table = TRUE)
  # } # latlon_from_anything() above already used latlon_is.valid() and using it directly here would return ONLY those 2 validity columns
  
  return(sitepoints)
}
################################################################################## #  

#' @export
#' @inherit sitepoints_from_any
sitepoints_from_anything <- function(anything, lon_if_used, invalid_msg_table = FALSE, set_invalid_to_na = TRUE, interactiveprompt = TRUE) {
  sitepoints_from_any(anything, lon_if_used, invalid_msg_table = invalid_msg_table, set_invalid_to_na = set_invalid_to_na,  interactiveprompt = interactiveprompt)
}
