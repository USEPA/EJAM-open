
#' Get lat lon (and NAICS) via Facility Registry ID
#' 
#' @param regid Facility Registry Service ID like 110010052520
#'
#' @return data.table with columns 
#'   lat,lon,REGISTRY_ID,PRIMARY_NAME,NAICS,PGM_SYS_ACRNMS
#' @examples 
#'  latlon_from_regid(110070874073)
#' x = latlon_from_regid(
#'     c(110071293460, 110070874073, 110070538057, 110044340807,
#'        110030509215, 110019033810, 110056111559, 110056982323)
#'         )
#' 
#' @export
#'
latlon_from_regid <- function(regid) {
  
  if (missing(regid)) {return(NULL)} else if (all(is.na(regid)) | is.null(regid)){return(NULL)}
  
  if (!exists("frs")) dataload_from_pins("frs")
  
  frs[match(regid, frs$REGISTRY_ID), ] # slower but retains order
}
