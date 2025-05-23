#' Clean EPA FRS SIC info
#' @details works just like [frs_clean()]
#'   but for SIC codes instead of NAICS
#' @param frs the frs data object from frs_read() 
#' @param usefulcolumns optional
#' @return frs data.table
#' @seealso  [frs_update_datasets()]
#' 
frs_clean_sic <- function(frs, usefulcolumns = c(
  "lat", "lon", "SIC",
  "LATITUDE83", "LONGITUDE83", "REGISTRY_ID", "PRIMARY_NAME", "SIC_CODES", "PGM_SYS_ACRNMS"
)) {
  
  # if (!all(usefulcolumns %in% names(frs))) {
  #   warning(paste0(
  #     "These colnames are not in frs passed to frs_clean_sic() :", 
  #     paste0(
  #       setdiff(usefulcolumns, names(frs)), 
  #       collapse = ", "
  #       )
  #     ))
  #   }
  usefulcolumns <- intersect(usefulcolumns, names(frs))
  
  frs <- frs[, ..usefulcolumns]
  
  # these are redundant and would fail if already done by frs_clean()
  if ("LATITUDE83" %in% names(frs)) {setnames(frs, "LATITUDE83", "lat")}
  if ("LONGITUDE83" %in% names(frs)) {setnames(frs, "LONGITUDE83", "lon")}
  if ("SIC_CODES" %in% names(frs)) {setnames(frs, "SIC_CODES", "SIC")}
  
  cat("Finished cleaning file.\n")
  cat("Total rows: ", NROW(frs), "\n")
  frs <- frs[!is.na(frs$lat), ]
  cat("Rows with lat/lon: ", NROW(frs), "\n")
  data.table::setkey(frs, REGISTRY_ID)
  
  return(frs)
}
###################################################################################
