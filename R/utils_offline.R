
#' utility to check if internet connection is not available
#'
#' @returns logical (TRUE or FALSE), TRUE if offline, FALSE if can connect to r-project.org
#' @param url optional URL checked, using [curl::nslookup()]
#' @seealso offline_warning() and offline_cat() utilities unexported undocumented,
#'   either returns the same as offline() does, 
#'   while also providing, if offline, warning() or cat() info to console
#' @keywords internal
#'
offline = function(url = "r-project.org") {
  
  hasinternet = !is.null(curl::nslookup(url, error = FALSE))
  return(!hasinternet)
}
################ # 

offline_warning = function(text = 'NO INTERNET CONNECTION AVAILABLE') {
  off <- offline()
  if (off) {warning(text)}
  return(off)
}
################ # 

offline_cat = function(text = 'NO INTERNET CONNECTION AVAILABLE\n') {
  off <- offline()
  if (off) {cat(text)}
  return(off)
}
################ # 
