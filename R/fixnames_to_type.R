
#' helper function to change elements of namesnow from an oldtype to a newtype of names
#' 
#' @description helps convert between original, friendly, and long versions of variable names
#' @details YOU NEED TO SPECIFY NAMES OF COLUMNS IN MAP_HEADERNAMES, like "apiname" or "rname",
#'   UNLIKE IN fixnames() or fixcolnames() where you specify a type like "long" or "api"
#'   Using lookup table mapping_for_names, finds each namesnow 
#'   in the column specified by oldtype
#'   and replaces it with the corresponding string in the column specified by newtype
#' @param namesnow vector of strings, such as from colnames(x)
#' @param mapping_for_names data.frame passed to [fixnames()] to do the work
#'   with colnames that are referred to by oldtype and newtype
#' @param oldtype string with name of a column in data.frame mapping_for_names,
#'   and that column has old column names that overlap with those in namesnow, 
#'   like "ejscreen_csv" aka "csvname2.2" or 
#'   like "ejscreen_api" aka "apiname" or
#'   like "rname" maybe aka "newnames_ejscreenapi"
#' @param newtype string with name of a column in data.frame mapping_for_names, 
#'   like "rname" maybe aka "newnames_ejscreenapi" or
#'   like  "longname_tableheader" or "shortlabel"
#'   and that column has old column names that overlap with those in namesnow
#' @seealso varinfo() from EJAM pkg, and [fixnames_to_type()] [fixcolnames()] [fixnames()]
#' @return Vector or new column names same length as input
#' 
#' @keywords internal
#' @export
#' 
fixnames_to_type <- function(namesnow, oldtype='apiname', newtype='rname', mapping_for_names) {

  if (missing(mapping_for_names)) {
    if (exists('map_headernames')) {
      mapping_for_names <- map_headernames
    } else {
      warning('Cannot rename. Returning unchanged names. Must specify valid mapping_for_names in function or map_headernames must be in global env')
      return(namesnow)
    }
  } else {
    # x <- try(exists(mapping_for_names)) # this does not work as intended
    # if (inherits(x, "try-error")) {
    #   warning('Cannot rename. Returning unchanged names. Must specify valid mapping_for_names in function or map_headernames must be in global env')
    #   return(namesnow)
    # } else {
    #   if (x & is.data.frame(mapping_for_names)) {
    #     # it is a df that exists, and valid colnames are checked for later
    #   } else {
    #     warning('Cannot rename. Returning unchanged names. Must specify valid mapping_for_names in function or map_headernames must be in global env')
    #     return(namesnow)
    #   }
    # }
  }
  
  if (!(newtype %in% colnames(mapping_for_names)))   {warning(paste('returning unchanged names because mapping_for_names has no column called ', newtype))
    return(namesnow)}
  if (!(oldtype %in% colnames(mapping_for_names))) {warning(paste('returning unchanged names because mapping_for_names has no column called ', oldtype))
    return(namesnow)}
  
  
  oldnames <- mapping_for_names[ , oldtype]
  newnames <- mapping_for_names[ , newtype]
  
  newnames <- newnames[oldnames %in% namesnow]
  oldnames <- oldnames[oldnames %in% namesnow]
  namesnow[match(oldnames, namesnow)] <- newnames
  return(namesnow)
}
