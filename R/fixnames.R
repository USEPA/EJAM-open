
#' like fixcolnames(), a helper function to rename variables that are colnames of data.frame
#' 
#' Changes column names to R variable names from original API names in FTP site file
#' 
#' @details YOU CAN SPECIFY A TYPE USING AN ALIAS LIKE 
#'   "api" or "long" UNLIKE IN [fixnames_to_type()] where
#'   you had to specify the actual colnames of map_headernames, like "apiname"
#'   
#'   NOTE: If you happen to pass the entire data.frame or data.table to this function, 
#'   instead of passing just the colnames, this function will see that and still 
#'   return just a vector of new colnames
#' @param namesnow vector of colnames (but can be a data.frame or data.table too)
#' @param oldtype friendly or long or original, or csv or r or api
#' @param newtype friendly or long or original, or csv or r or api
#' @param mapping_for_names data.frame passed to [fixnames()] to do the work.
#' 
#' @seealso [varinfo()]  [fixnames_to_type()] [fixcolnames()] [fixnames()]
#' @return Vector or new column names same length as input. 
#'   The function does NOT return an entire renamed df or dt. Just the new colnames are returned.
#' 
#' @keywords internal
#' 
fixnames <- function(namesnow, oldtype="api", newtype="r", mapping_for_names) {
  
  if (missing(mapping_for_names)) {
    if (exists('map_headernames')) {
      mapping_for_names <- map_headernames
    } else {
      warning('Cannot rename. Returning unchanged names. Must specify valid mapping_for_names in function or map_headernames must be in global env')
      return(namesnow)
    }
  } else {
  }
  # if they provided the whole table by accident or on purpose, note that and still just return new colnames
  
  if (is.data.frame(namesnow) | is.matrix(namesnow) | is.list(namesnow) ) {
    # this is also true if it is a data.table 
    # warn? no this is probably useful flexibility and is documented, so it seems ok to not warn
    
     if (is.matrix(namesnow)) {
      actual_names <- colnames(namesnow) # names() will not work if it is just a matrix 
    } else {
        actual_names <- names(namesnow) # colnames() will not work if it is just a named list() but not df or dt
      }
    
    updated_names <- fixcolnames(actual_names, oldtype = oldtype, newtype = newtype, mapping_for_names = mapping_for_names)
    # if (data.table::is.data.table(namesnow)) {
    ### Do we want to and can we rename by reference within this function? 
    ##  I think we could but it would be confusing to just return names sometimes and silently rename dt other times
    # data.table::setnames(namesnow, old = actual_names, new = updated_names)
    # }
    return(updated_names)
  }
  
  return(
    fixcolnames(namesnow, oldtype = oldtype, newtype = newtype, mapping_for_names = mapping_for_names)
    )
}
