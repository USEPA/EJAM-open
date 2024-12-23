

#' Utility to print summary info about the output of ejamit() or doaggregate()
#' 
#' @param x the output of [ejamit()] or of [doaggregate()],
#'   a list of objects holding results of analysis
#' @param maxshown shows only first 10 elements of list by default
#' @param objectname name to use in printing summary, e.g., "Output of ejamit()"
#'   or default is to use the name of the object passed to this function.
#' @return data.frame summarizing names of list, whether each element is a 
#'   data.table, data.frame, or vector, and rows/cols/length info
#' @examples  
#'   structure.of.output.list(testpoints_10)
#'   structure.of.output.list(testoutput_getblocksnearby_10pts_1miles)
#'   structure.of.output.list(testoutput_doaggregate_10pts_1miles)
#'   structure.of.output.list(testoutput_ejamit_10pts_1miles)
#'   structure.of.output.list(testoutput_ejscreenapi_plus_5)
#'   structure.of.output.list(testoutput_ejscreenit_5)
#'   
#' @keywords internal
#' 
structure.of.output.list <- function(x, maxshown=10, objectname = NULL) {
  
  if (is.null(objectname)) {
  nameofx  <- deparse1(substitute(x))
  cat(paste0("\n'", nameofx, "' is a"), paste0(class(x), collapse = " and "), ' with these elements:\n\n')
  } else {
    cat(paste0("\n", objectname, " is a"), paste0(class(x), collapse = " and "), ' with these elements:\n\n')
  }
  
  if (is.data.frame(x) | is.atomic(x)) {x <- list(x); names(x) <- nameofx } 
  if (is.list(x) & length(x) > maxshown) {
    message("Showing first ", maxshown," elements of list. To see more, try using parameter maxshown.")
    # if (data.table::is.data.table(x)) {
    #   x <- data.table::copy(x)
    #   x <- x[,1:maxshown]
    # } else {  
    #   x <- x[1:maxshown] 
    # }
    x <- x[1:maxshown]
  }
  y = data.frame(
    Name = names(x), 
    # data.frame = sapply(x, is.data.frame), 
    format = sapply(x, function(z) ifelse(
      data.table::is.data.table(z), "data.table", ifelse(
        is.data.frame(z), "data.frame", ifelse(
          is.vector(z), "vector", "other")))),
    rows =   sapply(x, function(z) ifelse(is.null(nrow(z)), NA, nrow(z)) ), 
    cols =   sapply(x, function(z) ifelse(is.null(ncol(z)), NA, ncol(z)) ),
    vectorlength = sapply(x, function(z) ifelse(is.vector(z), length(z), NA)), 
    
    row.names = NULL)
  
  return(y)
}
