
#' helper function - given indicator names, look up what type each is
#' 
#' @details  
#'   The types are things like raw data count for indicator, average, percentile, etc.
#'   Variable names are stored in column of map_headernames called newnames_ejscreenapi
#'   Types are stored in column of map_headernames called jsondoc_vartype
#' @param varname vector of 1 or more names

#' @param varnameinfo data.frame with info on type of each variable, like map_headernames
#'
#' @return vector same size as varname
#' @seealso [xls_varname2vartype()] [xls_vartype2color()] [xls_varname2color()]
#' 
#' @keywords internal
#' @export
#'
xls_varname2vartype <- function(varname, varnameinfo) {
  
  # for shading headers in Excel of results
  
  # unique( map_headernames[, c("varcategory", "DEJ", "varlist" 
  #   "jsondoc_shortzone", "raw_pctile_avg", "vartype", "jsondoc_vartype", "jsondoc_shortvartype")])

  if (missing(varnameinfo)) {
    if (exists('map_headernames'))  {varnameinfo <- map_headernames} else {
      warning('missing varnameinfo and map_headernames')
      return(rep(NA,length(varname)))
    }
  }
  varnameinfo[match(varname, varnameinfo[ , 'newnames_ejscreenapi'], nomatch = NA) , 'jsondoc_vartype']
}
################################################################################## #


#' helper function - assign fill color to shade excel cells by indicator type
#' 
#' Use color shading to make spreadsheet easier to use, grouping the indicators
#'
#' @param vartype must be one found in varnameinfo$jsondoc_vartype, 
#'   ie "percentile", "average", or "raw data for indicator"
#'   NA if not found.
#'
#' @return vector of colors like c('lightorange', 'gray')
#' @seealso [xls_varname2vartype()] [xls_vartype2color()] [xls_varname2color()]
#' 
#' @keywords internal
#' @export
#'
xls_vartype2color <- function(vartype) {
  
  # for shading headers in Excel of results
  
  # unique( map_headernames[, c("varcategory", "DEJ", "varlist" 
  #   "jsondoc_shortzone", "raw_pctile_avg", "vartype", "jsondoc_vartype", "jsondoc_shortvartype")])
  
  coloring <- matrix(
    c(
      # jsondoc_vartype 
      'percentile',              'lightorange',
      'raw data for indicator' , 'lightblue',
      'average',                 'gray' 
    ), 
    ncol = 2, byrow = TRUE
  )
  colnames(coloring) <- c('vartype', 'color')
  # but
  # jsondoc_zone
  # 'Region' , 'gray'
  coloring[match(vartype, coloring[, 'vartype'], nomatch = NA) , 'color']
}
################################################################################## #


#' helper function - for color coding excel sheet columns
#'
#' @param varname things like avg.pctlowinc 
#' @param varnameinfo data.frame with info on type of each variable
#'
#' @return vector of colors
#' @seealso [xls_varname2vartype()] [xls_vartype2color()] [xls_varname2color()]
#' 
#' @keywords internal
#' @export
#'
xls_varname2color <- function(varname, varnameinfo) {
  
  # for shading headers in Excel of results
  
  # unique( map_headernames[, c("varcategory", "DEJ", "varlist" 
  #   "jsondoc_shortzone", "raw_pctile_avg", "vartype", "jsondoc_vartype", "jsondoc_shortvartype")])
  
  if (missing(varnameinfo)) {
    if (exists('map_headernames'))  {varnameinfo <- map_headernames} else {
      warning('missing varnameinfo and map_headernames')
      return(rep('black', length(varname)))
    }
  }
  xls_vartype2color( xls_varname2vartype(varname = varname, varnameinfo = varnameinfo))
}
################################################################################## #
