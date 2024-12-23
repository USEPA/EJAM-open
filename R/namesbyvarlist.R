

#' Get indicator names within a varlist like names_d
#' @aliases varlist2names
#' @details
#'   varlist2names() is a way to just get a vector of variable names even if the varlist is not stored as a separate data object
#'   and is only found in the map_headernames$varlist column: 
#'   
#'   varlist2names(c('names_d', 'names_d_subgroups'))
#'   
#'    c(names_d, names_d_subgroups)
#'   
#' @param varlist one character string like "names_d", or a vector of them
#' @param nametype vector of 1 or more names of columns in map_headernames, or a shortcut type that can be
#'   api, csv, r, original, long, shortlabel
#' 
#' @return  a data.frame one row per indicator, one col per nametype and a column identifying the varlist
#'
#' @examples
#'  unique(map_headernames$varlist)
#'  
#'  namesbyvarlist('names_e_avg', 'rname')
#'  namesbyvarlist('names_d')
#'  namesbyvarlist('names_d', 'r')
#'  namesbyvarlist('names_d', 'long')
#'  namesbyvarlist('names_d', 'shortlabel')
#'  
#'  namesbyvarlist( 'names_e_pctile', c('r', 'longname'))    
#'  namesbyvarlist(c('names_e_pctile', 'names_e_state_pctile'), 
#'    c('varlist', 'rname', 'apiname', 'csvname', 'shortlabel', 'longname'))
#' @seealso [names_from_varlist()] [varlist2names()] [varin_map_headernames()] [varinfo()] [names_whichlist_multi_key()] [vnames()]
#' 
#' @keywords internal
#' @export
#'
namesbyvarlist <- function(varlist, nametype=c('rname','longname','apiname')[1]) {
  
  for (i in 1:length(nametype)) {
    nametype[i] <- switch(nametype[i], 
                           api = 'apiname', # if they say oldtype="api" then look up each of namesnow within the column map_headernames$apiname 
                           csv = 'csvname',
                           r =   'rname',
                          acs = 'acsname',
                           original = 'oldname',   # which might be csvname2.2 or apiname or rname
                          shortlabel = 'shortlabel', 
                          long = 'longname',     
                           nametype[i]) # if no match above, use as-is
  }
  if (!(any(varlist %in% unique(map_headernames$varlist)))) {stop('specified varlist is not found among map_headernames$varlist')}
  if (any(!(nametype %in% names(map_headernames)))) {stop('specified nametype not a known type and not found among colnames(map_headernames)')}
  
  if (length(nametype) > 1) {
    return( map_headernames[map_headernames$varlist %in% varlist, c('varlist', nametype), drop = FALSE] )
    
    # out <- list()
    # for (i in 1:length(nametype)) {
    #   out[[i]] <- map_headernames[map_headernames$varlist %in% varlist, nametype[i]]
    # }
    # return(out)
  } else {
    return( map_headernames[map_headernames$varlist %in% varlist, c('varlist', nametype), drop = FALSE] )
  }
}
########################################## #
#' @keywords internal
#' @export
names_from_varlist <- function(vlist) {
  namesbyvarlist(vlist)$rname
}
########################################## #
#' @keywords internal
#' @export
#'
varlist2names <- function(varlist) {
  namesbyvarlist(varlist)$rname
}
########################################## #


