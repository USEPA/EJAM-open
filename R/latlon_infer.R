
#' Guess which columns have lat and lon based on aliases like latitude, FacLat, etc.
#'
#' @param mycolnames e.g., colnames(x) where x is a data.frame from read.csv
#'
#' @return returns all of mycolnames except replacing the best candidates with lat and lon
#' @seealso latlon_df_clean() latlon_is.valid() latlon_as.numeric()[fixnames_aliases()] [fixcolnames_infer()]
#'
#' @examples 
#'   latlon_infer(c('trilat', 'belong', 'belong')) # warns if no alias found, 
#'     #  but doesnt warn of dupes in other terms, just preferred term.
#'   latlon_infer(c('a', 'LONG', 'Longitude', 'lat')) # only the best alias is converted/used
#'   latlon_infer(c('a', 'LONGITUDE', 'Long', 'Lat')) # only the best alias is converted/used
#'   latlon_infer(c('a', 'longing', 'Lat', 'lat', 'LAT')) # case variants of preferred are 
#'       # left alone only if lowercase one is found
#'   latlon_infer(c('LONG', 'long', 'lat')) # case variants of a single alias are 
#'       # converted to preferred word (if pref not found), creating dupes!  warn!
#'   latlon_infer(c('LONG', 'LONG')) # dupes of an alias are renamed and still are dupes! warn!
#'   latlon_infer(c('lat', 'lat', 'Lon')) # dupes left as dupes but warn!
#'
#' @keywords internal
#'
latlon_infer <- function(mycolnames) {
  
  if (missing(mycolnames)){
    warning('No value provided for argument "mycolnames".')
    return(NULL)
  }
  else if(all(is.na(mycolnames)) | is.null(mycolnames)){
    warning('NULL or NA "mycolnames" passed as inputs.')
    return(NULL)
  }
  x <- mycolnames
  if (!is.atomic(x) || !is.vector(x)) {
    if (is.data.frame(x)) {
      stop("latlon_infer() requires a vector of colnames(yourdata.frame) as input, not a data.frame")
    } else {
      stop("latlon_infer() requires a vector of colnames as input")
    }
  }
  if (all(is.na(x))) {warning("all of mycolnames were NA")} else {
    if (any(is.na(x))) {warning("some of mycolnames were NA")}
  }
  
  infer <- function(lword, x) {
    if (!(lword %in% x)) {
      if (lword == 'lat') {
        # try to infer lat, using these in order of preferred to less
        # aliases <- tolower(c('lat', 'latitude83', 'latitude', 'latitudes', 'faclat', 'lats', "y"))
        aliases <- lat_alias
      }
      if (lword == 'lon') {
        # try to infer lon, using these in order of preferred to less
        # aliases <- tolower(c('lon', 'longitude83', 'longitude', 'longitudes', 'faclong', 'lons','long', 'longs', 'lng', "x"))
        aliases <- lon_alias
      }
      
      bestfound <- intersect(aliases, tolower(x))[1] 
      # bestfound <- x[which.min( match(x, aliases ) )] # another way
      if (is.na(bestfound)) { # intersect()[1] returns NA if none
        warning(paste0(lword, ' missing and no synonyms found')) # do not change x at all
      } else {
        # ignoring case, replace any exact match(es) to that one word. # should ideally confirm unique?
        x <- gsub(paste0('^', bestfound, '$'), lword, x, ignore.case = TRUE)
      }
    }
    if (sum(grepl(paste0('^', lword, '$'), x)) > 1) {warning(paste0('DUPLICATED ', lword))}
    x
  }
  
  x <- infer('lat', x)
  x <- infer('lon', x)
  if (!isTRUE(all.equal(x, mycolnames))) {
    message("Replaced column names that were inferred to be and therefore renamed as the lat and/or lon columns!")
  }
  return(x)
}
