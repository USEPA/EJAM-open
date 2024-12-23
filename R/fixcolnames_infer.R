

#' convert colnames to standardized names, via aliases, but change only best match for each standard name
#' Used by address_from_table()
#' @details [fixcolnames_infer()] and [fixnames_aliases()] are very similar.
#'   and latlon_infer() is also very similar.
#' 
#'   - [fixcolnames_infer()] is designed to figure out for a data.frame
#'   which one column is the best guess (top pick) for which should be 
#'   used as the "lat" column, for example,
#'   so when several colnames are matches to one preferred name,
#'   based on the alias_list,
#'   this function picks only one of them to rename to the preferred or
#'   canonical name, leaving others as-is.
#'   
#'   - In contrast to that, [fixnames_aliases()] is more general and
#'   every input element that can be matched with a
#'   canonical name gets changed to that preferred version, so
#'   even if multiple input names are different aliases of "lat",
#'   for example, they all get changed to "lat."
#'
#' @param currentnames vector of colnames that may include aliases
#' @param alias_list optional named list where
#'   names are standard colnames like "street"
#'   and each named element in list is a vector of aliases
#'   for that standard name
#' @param ignore.case whether to ignore case in matching to aliases
#' @param verbose set to TRUE for testing/ to check what this function does
#' @return vector like currentnames but some renamed to a
#'   standard name if alias found, ignoring case.
#'   
#' @seealso [latlon_infer()] [fixnames_aliases()] that is almost the same
#' 
#' @export
#'
fixcolnames_infer <- function(currentnames, 
                              alias_list = list(
                                lat = lat_alias, 
                                lon = lon_alias,
                                address = c("address"),
                                street = c("street", "street address", "address1", "address 1"),
                                city = c("city", "cityname", "city name"),
                                state = c("state", "mystate", "statename", "ST"),
                                zip = c("zip", "zipcode", "zip code")
                              ), 
                              ignore.case = TRUE, 
                              verbose = FALSE) {
  
  ######################################## #
  # see also fixnames_aliases1() in fixnames_aliases()
  
  fixcolname1_infer <- function(currentnames, standardname, aliases = NULL, ignore.case = TRUE) {
    if (missing(currentnames)) stop('currentnames missing')
    if (missing(standardname)) stop('standardname missing')
    
    if (standardname %in% currentnames) {
      currentnames_now <- currentnames
    } else {
      if (standardname == 'lat') {
        # try to infer lat, using these in order of preferred to less
        # aliases <- tolower(c('lat', 'latitude83', 'latitude', 'latitudes', 'faclat', 'lats'))
        aliases <- lat_alias
      }
      if (standardname == 'lon') {
        # try to infer lon, using these in order of preferred to less
        # aliases <- tolower(c('lon', 'longitude83', 'longitude', 'longitudes', 'faclong', 'long', 'longs', 'lons','lng'))
        aliases <- lon_alias
      }
      if (is.null(aliases)) stop('aliases missing - must be provided unless standardname is lat or lon') # 
      
      # compare all currentnames to all aliases of this 1 standarname being sought (e.g., lat)
      # bestfound should be the one of the currentnames that is the first to match any alias, going in order of aliases preferred to least
      # 
      if (ignore.case) {
        # bestfound <- intersect(aliases, tolower(currentnames))[1]
        # aliases[match(tolower(currentnames), tolower(aliases) ) ] # should ignore case but return alias in whatever case it is stored as
        bestfound <- na.omit(currentnames[match(tolower(aliases), tolower(currentnames))])[1]
      } else {
        bestfound <- na.omit(currentnames[match(aliases, currentnames)])[1]
      }
      if (is.na(bestfound)) {
        # warning(paste0(standardname, ' missing and no synonyms found')) # do not change currentnames at all
        currentnames_now <- currentnames
      } else {
        #  replace any exact match(es) to that one word. # should ideally confirm unique?
        currentnames_now <- gsub(paste0('^', bestfound, '$'), standardname, currentnames, ignore.case = ignore.case) # ok?
      }
    }
    return(currentnames_now)
  }
  ######################################## #
  
  currentnames_now <- currentnames
  
  for (i in seq_along(alias_list)) {
    standardname <- names(alias_list)[i]
    aliases <- alias_list[[i]]
    
    currentnames_now <- fixcolname1_infer(
      currentnames = currentnames_now,
      standardname = standardname,
      aliases = aliases,
      ignore.case = ignore.case
    )
  }
  if (verbose) {print(data.frame(currentnames = currentnames, currentnames_now = currentnames_now))}
  return(currentnames_now)
}
####################################################################### #
