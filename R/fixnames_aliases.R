

#' Convert terms to standardized terms based on synonyms
#'
#' @param x vector of terms, such as colnames(testpoints_10), etc.
#' @param na_if_no_match optional, set to TRUE if you want it to return NA
#'   for each element of x not found in the alias_list info
#' @param alias_list built-in already in source code (but can replace
#'   using this optional parameter), a list of named vectors where
#'   names are standard, preferred, canonical versions of terms, and
#'   each vector is a set of aliases for that term.
#' @param ignore.case optional set to FALSE if you want to not ignore case
#'
#' @return character vector like x but where some or all may be replaced by
#'   standardized versions of the elements of x, or NA if appropriate
#' @seealso [fixcolnames_infer()] [latlon_infer()]
#' @details [fixcolnames_infer()] and [fixnames_aliases()] are very similar.
#' 
#'   - [fixcolnames_infer()] is designed to figure out for a data.frame
#'   which one column is the best guess (top pick) for which should be 
#'   used as the "lat" column, for example,
#'   so when several colnames are matches based on the alias_list,
#'   this function picks only one of them to rename to the preferred or
#'   canonical name, leaving others as-is.
#'   
#'   - In contrast to that, [fixnames_aliases()] is more general and
#'   every input element that can be matched with a
#'   canonical name gets changed to that preferred version, so
#'   even if multiple input names are different aliases of "lat",
#'   for example, they all get changed to "lat."
#'
#' The alias_list could be for example this:
#' 
#' \preformatted{
#'  alias_list <- list(
#'   sqkm = c('km2', 'kilometer2','kilometers2', 'sq kilometers', 'sq kilometer',
#'    'sqkilometers', 'sqkilometer',  'squarekilometers', 'squarekilometer',
#'    'square kilometers', 'square kilometer'),
#'   sqm = c('m2', 'meter2','meters2', 'sq meters', 'sq meter','sqmeters', 'sqmeter',
#'   'squaremeters', 'squaremeter', 'square meters', 'square meter'),
#'   mi = c('mile', 'miles'),
#'   
#'   lat = lat_alias,
#'   #[1]"lat" "latitude83" "latitude" "latitudes"  "faclat" "lats" "y" 
#'   lon = lon_alias, 
#'   #[1]"lon" "longitude83" "longitude" "longitudes" "faclong" "lons" "long" "longs" "lng" "x"
#'   
#' )
#' }
#' @examples
#' fixnames_aliases(c("km", "kilometer", "miles", "statename", 'X', "y"))
#' fixnames_aliases("LATITUDE")
#' fixnames_aliases("LATITUDE", ignore.case = F)
#' fixnames_aliases("LATITUDE", na_if_no_match = T)
#' fixnames_aliases("LATITUDE", na_if_no_match = T, ignore.case = F)
#' fixnames_aliases(c(NA, 1, "typo", 1:2, list()))
#' 
#' fixnames_aliases(c(1:4, "na", "tbd"), 
#'   alias_list = list(upto1 = 0:1, company = 2, crowd = 3:10, other = c("na", "tbd")))
#' 
#' @export
#'
fixnames_aliases <- function(x, na_if_no_match = FALSE, alias_list = NULL, ignore.case = TRUE) {
  
  if (any(!is.atomic(x))) {stop("x must be an atomic vector")}
  ######################################## #
  # synonyms
  if (is.null(alias_list)) {
    alias_list <- list(
      # do these need to and is it ok to have the names like sqkm and km etc. also within each list? ***
      sqkm = c('km2', 'kilometer2','kilometers2', 'sq kilometers', 'sq kilometer','sqkilometers', 'sqkilometer',  'squarekilometers', 'squarekilometer', 'square kilometers', 'square kilometer'),
      sqm = c('m2', 'meter2','meters2', 'sq meters', 'sq meter','sqmeters', 'sqmeter',  'squaremeters', 'squaremeter', 'square meters', 'square meter'),
      sqcm = 'cm2',    
      sqmm = 'mm2',
      km = c('kilometer', 'kilometers'),
      m = c('meter', 'meters'),
      cm = c('centimeter', 'centimeters'),
      mm = c('millimeter', 'millimeters'),
      sqmi = c('mi2', 'mile2','miles2', 'sq miles', 'sq mile','sqmiles', 'sqmile',  'squaremiles', 'squaremile', 'square miles', 'square mile'), 
      sqyd = c('yd2'),
      sqft = c('ft2'),
      sqin = c('in2'),
      mi = c('mile', 'miles'),
      yd = c('yard', 'yards'),
      ft = c('foot', 'feet'),
      `in` = c('inch', 'inches'),  # using "in" is not ideal as it is a reserved word
      
      # used in address_from_table() etc.
      address = c("address"),
      street = c("street", "street address", "address1", "address 1"),
      city = c("city", "cityname", "city name"),
      state = c("state", "mystate", "statename", "ST"),
      zip = c("zip", "zipcode", "zip code"), 
      
      # used in latlon_infer() etc.
      lat = lat_alias, #"lat" "latitude83" "latitude"   "latitudes"  "faclat"  "lats" "y"
      lon = lon_alias, #"lon" "longitude83" "longitude" "longitudes" "faclong" "lons" "long" "longs" "lng" "x"
      
      # could be used by fips_from_table() etc.  NOTE THESE ARE NOT REALLY IDENTICAL, but fipstype() can differentiate them
      fips = c('FIPS',   'Fips', 'fips_code', 'fipscode',
               'blockfips', 
               'bgfips', 'blockgroupfips', 'blockgroup_fips', 'blockgroup_fips_code',
               'FIPS.TRACT', 'tractfips', 'tract_fips',
               'countyfips', 'FIPS.COUNTY',
               'statefips', 'ST_FIPS','st_fips','ST_FIPS','st_fips', 'FIPS.ST'
         ),
      
      # had been used in fixcolnames("pctlowinc", "rname", "long") etc. but "friendly" is ambiguous and should be phased out
      rname = c("r", "friendly"),
      
      longname = c(
        # "long",  # *** problem! we already interpret "long" as "lon" (longitude)
        "longname", "longnames", "full", "description", "header"),
      
      shortlabel = c("short", "shortname", "shortnames", "shortlabel", "shortlabels", "labels", "label"),
      apiname = c('api', 'apiname'),
      csvname = c("csv", "csvname"),
      acsname = c('acs', 'acsname'),
      oldname = c("old", "oldnames", "oldname", 'original') 
       
    )
  } else {
    stopifnot(is.list(alias_list), all(sapply(alias_list, is.atomic)), all(sapply(alias_list, length) > 0))
  }
  ######################################## #
  # fix a single value only, x1: 
  fixnames_aliases1 = function(x1, na_if_no_match = FALSE, alias_list = NULL, ignore.case = TRUE) {
    if (is.null(alias_list)) {stop("requires alias_list")}
    if (length(x1) != 1) {stop("x1 must be length 1")}
    if (ignore.case) {
      fixed_or_na <- names(which(sapply(alias_list, function(z) tolower(x1) %in% tolower(z))))
    } else {
      fixed_or_na <- names(which(sapply(alias_list, function(z) x1 %in% z)))
    }
    
    if (length(fixed_or_na) == 0) {
      if (na_if_no_match) {
        fixed_or_na <- NA
      } else {
        fixed_or_na <- x1
      }
    }
    x1 <- fixed_or_na
    return(x1)
  }
  ######################################## #
  
  canonical <- x %in% names(alias_list)
  x[!canonical] <- sapply(x[!canonical], 
                          fixnames_aliases1, 
                          na_if_no_match = na_if_no_match, 
                          alias_list = alias_list,
                          ignore.case = ignore.case)
  x <- unlist(x) # fixes problem!! where if x was already the canonical name(s) and no non-canonical ones provided, it would return a list
  return(x)  
}
###################################################################### # 
