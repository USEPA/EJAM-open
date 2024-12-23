
#' helper function to rename variables that are colnames of data.frame
#' 
#' Changes variable names like colnames to long plain-English headers or short labels for plots
#' 
#' @details You specify an alias of a type like "api", "r", "long", or "short",
#'   or one of `colnames(map_headernames)` like "rname", "vartype", "decimals", "varlist", etc.
#'   
#'   Also, you can use this to extract any info from `map_headernames` (which 
#'   here is called mapping_for_names).
#'   
#'   NOTE: if you ask to rename your words to a known type like rname or apiname, and 
#'   the namesnow is not found among the oldtype, then it is not renamed, and those are returned as unchanged.
#'   BUT, if you specify as newtype some column that is not a known type of name, like "varcategory" 
#'   then it will instead return an empty string for those in namesnow that are not found among the oldtype.
#'   That way if you are really seeking a new name, but it cannot rename, it keeps the old name
#'   while if you are really seeking metadata like what category it is in, 
#'   it returns a blank if the old name is not found at all.
#'  
#'   These are some key column names in the [map_headernames] table:
#'    
#'   - "shortname" (aka "short", for plot labels, etc.)
#'   
#'   - "longname" (aka "long", for full explanatory headers to use on a table)
#'   
#'   - "rname" (aka "r", the R variable names as used in the EJAM code)
#'   
#'   - "apiname" (aka "api", as returned by EJScreen API)
#'   
#'   - "csvname" (aka "csv", as found in the CSV files of just the key demographic and environmental indicators, found on the EJScreen FTP site)
#'   
#'   - "acsname" (aka "acs", as found in a ACS data file internally used by EJScreen, containing all the extra demographic and other indicators not stored in the CSV files on the EJScreen FTP site)
#'   
#'   - "DEJ" (whether the indicator is demographic, environmental, etc.)
#'   
#'   - "varlist" (which group of names is this variable in, such as "names_d", "names_d_subgroups", "names_d_state_pctile", etc.)
#'   
#'   - "calculation_type" (how it should be aggregated over block groups, such as "wtdmean", "sum of counts", etc.)
#'   
#'   - "denominator" (the weight to use in aggregating as a wtdmean, normally a count variable that is the universe for a percentage, such as "pop", "hhlds", etc.)
#'   
#' @param namesnow vector of colnames to be renamed
#' @param oldtype "longname" or "shortname", or "csv" or "r" or "api", etc.
#'   or a colname of map_headernames, used if one of those known types was not specified.
#' @param newtype "longname" or "shortname", or "csv" or "r" or "api", etc.
#'   or a colname of map_headernames, used if one of those known types was not specified.
#' @param mapping_for_names default is a dataset already in the package.
#' 
#' @seealso [varinfo()]
#' 
#' @return Vector or new column names same length as input
#' @examples  # see package tests
#'   
#'  names_d
#'  namesbyvarlist('names_d')
#'  x = varinfo("pctlowinc")
#'  x = varinfo("pcthisp")
#'  
#'
#'  # see the different names for the same variable, and see it is not in the csv tables on the FTP site
#'  varinfo("pcthisp", c("csvname", "acsname", "apiname"))
#'
#'  # EJAM:::names_whichlist("RAW_D_INCOME")
#'  fixcolnames(c("RAW_D_INCOME", "S_D_LIFEEXP"), 'api')
#'  fixcolnames('LOWINCPCT', 'csv')
#'  fixcolnames(c("PCT_HISP", "HISP"), 'acs')
#'  fixcolnames(c("RAW_D_INCOME", "S_D_LIFEEXP"), newtype = "longname")
#'
#'  addmargins(table(map_headernames$vartype, map_headernames$DEJ))
#'   
#'   # the columns "newsort" and "reportsort" provide useful sort orders
#'   x <- map_headernames$rname[map_headernames$varlist == "names_d"]
#'   # same as 
#'   
#'   print("original order"); print(x) 
#'   x <-  sample(x, length(x), replace = FALSE)   
#'   print("out of order"); print(x)
#'   print("fixed order")
#'   x[ order(fixcolnames(x, oldtype = "r", newtype = "newsort")) ]
#'
#' @export
#' 
fixcolnames <-  function(namesnow, oldtype='csvname', newtype='r', mapping_for_names) {
  
  if (missing(mapping_for_names)) {
    if (exists('map_headernames')) {
      mapping_for_names <- map_headernames
    } else {
      warning('Cannot rename. Returning unchanged names. Must specify valid mapping_for_names in function or map_headernames must be in global env')
      return(namesnow)
    }
  }
  
  fromcolname <- fixmapheadernamescolname(oldtype)
  tocolname   <- fixmapheadernamescolname(newtype)
  
  if (!(tocolname %in% colnames(mapping_for_names)))   {
    warning(paste('returning unchanged names because mapping_for_names has no column called ', tocolname))
    return(namesnow)
  }
  if (!(fromcolname %in% colnames(mapping_for_names))) {
    warning(paste('returning unchanged names because mapping_for_names has no column called ', fromcolname))
    return(namesnow)
  }
  
  oldnames <- mapping_for_names[, fromcolname]
  newnames <- mapping_for_names[, tocolname]
  newnames <- newnames[oldnames %in% namesnow]
  oldnames <- oldnames[oldnames %in% namesnow]
  
  # if no match found in oldnames, we will just return the name unfixed/unchanged,
  # if they asked for renaming to a name type like api, csv, original, etc.
  # BUT, if they asked to rename to some other column such as varlist or vartype, 
  #  then for the cases where there is no match found among oldnames, we should return NA or empty string
  # rather than returning the name unchanged, since it is confusing to ask for varlist and get back the input term.
  
  nametypes <- sort(c(
    # canonical terms
    names(eval(formals(fixmapheadernamescolname)$alias_list)), 
    # synonyms for those
    as.vector(unlist(eval(formals(fixmapheadernamescolname)$alias_list)))
  ))
  
  if (!(newtype %in% nametypes)) {
    namesnow[!(namesnow %in% oldnames)] <- ''
    # so that when there is no match below, it is left like this and is returned as ''
  }
  
  namesnow[match(oldnames, namesnow)] <- newnames
  
  return(namesnow)
}
