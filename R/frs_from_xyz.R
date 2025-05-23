
#' Use registry ID to see FRS Facility Registry Service data on those EPA-regulated sites
#' 
#' @param regid vector of one or more EPA Registry ID codes used by FRS
#' @return relevant rows of the data.table called frs, which has column names that are
#'   "lat" "lon" "REGISTRY_ID" "PRIMARY_NAME" "NAICS" "PGM_SYS_ACRNMS"
#'
#' @examples 
#'   frs_from_regid(testinput_registry_id)
#'   frs_from_regid(110000307695)
#'   frs_from_regid("110000307695")
#'
#' @export
#' 
frs_from_regid <- function(regid) {
  
  if (!exists("frs")) dataload_dynamic("frs")
  frs[match(regid, frs$REGISTRY_ID, nomatch=0), ] # to return results in same order as search terms were provided
  # frs[REGISTRY_ID %in% regid, ]
}
########################################## # 

# got rid of frs_from_  site  id  () since the name was confusing as a site id might be 1:n or whatever

########################################## # 


#' Use EPA Program ID to see FRS Facility Registry Service data on those EPA-regulated sites
#' 
#' @inheritParams latlon_from_programid
#' @return relevant rows of the data.table called frs, which has column names that are
#'    "lat" "lon" "REGISTRY_ID" "PRIMARY_NAME" "NAICS" "PGM_SYS_ACRNMS"
#' @examples
#'  test <- data.frame(programname = c('STATE','FIS','FIS'),
#'                     programid = c('#5005','0-0000-01097','0-0000-01103'))
#'  x = frs_from_programid(test$programname, test$programid)
#'  x
#'  mapfast(x)
#' 
#' @export
#' 
frs_from_programid <- function(programname, programid) {
  
  if (!exists("frs")) dataload_dynamic("frs")
  regid <- latlon_from_programid(programname,programid)$REGISTRY_ID
  frs[match(regid, REGISTRY_ID), ] # try to return results in same order as search terms were provided
}
########################################## # 


#' Use EPA Program acronym like TRIS to see FRS Facility Registry Service data on those EPA-regulated sites
#' 
#' @description Get data.table based on given FRS Program System CATEGORY.
#'   Find all FRS sites in a program like RCRAINFO, TRIS, or others.
#' @param program vector of one or more EPA Program names used by FRS 
#' @return relevant rows of the data.table called frs, which has column names that are
#'   "lat" "lon" "REGISTRY_ID" "PRIMARY_NAME" "NAICS" "PGM_SYS_ACRNMS"
#' @details Also see [EPA documentation describing each program code](https://www.epa.gov/frs/frs-data-sources) aka data source.
#'     
#' @export
#' 
frs_from_program  <- function(program) {
  
  if (!exists("frs_arrow")) {
    dataload_dynamic("frs", return_data_table = FALSE)
  }
  #  #  return results in any order since we are getting an entire program, not a list of facilities in some specified order
  regid <- latlon_from_program(program)$REGISTRY_ID
  
  res <- frs_arrow %>% 
    filter(.data$REGISTRY_ID %in% regid) %>% 
    collect() %>% 
    data.table::as.data.table()
  
  return(res)
}
########################################## # 


#' Use NAICS code or industry title text search to see FRS Facility Registry Service data on those EPA-regulated sites
#'
#' @param naics_code_or_name passed to [naics_from_any()] as the query
#' @param childrenForNAICS passed to [naics_from_any()] as the children param of that function
#' @param ... passed to [naics_from_any()]
#' @return relevant rows of the data.table called frs, which has column names that are
#'   "lat" "lon" "REGISTRY_ID" "PRIMARY_NAME" "NAICS" "PGM_SYS_ACRNMS"
#'   
#' @seealso [latlon_from_naics()] [latlon_from_sic()] [frs_from_sic()] [regid_from_naics()] [naics_from_any()]
#' 
#' @details  The EPA also provides a [FRS Facility Industrial Classification Search tool](https://www.epa.gov/frs/frs-query#industrial)
#'  where you can find facilities based on NAICS or SIC.
#'  
#'  EPA's [ECHO query tools](https://echo.epa.gov/help/facility-search/search-criteria-help#facchar)
#'  also provide search by NAICS or SIC, and by MACT subpart.
#'  
#' @examples 
#'   frs_from_naics("uranium")
#'   mapfast(frs_from_naics(naics_from_any("nuclear")$code))
#'   naics_from_any("silver")
#'   naics_from_name("silver")
#'   naics_from_any(212222 )
#'   frs_from_naics(21222)
#'   regid_from_naics(21222)
#'   latlon_from_naics(21222)
#'   
#' @export
#'
frs_from_naics <- function(naics_code_or_name, childrenForNAICS = TRUE, ...) {
  
  if (!exists("frs_arrow")) {
    dataload_dynamic("frs", return_data_table = FALSE)
  }
  #  return results in any order since we are getting an entire NAICS, not a list of facilities in some specified order
  res <- frs_arrow %>% 
    filter(.data$REGISTRY_ID %in% regid_from_naics(naics_from_any(naics_code_or_name, children = childrenForNAICS,...)$code,
                                             children = FALSE, id_only = TRUE)) %>% 
    collect() %>% 
    data.table::as.data.table()
  return(res)
}
########################################## # 


#' Use site name text search to see FRS Facility Registry Service data on those EPA-regulated sites
#' 
#' VERY SLOW search within PRIMARY_NAME of facilities for matching text
#' @param sitenames one or more strings in a vector, which can be regular expressions or query for exact match using fixed=TRUE
#' @param ignore.case logical, search is not case sensitive by default (unlike [grepl()] default)
#' @param fixed see [grepl()], if set to TRUE it looks for only exact matches
#'
#' @return relevant rows of the data.table called frs, which has column names that are
#'   "lat" "lon" "REGISTRY_ID" "PRIMARY_NAME" "NAICS" "PGM_SYS_ACRNMS"
#'
#' @examples \donttest{
#'  # very slow
#'  x=frs_from_sitename
#'  nrow(x)
#'  head(x)
#' }
#'   
#' @export
#'
frs_from_sitename <- function(sitenames, ignore.case=TRUE, fixed=FALSE) {
  
  if (!exists("frs")) dataload_dynamic("frs")
  
  results <- list()
  for (i in 1:length(sitenames)) {
    # VERY SLOW WAY:
    results[[i]] <- frs[grepl(sitenames[i], PRIMARY_NAME, ignore.case = ignore.case, fixed = fixed), ]
  }
  results  <- unique(data.table::rbindlist(results))
  return(results)
}
########################################## # 
