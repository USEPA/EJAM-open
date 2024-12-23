
#' Get point locations for US EPA-regulated facilities that have 
#' sources subject to Maximum Achievable Control Technology (MACT) standards
#' under the Clean Air Act.
#' 
#' @details See [EPA information about MACT subparts, NESHAPs, etc.](https://www.epa.gov/stationary-sources-air-pollution/national-emission-standards-hazardous-air-pollutants-neshap-9)
#' 
#'  EPA's [ECHO query tools](https://echo.epa.gov/help/facility-search/search-criteria-help#facchar)
#'  also provide search by NAICS or SIC, and by MACT subpart.
#'
#' @param subpart vector of one or more strings indicating the 
#'   Subpart of CFR Title 40 Part 63 that covers the source category 
#'   of interest, such as "FFFF" - see for example, 
#'   <https://www.ecfr.gov/current/title-40/part-63/subpart-FFFF>
#' @param include_if_no_latlon logical - many in the database lack lat lon values but have a MACT code
#' @return a data.table with columns named 
#' 
#'   programid, subpart, title, lat, lon, REGISTRY_ID, program 
#' 
#'   for US EPA FRS sites with that MACT code.
#'   Or NA if none found. 
#' @examples 
#'   mact_table
#'   latlon_from_mactsubpart("OOOO", include_if_no_latlon = FALSE) # default
#'   latlon_from_mactsubpart("OOOO", include_if_no_latlon = TRUE)
#'   
#' @export
#'
latlon_from_mactsubpart <- function(subpart = "JJJ", include_if_no_latlon = FALSE) {
  
  # https://www.ecfr.gov/reader-aids/ecfr-developer-resources
  #  https://www.ecfr.gov/current/title-40/part-63/subpart-FFFF
  #  https://www.ecfr.gov/current/title-40/chapter-I/subchapter-C/part-63/subpart-FFFF
  # subparts here focus on various source categories, from F onwards.
  # https://github.com/usgpo/bulk-data/blob/master/ECFR-XML-User-Guide.md
  
  # see e.g.,  https://www.govinfo.gov/content/pkg/FR-2012-09-19/pdf/2012-20642.pdf#page=1 
  #   which showed NAICS 332813 (and 3311 and 3312) for MACT subpart N (and subpart C) 
  # as being MACT codes 1607, 1610, 1615 (and 0310).
  # Chromium Electroplating NESHAP, Subpart N  
  #   Chromium Anodizing Tanks .........................................332813 1607
  #   Decorative Chromium Electroplating ............................   332813 1610
  #   Hard Chromium Electroplating .....................................332813 1615
  # Steel Pickling—HCl Process Facilities And Hydrochloric Acid Regeneration Plants NESHAP, Subpart CCC ..... 3311, 3312  0310
  
  # stop("latlon_from_mactsubpart() is not yet available")
  
  if (!exists("frs_by_mact")) dataload_from_pins("frs_by_mact")
  query <- subpart
  
  if (missing(subpart) || !is.atomic(query) || class(query) !=  "character") {
    warning("subpart must be a character vector of 1 or more codes like 'AAAA' ")
    ## now allow more than 1 code in query
  # if (missing(subpart) || !is.atomic(query) || length(query) != 1 || class(query) !=  "character") {
  #   warning("subpart must be a single character string like 'AAAA' ")
    
    query <- ""
    }
  query <- toupper(query)
  mact_out = frs_by_mact[subpart %in% query, ] # was using == which did not work for multiple codes in query.
  #   now use %in% not == even though groups are already large so not always a great idea to allow multiple subparts
  # mact_out <- frs_by_mact[match(query, subpart), ] 
  if (!include_if_no_latlon) {
    mact_out = mact_out[!is.na(lat) & !is.na(lon), ]
  }
  if (NROW(mact_out) == 0) {return(NA)} else {return(mact_out)}
}
