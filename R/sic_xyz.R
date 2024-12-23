#' Use SIC code or industry title text search to see FRS Facility Registry Service data on those EPA-regulated sites
#'
#' @param sic_code_or_name passed to [sic_from_any()]
#' @param ... passed to [sic_from_any()]
#' @return relevant rows of the data.table called frs, which has column names that are
#'   "lat" "lon" "REGISTRY_ID" "PRIMARY_NAME" "NAICS" "SIC" "PGM_SYS_ACRNMS"
#'   
#'  The EPA also provides a [FRS Facility Industrial Classification Search tool](https://www.epa.gov/frs/frs-query#industrial)
#'  where you can find facilities based on NAICS or SIC.
#'  
#' @seealso [regid_from_sic()] [sic_from_any()] [latlon_from_sic()]
#' @export
#'
#' @examples
#'   frs_from_sic("glass")
#'   mapfast(frs_from_sic(sic_from_any("silver")$code))
#'   sic_from_any("silver")
#'   sic_from_name("silver")
#'   sic_from_any('0780')
#'   frs_from_sic('0780')
#'   regid_from_sic('0780')
#'   latlon_from_sic('0780')
#'
frs_from_sic <- function(sic_code_or_name, ...) {

  if (!exists("frs")) dataload_from_pins("frs")

  frs[REGISTRY_ID %in% regid_from_sic(sic_from_any(sic_code_or_name, ...)$code, id_only = TRUE) , ]
}
############################################################################## #


#' Find EPA-regulated facilities in FRS by SIC code (industrial category)
#'
#' @description Get lat lon, Registry ID, given SIC industry code(s)
#' Find all EPA Facility Registry Service (FRS) sites with this exact SIC code (not subcategories)
#' @details
#'   
#'  The EPA also provides a [FRS Facility Industrial Classification Search tool](https://www.epa.gov/frs/frs-query#industrial)
#'  where you can find facilities based on NAICS or SIC.
#' 
#'  NOTE: many FRS sites lack SIC code!
#'
#'   Also, this function does not find the sites
#'   identified by FRS data as being in a child SIC (subcategory of your exact query)!
#'
#'   Relies on  frs_by_sic (a data.table)
#'
#'   See info about SIC industry codes at <https://www.naics.com/search>
#' @param sic a vector of SIC codes, or
#'   a data.table with column named code, as with output of [EJAM::sic_from_any()]
#' @param id_only logical optional, set TRUE to get only the vector of REGISTRY_ID
#'   values back instead of a data.frame with lat,lon,SIC columns too. 
#' @return A data.table (not just data.frame) with columns called
#'   lat, lon, REGISTRY_ID, SIC (unless the id_only parameter is set to TRUE)
#' @aliases regid_from_sic
#' @examples
#'   regid_from_sic('7300')
#'   latlon_from_sic('7300')
#'   latlon_from_sic(sic_from_any("cheese")[,code] )
#'   head(latlon_from_sic(c('6150', '6300', '5995'), id_only=TRUE))
#'   # mapfast(frs_from_sic('6150')) # simple map
#'
#' @export
#'
latlon_from_sic <- function(sic, id_only=FALSE) {

  if (length(sic) != 0) {
    if (any(is.na(as.numeric(sic)))) {
      warning("SIC can not be coerced to a number.")
      return(NULL)
      }
  }
  if (missing(sic)) {return(NULL)}

  if (!exists("frs_by_sic")) dataload_from_pins("frs_by_sic")

  if (data.table::is.data.table(sic) & "code" %in% names(sic)) {sic <- sic$code} # flexible in case it was given output of EJAM::sic_from_any() which is a table not just code


  df <- frs_by_sic[SIC %in% sic]
  if (nrow(df) == 0) {warning("There are no sites with that SIC.")}
  if (id_only) {return(df$REGISTRY_ID)
  } else { return(df)}
}
############################################################################## #

#' Alias for latlon_from_sic()
#' @inheritParams latlon_from_sic
#' @return A data.table (not just data.frame) with columns called
#'   lat, lon, REGISTRY_ID, SIC (but see the id_only parameter)
#' @noRd
#' @export
#'
regid_from_sic <- latlon_from_sic
############################################################################## #



#' Find subcategories of the given overall SIC industry code(s)
#'
#' Given 3-digit SIC code, for example, get all SIC that start with those digits.
#' @details  similar idea was naics2children() but this is more robust
#' See [sic_from_any()] which uses this
#' @param mycodes SIC codes vector, of 2 to 4 digits each. See <https://siccode.com>
#'
#' @return a subset of the [sictable] data.table (not just the codes column)
#' @seealso [sic_subcodes_from_code()] [sic_from_code()]  [sic_from_name()]  [sic_from_any()]
#' @examples
#'   # codes starting with '07'
#'   sic_subcodes_from_code('07')
#'   # codes starting with '078'
#'   sic_subcodes_from_code('078')
#' @export
#'
sic_subcodes_from_code <- function(mycodes) {
  len <- nchar(mycodes)
  cnames  <- paste0("n", 1:4)
  results <- list()
  results[[1]] <- NULL
  for (digits in 2:4) {
    mycolname = cnames[digits]
    myvalues = unlist(as.vector(sictable[ , ..mycolname]))
    results[[digits]] <-  sictable[ myvalues %in% mycodes[len == digits] ,]
  }
  results <- data.table::rbindlist(results)
  return(results)
}
############################################################################## #


#' Search for industry names by SIC code(s), 4 digits each
#'
#' @param mycodes vector of character SIC codes. see <https://siccode.com>
#' @param children logical, if TRUE, also return all the subcategories - where SIC starts with the same digits
#' @seealso [sic_subcodes_from_code()] [sic_from_code()]  [sic_from_name()]
#'
#' @return a subset of the [sictable] data.table (not just the codes column)
#' @export
#'
sic_from_code <- function(mycodes, children=FALSE) {
  # find sictable data.table rows by exact matches on character SIC codes vector
  results <- NULL
  # results <- sictable[code %in% mycodes, ]
  results <- sictable[match(mycodes, sictable$code), ]
  if (children) {
    # add subcategories
    results <- sic_subcodes_from_code(results$code)
  }
  return(results)
}
############################################################################## #


#' Search for industry names and SIC codes by query string
#'
#' query by parts of words, etc. in the industry name.
#' @param mynames query string, vector of SIC industry names or any regular expression or partial words. See <https://siccode.com>
#' @param children logical, if TRUE, also return all the subcategories - where SIC starts with the same digits
#' @param ignore.case see [grepl()]
#' @param fixed should it be an exact match? see [grepl()]
#' @seealso [sic_subcodes_from_code()] [sic_from_code()]  [sic_from_name()]  [sic_from_any()]
#' @examples
#'  data.table::fintersect(sic_from_any( "glass"), sic_from_any("paint"))
#' @return a subset of the [sictable] data.table (not just the codes column)
#' @export
#'
sic_from_name <- function(mynames, children = FALSE, ignore.case = TRUE, fixed = FALSE) {
  # find sictable data.table rows by text search in SIC industry names via grepl()
  hits <- vector()
  results <- NULL
  for (i in 1:length(mynames)) {
    hits <- c(hits, which(grepl(mynames[i], sictable$name, ignore.case = ignore.case,  fixed = fixed)) )
  }
  results <- sictable[unique(hits),]
  if (children) {
    # add subcategories
    results <- sic_subcodes_from_code(results$code)
  }
  return(results)
}
############################################################################## #


#' General way to search for industry names and NAICS codes
#'
#' Find industry names and codes by searching for queried code(s) or text
#' @param query query string(s) and/or number(s), vector of NAICS codes or industry names or any regular expression or partial words
#' @param children logical, if TRUE, also return all the subcategories - where NAICS starts with the same digits
#' @param ignore.case see [grepl()]
#' @param fixed should it be an exact match? see [grepl()]
#' @param website_scrape whether to scrape info from the NAICS website to return a table of codes and names that match (web query uses synonyms so gets more hits)
#' @param website_url whether to return the URL of the webpage with info on the NAICS (web query uses synonyms so gets more hits)
#' @seealso [sic_subcodes_from_code()] [sic_from_code()]  [sic_from_name()]  [sic_from_any()]
#'
#' @return a subset of the [sictable] data.table (not just the codes column)
#' @export
#'
sic_from_any <- function(query, children=FALSE, ignore.case = TRUE, fixed = FALSE, website_scrape=FALSE, website_url=FALSE) {
  # find naicstable data.table rows by vector of text queries and/or numeric NAICS codes
  # returns subset of naicstable, not in any particular order and number of rows may be longer than number of query terms
  isnum <- suppressWarnings( !is.na(as.numeric(query)) )

  # if (website_url) {
  #   return(naics_url_of_query(query))
  # }
  # if (website_scrape) {
  #   return(naics_findwebscrape(query))
  # }

  query_codes <- query[isnum]
  if (length(query_codes) != 0) {
    via_codes <- sic_from_code(query_codes, children = children)
  } else {
    via_codes <- NULL
  }
  query_text <- query[!isnum]
  if (length(query_text) != 0) {
    via_text  <- sic_from_name(query_text,  children = children, ignore.case = ignore.case, fixed = fixed)
  } else {
    via_text <- NULL
  }
  results <- data.table::rbindlist(list(via_codes, via_text))
  if (children) {
    # add subcategories
    # takes subcategories only if the results return something
    # results <- sic_subcodes_from_code(results$code)
    # run on query_codes instead and join with results
    results_subcat <- sic_subcodes_from_code(query_codes)
    results <- results %>% full_join(results_subcat)
  }
  return(results)
}
############################################################################## #

