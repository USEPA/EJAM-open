
#' Find EPA-regulated facilities in FRS by NAICS code (industrial category)
#' @description Get lat lon, Registry ID, given NAICS industry code(s)
#' Find all EPA Facility Registry Service (FRS) sites with this/these NAICS code(s)
#' @details 
#'  Important notes:
#'  
#'  * Finding the right NAICS and finding all the right sites by NAICS is complicated,
#'  and requires understanding the NAICS codes system, the FRS data, and the EJAM functions. 
#'  See the discussion in the "Advanced" or other vignettes/articles. 
#'  
#'  * Many FRS sites lack NAICS code!
#'
#'  * Note the difference between children = TRUE and children = FALSE
#'  
#'  * The NAICS in the returned table may be a child NAICS not the NAICS used in the query! This may cause confusion if you are querying multiple parent NAICS and you want to analyze results by NAICS!
#'   
#'   The functions like [regid_from_naics()], [latlon_from_naics()], and [frs_from_naics()]
#'   try to find EPA FRS sites based on naics codes or titles.
#'   
#'  EPA also provides a [FRS Facility Industrial Classification Search tool](https://www.epa.gov/frs/frs-query#industrial)
#'  where you can find facilities based on NAICS or SIC.
#'
#'  See more about NAICS industry codes at <https://www.naics.com/search>
#'   
#' @param naics a vector of naics codes or query of titles of NAICS, or
#'   a data.table with column named code, as with output of [naics_from_any()]
#' @param id_only optional logical. Must set TRUE to get only regid instead of table
#' @param children optional logical. set to FALSE to get only exact matches
#'   rather than all facilities whose NAICS
#'   starts with provided naics (or naics based on provided title).
#'   Many facilities have only a longer more specific NAICS code
#'   listed in the FRS, such as a 6-digit code, so if the category (e.g., 4-digit)
#'   is queried then without children = TRUE one would not find all the sites
#'   within that overall category.
#'   
#' @param ... passed to [naics_from_any()]
#' @return A data.table (not just data.frame) with columns called
#'   lat, lon, REGISTRY_ID, NAICS, naics_found, naics_query (unless id_only parameter set TRUE).
#'   naics_query is the input parameter that was used (that had been provided to this function as naics).
#'   naics_found and NAICS are identical (redundant), and are the code found that
#'   was listed in the [frs_by_naics] table, so it might be a subcategory (child)
#'   of the naics_query term. For example, naics_query might be 33611 (5 digits)
#'   and for one facility the NAICS and naics_found might be 336111 (a 6-digit code)
#'   and for another facility they might be 336112.
#'   
#' @seealso [frs_from_naics()]  [frs_from_sic()] [latlon_from_sic()] [regid_from_naics()] [naics_from_any()]
#'   
#' @examples
#' \dontrun{
#'   regid_from_naics(321114)
#'   latlon_from_naics(321114)
#'   # latlon_from_naics(naics_from_any("cheese")[,code] )
#'   latlon_from_naics("cheese")
#'   head(latlon_from_naics(c(3366, 33661, 336611), id_only=TRUE))
#'   head(regid_from_naics(c(3366, 33661, 336611))
#'   head(regid_from_naics(3366, children = TRUE))
#'   # mapfast(frs_from_naics(336611)) # simple map
#'   
#'   # get name from one code
#'   naics_from_code(336)$name
#'   # get the name from each code
#'   mycode = c(33611, 336111, 336112)
#'   naics_from_code(mycode)$name
#'   # see counts of facilities by code (parent) and subcategories (children)
#'   naics_counts[NAICS %in% mycode, ]
#'   # see parent codes that contain each code
#'   naicstable[code %in% mycode, ]
#'   
#'   # how many were found via each naics code?
#'   found = latlon_from_naics(c(211,331))
#'   x = table( found$naics_found, found$naics_query)
#'   x = x[order(x[, 1],decreasing = T),]
#'   x
#'   }
#' @export
#'
latlon_from_naics <- function(naics, children = TRUE, id_only = FALSE, ...) {
  
  if (missing(naics)) {return(NULL)} else if (all(is.na(naics)) | is.null(naics)) {return(NULL)}
  
  if (data.table::is.data.table(naics) & "code" %in% names(naics)) {naics <- naics$code} # flexible in case it was given output of EJAM::naics_from_any() which is a table not just code
  
  if (!exists("frs_by_naics")) dataload_from_pins("frs_by_naics")
  
  naics_queries <- naics
  results <- list()
  
  for (i in 1:length(naics_queries)) {
    
    naics_found <- naics_from_any(query = naics_queries[i], children = children, ...)$code 
    # children = TRUE would get more than just exact matches to the provided specific number of digits NAICS
    
    if (id_only) {
      results[[i]] <- frs_by_naics[NAICS %in% naics_found, REGISTRY_ID] # a vector
      # Just a vector, results$REGISTRY_ID 
      # lacks info on query vs children found, 
      # but changes could break code that assumes this provides a vector vs a data.table
      ### if more than a vector were returned:
      # results[[i]] <- cbind(
      #   naics_query = naics_query[i], 
      #   naics_found = naics_found, 
      #   frs_by_naics[NAICS %in% naics_found, REGISTRY_ID]
      # )
      
    } else {
      results[[i]] <- cbind(
        frs_by_naics[NAICS %in% naics_found, ],
        naics_query = naics_queries[i]
      )
      results[[i]]$naics_found <- results[[i]]$NAICS # other code expected col called NAICS, so this retains that and makes it very clear it means found not query, though redundant
      setcolorder(results[[i]], "naics_query", after = NCOL(results[[i]]))
      # return(frs_by_naics[REGISTRY_ID %in% regid_from_naics(naics), ])
    }
  }
  if (id_only) {
    results <- unlist(results)
  } else {
    results <- rbindlist(results)
  }
  return(results)
}
########################################## #


#' Find registry ids of EPA-regulated facilities in FRS by NAICS code (industrial category)
#' Like latlon_from_naics() but returns only regid
#' @param naics a vector of naics codes, or
#'   a data.table with column named code, as with output of [naics_from_any()]
#' @param children optional logical. Must set to TRUE to get facilities whose NAICS
#'   starts with provided naics (or naics based on provided title) rather than
#'   only exact matches. Many facilities have only a longer more specific NAICS code
#'   listed in the FRS, such as a 6-digit code, so if the category (e.g., 4-digit)
#'   is queried then children = TRUE has to be specified to find all the sites
#'   within that overall category.
#' @param id_only optional, only for backward compatibility
#' @param ... passed to [naics_from_any()]
#' @seealso [latlon_from_naics()]
#' @details Finding the right NAICS/SIC and finding all the right 
#'   sites is complicated. See discussion of [latlon_from_naics()].
#' @return vector of registry ID values of facilities in EPA FRS
#'   that are listed there as being in this/these NAICS,
#'   like [latlon_from_naics()] but with id_only = TRUE
#' 
#' @export
#'
regid_from_naics <- function(naics, children = FALSE, id_only = TRUE, ...) {
  
  latlon_from_naics(naics = naics, children = children, id_only = id_only, ...)
}
######################################## #
