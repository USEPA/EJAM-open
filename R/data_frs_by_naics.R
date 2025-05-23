#' @name frs_by_naics
#' @title frs_by_naics (DATA) data.table of NAICS code(s) for each EPA-regulated site in Facility Registry Service
#' @seealso [frs] [frs_from_naics()] [naics_categories()] [frs_by_programid] and see naics_from_any in EJAM pkg.
#' @description 
#'    This is the format with one row per site-NAICS pair, 
#'    so multiple rows for one site if it is in multiple NAICS.
#'  @details  
#'   This file is not stored in the package, but is obtained via [dataload_dynamic()].
#'   
#'  The EPA also provides a [FRS Facility Industrial Classification Search tool](https://www.epa.gov/frs/frs-query#industrial)
#'  where you can find facilities based on NAICS or SIC.
#'  
#'  
#'  MOST SITES LACK NAICS INFO IN FRS! NAICS is missing for about 80 percent of these facilities.
#'   
#'   frs here had about 2.5 million unique REGISTRY_ID values, but 
#'   
#'    frs_by_naics had only about 700k rows
#'    
#'    about 581,000 unique REGISTRY_ID values with
#'    
#'    about 1,858 unique NAICS codes.
#' 
#'   length(unique(frs_by_naics$REGISTRY_ID))
#'   
#'   `length(unique(frs_by_naics[,REGISTRY_ID]))`
#'   
#'   `length(frs_by_naics[, unique(REGISTRY_ID)])`
#'   
#'   `frs_by_naics[,uniqueN(REGISTRY_ID)]`
#'   
#'        e.g., 581,416 in 02/2025
#' 
#'         lat       lon  REGISTRY_ID  NAICS
#'         
#' 1: 34.04722 -81.15136 110000854246 325211
#' 
#' 2: 34.04722 -81.15136 110000854246 325220
#' 
#' 3: 34.04722 -81.15136 110000854246 325222
#' @examples
#'  # NAICS is missing for almost 80 percent of facilities
#'  `frs[ NAICS == "", .N] / frs[,.N] `
#'  # only about 647k facilities had some NAICS info as of 02/2025
#'  `frs[ NAICS != "", .N]`
#'  `frs_by_naics[, uniqueN(REGISTRY_ID)]` # about 581k as of 02/2025
#'  
#'  dim(frs_by_naics) 
#'  # about 680k rows here, or pairs of 1 NAICS - 1 registry ID pair,
#'  #  since some IDs have 2 or more NAICS so appear as 2 or more rows here.
#'  
#'  # About 1858 different NAICS codes appear here:
#'  `frs_by_naics[,  uniqueN(NAICS)]`
#'  `frs_by_naics[, .(sum(.N > 1)), by=NAICS][,sum(V1)]`
#'    #  1,699 NAICS codes are used to describe more than one Registry ID
#'   `frs_by_naics[, .(sum(.N == 1)), by=NAICS][,sum(V1)]`
#'    #  almost 200 NAICS codes appear only once, i.e., apply to only a single facility 
#'    
#'  # Which 2-digit NAICS are found here most often?
#'  `frs_by_naics[ , .N, keyby=substr(NAICS,1,2)]`
#'  `frs_by_naics[ , .N,   by=substr(NAICS,1,2)][order(N),]` # Most common are 33 and 81
#'  # Top 10 most common 3-digit NAICS here:
#'  `x = tail(frs_by_naics[ , .N,   by=.(n3 = substr(NAICS,1,3))][order(N), ],10)`
#'  `cbind(x, industry = rownames(naics_categories(3))[match(x$n3, naics_categories(3))])`
NULL
