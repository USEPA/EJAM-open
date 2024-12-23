#' @name frs_by_mact
#' @title frs_by_mact (DATA) MACT NESHAP subpart(s) that each EPA-regulated site is subject to
#' @description 
#'    This is a data.table with one row per site -- MACT subpart pair, 
#'    so it has multiple rows for one site if the site is covered by multiple subparts. 
#'    It has been joined with frs_by_programid to get latlons for matching facilities.
#'  @details  
#'  
#'   This file is not stored in the package, but is obtained via [dataload_from_pins()].
#'
#'  There are about 115k rows here but only about 86k unique program IDs in this table, 
#'  which is from the ECHO data download of ICIS Air and AFS. 
#'  The programid column here should be found in the pgm_sys_id column in frs_by_programid, 
#'  but as of mid 2024 only 55k of them were found there. 
#'  ```
#'   table(frs_by_mact$programid %in% frs_by_programid$pgm_sys_id)
#' FALSE  TRUE 
#' 59944 55429 
#'   ```
#'   Also note we have found some typos in downloaded data from ECHO/FRS, such as 
#'   
#'   "WOOD PERSERVING AREA SOURCES" instead of "WOOD PRESERVING AREA SOURCES"
#'   
#' @examples  
#'   mact_table
#'   mact_table[order(mact_table$title),]
#'   mycodes <- c("BBBBBB", "OOOO")
#'   frs_by_mact[subpart %in% mycodes, ]
#'   mact_table[grepl("smelt", mact_table$title, ignore.case = T), ]
#'   frs_by_mact[grepl("smelt", title, ignore.case = T), ]
#'   # a single site can be covered by 19 categories
#'   frs_by_mact[, howmany := .N, by="programid"][order(howmany), ] 
#'   table(frs_by_mact[, howmany := .N, by="programid"][order(howmany), howmany])
#'   
#' @seealso [dataload_from_pins()] [mact_table] [frs_by_programid]  [frs]
NULL
