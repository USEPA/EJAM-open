#' @name frs_by_programid
#' @title frs_by_programid (DATA) data.table of Program System ID code(s) for each EPA-regulated site in 
#'   the Facility Registry Service
#' @seealso [frs] [frs_by_naics]  
#' @details 
#'   This file is not stored in the package, but is obtained via [dataload_from_pins()].
#'  
#'  Created by frs_make_programid_lookup()  that was in EJAMfrsdata package
#'  
#'    This is the format with one row per site-programid pair, 
#'    so multiple rows for one site if it is in multiple programs.
#'    
#'  ```
#'  > dim(frs_by_programid)
#'  [1] 3440451       5   as of 1/3/2023
#'
#' nn=sample(1:nrow(frs_by_programid), 1); frs_by_programid[REGISTRY_ID == frs_by_programid$REGISTRY_ID[nn],]
#'
#'              lat       lon  REGISTRY_ID  program         pgm_sys_id
#'      1: 40.21262 -100.6464 110040499724 AIRS/AFS         3114500040
#'      2: 40.21262 -100.6464 110040499724     NDEQ              87933
#'      3: 40.21262 -100.6464 110040499724      AIR NE0000003114500040
#'      
#' nn=sample(1:nrow(frs_by_programid), 1); frs_by_programid[REGISTRY_ID == frs_by_programid$REGISTRY_ID[nn],]
#'
#'              lat       lon  REGISTRY_ID program   pgm_sys_id
#'      1: 47.00071 -120.5649 110037546493 WA-FSIS      1796553
#'      2: 47.00071 -120.5649 110037546493    ICIS   1800041945
#'      3: 47.00071 -120.5649 110037546493 WA-FSIS      7886103
#'  ```
#'  
NULL
