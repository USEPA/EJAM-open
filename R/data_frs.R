#' @name frs
#' @title frs (DATA) EPA Facility Registry Service table of regulated sites
#' @description This is a data.table snapshot version of the EPA FRS. 
#'   You can look up sites by REGISTRY_ID in [frs], and get their location, etc.
#' @seealso [epa_programs] [epa_programs_defined] [frs_by_programid]  [frs_by_naics] [frs_by_sic]
#' @details 
#'  This dataset can be updated by a package maintainer by using 
#'     frs_update_datasets() (which is not an exported function)
#'
#'   The definitions of active/inactive here are not quite the 
#'   same as used in ECHO. See attributes(frs) to see date created, etc.
#'   
#'   Also, EJScreen has maps of EPA-regulated facilities of a few program types,
#'   as provided here: <https://web.archive.org/web/20250118193121/https://www.epa.gov/ejscreen/ejscreen-map-descriptions#sites-reporting-to-epa>
#'   and for a table of acronym definitions 
#'   see https://www.epa.gov/sites/default/files/2021-05/frs_program_abbreviations_and_names.xlsx
#'   and [epa_programs_defined]
#'   
#'  - Count of    all REGISTRY_ID rows:   Approx 7 million
#'  - Count of unique REGISTRY_ID values: Approx 4-5 million
#'  - Clearly inactive unique IDs:        Approx 1-2 million
#'  - Assumed   active unique IDs:        Approx 3 million
#' 
#'  - frs rows total:            Approx 2-3 million (2.6m as of 02/2025)
#'  - frs_by_programid rows:     Approx 3-4 million (3.5m as of 02/2025)
#'  - frs_by_naics rows:         Approx 640k (541k unique regid as of 02/2025, 1858 unique NAICS)
#'  - frs_by_sic rows:           Approx 764k (664k unique regid as of 02/2025, 2048 unique SIC)
#' 
#'   Classes ‘data.table’ and 'data.frame'
#'   
#'   colnames 
#'   
#'   - \[1,\] "lat"
#'   - \[2,\] "lon"
#'   - \[3,\] "REGISTRY_ID" like 110000343003
#'   - \[4,\] "PRIMARY_NAME"
#'   - \[5,\] "NAICS" csv group of codes per site
#'   - \[6,\] "SIC"
#'   - \[7,\] "PGM_SYS_ACRNMS" like RCRAINFO:XJW000200113
NULL
