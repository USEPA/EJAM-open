#' @name frsprogramcodes
#' @title frsprogramcodes DATA EPA programs listed in Facility Registry Service
#' @docType data
#' @seealso [frs] 
#' @description 
#'     data.frame
#'     
#'                                                                   description     code
#'                                                                   
#'     1    National Pollutant Discharge Elimination System (NPDES) (ICIS-NPDES)    NPDES
#'     
#'     2  The Integrated Compliance Information System (ICIS) for Air (ICIS-Air)      AIR
#'     
#'     3    The Resource Conservation and Recovery Act (RCRA) Information System RCRAINFO
#'     
#'     4                                   Risk Management Plan (RMP) facilities      RMP
#'     
#'     5                      The Safe Drinking Water Information System (SDWIS)     SFDW
#'     
#'     6                              The Superfund Enterprise Management System     SEMS
#'     
#'     7                              Clean Air Markets Division Business System   CAMDBS
#'     
#'     8                                        Toxics Release Inventory Program     TRIS
#'     
#'     9                                        Greenhouse Gas Reporting Program   E-GGRT
#'     
#'     10                                             Emissions Inventory System      EIS
#'     
#'     11                                           Toxic Substances Control Act     TSCA
#'     
#' @details Created by script in /data-raw/
#' 
#' Also see [EPA documentation describing each program code](https://www.epa.gov/frs/frs-data-sources) aka data source.
#' 
#' @examples \dontrun{
#'   frs_by_programid[program %in% frsprogramcodes$code, .N, by=program]
#'   
#'   setkey(frs_by_programid,"program")
#'   frs_by_programid["TRIS",]
#'   }
'frsprogramcodes'
