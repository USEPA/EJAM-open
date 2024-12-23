#' @name bgej
#' @title bgej (DATA) EJScreen EJ Indexes for Census block groups
#' @description
#'   bgej is a table of all blockgroups, with the raw scores of the EJ Indexes
#'   and supplemental EJ Indexes for all the environmental indicators.
#' 
#' @details
#'   This file is not stored in the package, but is obtained via [dataload_from_pins()].
#'   
#'   For documentation on the demographic and environmental data and indicators used by
#'   EJScreen and EJAM, see <https://www.epa.gov/ejscreen/understanding-ejscreen-results>.
#'   
#'   See 
#'     
#'     dataload_from_pins('bgej')
#'     
#'     names(bgej)
#'   
#'   The column names are these:
#'   
#'     c("bgfips", "bgid", "ST", "pop", 
#'     names_ej, 
#'     names_ej_supp, 
#'     names_ej_state,
#'     names_ej_supp_state
#'     )
#'   
NULL

