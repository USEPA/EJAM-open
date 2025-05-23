#' @name bgej
#' @title bgej (DATA) Summary Indexes for Census block groups
#' @description
#'   bgej is a table of all blockgroups, with the raw scores of the Summary Indexes
#'   and supplemental summary indexes for all the environmental indicators.
#' 
#' @details
#'   This file is not stored in the package, but is obtained via [dataload_dynamic()].
#'   
#'   For documentation on the residential population and environmental data and indicators used,
#'   see <https://web.archive.org/web/20250118193121/https://www.epa.gov/ejscreen/understanding-ejscreen-results>.
#'   
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

