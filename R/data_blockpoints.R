
#' @name blockpoints
#' @title blockpoints (DATA) Census blocks locations 
#' @details
#'   For documentation on EJScreen, see (https://web.archive.org/web/20250118193121/https://www.epa.gov/ejscreen)
#'   
#'   blockpoints is a table of all census blocks, with the lat, lon
#'   providing the latitude and longitude of the Census Bureau-defined
#'   internal point, like a centroid, of each block. 
#'   
#'   It also has a column
#'   called blockid that can join it to other block datasets.
#'   
#'     dataload_dynamic('blockpoints')
#'     
#'     names(blockpoints)
#'     dim(blockpoints)
NULL
