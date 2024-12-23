
#' @name bgid2fips
#' @title bgid2fips (DATA) Census FIPS codes of block groups
#' @details
#'   For documentation on EJScreen, see [EJScreen documentation](https://www.epa.gov/ejscreen)
#'   
#'   bgid2fips is a table of all census block groups, with their FIPS codes.
#'   
#'   It also has a column
#'   called `blockid` that can join it to other block datasets.
#'   ```
#'     dataload_from_pins('bgid2fips')
#'     
#'     names(bgid2fips)
#'     dim(bgid2fips)
#'   ```
NULL
