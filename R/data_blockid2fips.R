
#' @name blockid2fips
#' @title blockid2fips (DATA) Census FIPS codes of blocks
#' @details
#'   This is a VERY large file, used only when essential.
#'   For documentation on EJScreen, see [EJScreen documentation](https://www.epa.gov/ejscreen)
#'   
#'   blockid2fips is a table of all census blocks, with the FIPS codes.
#'   
#'   It also has a column
#'   called `blockid` that can join it to other block datasets.
#'   ```
#'     dataload_from_pins('blockid2fips')
#'     
#'     names(blockid2fips)
#'     dim(blockid2fips)
#'   ```
NULL
