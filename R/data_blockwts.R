#' @name blockwts
#' @title blockwts (DATA) population weights of Census blocks
#' @details
#'   For documentation on EJScreen, see [EJScreen documentation](https://www.epa.gov/ejscreen)
#'   
#'   blockwts is a table of all census blocks, with the weights reflecting
#'   what fraction of the parent block group census 2020 population lived in
#'   that block.  The weights are used to aggregate block-level data to blockgroup,
#'   for cases where only some of the blockgroup is in a circular buffer or polygon.
#'   
#'   It also has a column
#'   called `blockid` that can join it to other block datasets.
#'   ```
#'     dataload_from_pins('blockwts')
#'     
#'     names(blockwts)
#'     dim(blockwts)
#'   ```
NULL
