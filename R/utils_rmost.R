#' utility to rm(list=ls()) but NOT remove key datasets EJAM uses 
#' @details removes them from globalenv()
#' 
#' @export
#' @keywords internal
#'
rmost <- function(notremove = c(
  "blockwts", "blockpoints", "blockid2fips", "quaddata", "localtree",
  "bgej", "bgid2fips",
  "frs", "frs_by_programid", "frs_by_naics", "frs_by_sic", "frs_by_mact"
) ) {
  rm(list = setdiff(
    ls(envir = globalenv()),
    notremove
  ),
  envir = globalenv()
  )
}
