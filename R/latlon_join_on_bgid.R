

#' utility to add lat lon columns to data.table by reference, joining on bgid
#'
#' get expanded version of data.table, such as copy(blockgroupstats),
#' with new lat,lon columns
#'
#' @param x data.table with column called bgid
#'   (as used in bgid2fips or blockgroupstats)
#' @return x with 2 new columns but side effect is it updates x in calling envt
#' @examples
#' # quick map of blockgroups in 1 state, shown as blockgroup centroids
#' myst <- "NY"
#' dat <- bgpts[fips2state_abbrev(substr(bgfips,1,2)) == myst, ]
#' mapfast(dat, radius = 0.1)
#'
#' # same but popups have all the indicators from EJScreen
#' myst <- "NY"
#' dat <- copy(blockgroupstats[ST == myst, ])
#' # add latlon cols by reference:
#' latlon_join_on_bgid(dat)
#' # specify useful labels for the map popups
#' mapfast(dat, radius = 0.1,
#'         labels = fixcolnames(names(dat), 'r', 'shortlabel'))
#'
#' ## or add the useful labels to the table 1st
#' names(dat) <- fixcolnames(names(dat), "r", "shortlabel")
#' mapfast(dat, radius = 0.1)
#'
#' @keywords internal
#'
latlon_join_on_bgid <- function(x) {

  # seealso latlon_join_on_blockid() utility may be unexported

  if (all(c('lat','lon') %in% names(x))) {message('already has lat,lon'); return(x)}
  x[bgpts, `:=`(lat = lat, lon = lon), on = "bgid"]
}
#################################################### #
