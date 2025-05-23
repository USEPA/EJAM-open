

#' utility to add lat lon columns to data.table by reference, joining on blockid
#'
#' get expanded version of data.table, such as sites2blocks,
#' with new lat,lon columns
#'
#' @param s2b data.table like [testoutput_getblocksnearby_10pts_1miles],
#' output of [getblocksnearby()],
#' with column called blockid
#'
#' @return returns the input data.table but with lat,lon columns added as block coordinates
#' @examples
#'  s2b = copy(testoutput_getblocksnearby_10pts_1miles)
#'  latlon_join_on_blockid(s2b) 
#'
#' @keywords internal
#'
latlon_join_on_blockid = function(s2b) {
  
  if (missing(s2b)) {
    warning('No value provided for argument "s2b".')
    return(NULL)
  }
  else if (all(is.na(s2b)) | is.null(s2b)) {
      warning('NULL or NA "s2b" passed as inputs.')
      return(NULL)
  }
  if (all(c('lat','lon') %in% names(s2b))) {message('already has lat,lon'); return(s2b)}
  return(
    # merge(s2b, blockpoints , on = "blockid")
    # better via a join, though right? could modify param by reference without even explicitly returning anything then
    s2b[blockpoints, `:=`(lat = lat, lon = lon), on = "blockid"]
  )
}
########################################################################################### #
