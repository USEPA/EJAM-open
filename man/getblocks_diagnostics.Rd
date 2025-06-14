% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getblocks_diagnostics.R
\name{getblocks_diagnostics}
\alias{getblocks_diagnostics}
\title{utility - How many blocks and many other stats about blocks and sites}
\usage{
getblocks_diagnostics(
  x,
  detailed = FALSE,
  see_pctiles = FALSE,
  see_distanceplot = FALSE
)
}
\arguments{
\item{x}{The output of \code{\link[=getblocksnearby]{getblocksnearby()}} like \link{testoutput_getblocksnearby_10pts_1miles}}

\item{detailed}{if TRUE, also shows in console a long table of frequencies via \code{\link[=getblocks_summarize_blocks_per_site]{getblocks_summarize_blocks_per_site()}}}

\item{see_pctiles}{set to TRUE to see 20 percentiles of distance in a table}

\item{see_distanceplot}{if TRUE, also draws scatter plot of adjusted vs unadj distances}
}
\value{
A list of stats
}
\description{
utility - How many blocks and many other stats about blocks and sites
}
\examples{
  getblocks_diagnostics(testoutput_getblocksnearby_10pts_1miles)
  # library(data.table)
  x <- data.table::copy(testpoints_10)
  setDT(x)
  pts <- rbind(data.table(lat = 40.3, lon = -96.23),
    x[ , .(lat, lon)])
 z <- getblocksnearbyviaQuadTree(pts, 1, quadtree = localtree, quiet = T)
 z[ , .(blocks = .N) , keyby = 'ejam_uniq_id']
 plotblocksnearby(pts, radius = 1, sites2blocks = z)
 zz <- getblocks_diagnostics(z, detailed = T, see_pctiles = T)
cbind(stats = zz)

  getblocks_diagostics(testoutput_getblocksnearby_1000pts_1miles, see_distanceplot = TRUE)

}
\seealso{
This relies on  \code{\link[=getblocks_summarize_blocks_per_site]{getblocks_summarize_blocks_per_site()}} and \code{\link[=getblocks_summarize_sites_per_block]{getblocks_summarize_sites_per_block()}}
}
