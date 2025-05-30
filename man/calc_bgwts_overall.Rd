% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PROXIMITY_FUNCTIONS.R
\name{calc_bgwts_overall}
\alias{calc_bgwts_overall}
\title{aggregate blockwt values by blockgroup}
\usage{
calc_bgwts_overall(sites2blocks)
}
\arguments{
\item{sites2blocks}{like output of \code{\link[=getblocksnearby]{getblocksnearby()}}
or input to \code{\link[=doaggregate]{doaggregate()}} or \code{\link[=custom_doaggregate]{custom_doaggregate()}}}
}
\value{
data.table, 1 row per blockgroup (even if bg is near 2+ sites),
so it is a table of all the unique block groups in the overall
analysis (merged across all sites), with a weight that indicates
what fraction of that bg population is included in the overall
analysis. This can be used to get overall results if it is
joined to block group residential population data, etc.,
to aggregate each indicator over all block groups using the weights.
}
\description{
aggregate blockwt values by blockgroup
}
\seealso{
\code{\link[=custom_doaggregate]{custom_doaggregate()}}
}
\keyword{internal}
