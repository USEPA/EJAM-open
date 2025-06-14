% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ejamit_compare_distances.R
\name{out_bydistance2results_bysite_bydistance}
\alias{out_bydistance2results_bysite_bydistance}
\title{A way to focus on 1 DISTANCE (RADIUS) at a time (after a multi-distance run), for the list of sites
Get a list of tables, one per distance. Each table has a row per site.}
\usage{
out_bydistance2results_bysite_bydistance(out_bydistance)
}
\arguments{
\item{out_bydistance}{list of tables that is output of \code{\link[=ejamit_compare_distances_fulloutput]{ejamit_compare_distances_fulloutput()}}}
}
\value{
a LIST you can call results_bysite_bydistance (not results_bydistance_bysite),
that is a list where each element is ejamit()$results_bysite for
a unique distance (radius or buffer width)
}
\description{
A way to focus on 1 DISTANCE (RADIUS) at a time (after a multi-distance run), for the list of sites
Get a list of tables, one per distance. Each table has a row per site.
}
\details{
This function might not be used at all.
Extract results_bysite for each distance
from list of ejamit() runs at multiple distances
}
\seealso{
\code{\link[=ejamit_compare_distances]{ejamit_compare_distances()}}  \code{\link[=ejamit_compare_distances_fulloutput]{ejamit_compare_distances_fulloutput()}}
and internal functions \code{\link[=out_bydistance2results_bydistance]{out_bydistance2results_bydistance()}} \code{\link[=out_bydistance2results_bydistance_bysite]{out_bydistance2results_bydistance_bysite()}}
}
\keyword{internal}
