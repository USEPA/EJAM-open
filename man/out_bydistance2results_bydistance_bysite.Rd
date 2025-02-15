% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ejamit_compare_distances.R
\name{out_bydistance2results_bydistance_bysite}
\alias{out_bydistance2results_bydistance_bysite}
\title{A way to focus on 1 SITE at a time, for a few radius choices
Get a list of tables, one per site. Each table has a row per distance.}
\usage{
out_bydistance2results_bydistance_bysite(out_bydistance)
}
\arguments{
\item{out_bydistance}{list of tables that is output of \code{\link[=ejamit_compare_distances_fulloutput]{ejamit_compare_distances_fulloutput()}}}
}
\value{
a LIST you can call results_bydistance_bysite (not results_bysite_bydistance),
that is a list where each element is a table for 1 site (ejam_uniq_id value)
with one row per distance (radius or buffer width).
This table is in the same format as the output of \code{\link[=ejamit_compare_distances]{ejamit_compare_distances()}}
or the internal function out_bydistance2results_bydistance()
}
\description{
A way to focus on 1 SITE at a time, for a few radius choices
Get a list of tables, one per site. Each table has a row per distance.
}
\details{
This function might not be used at all.
Extract/create results_bydistance for each site,
from list of ejamit() runs at multiple distances
}
\seealso{
\code{\link[=ejamit_compare_distances]{ejamit_compare_distances()}} \code{\link[=ejamit_compare_distances_fulloutput]{ejamit_compare_distances_fulloutput()}}
and internal functions \code{\link[=out_bydistance2results_bysite_bydistance]{out_bydistance2results_bysite_bydistance()}} \code{\link[=out_bydistance2results_bydistance]{out_bydistance2results_bydistance()}}
}
\keyword{internal}
