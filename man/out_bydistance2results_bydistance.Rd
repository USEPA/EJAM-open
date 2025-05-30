% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ejamit_compare_distances.R
\name{out_bydistance2results_bydistance}
\alias{out_bydistance2results_bydistance}
\title{Extract summary from list of ejamit() runs at multiple distances
Get a table, one row per distance. Overall summary, not each site.}
\usage{
out_bydistance2results_bydistance(out_bydistance)
}
\arguments{
\item{out_bydistance}{list of tables that is output of \code{\link[=ejamit_compare_distances_fulloutput]{ejamit_compare_distances_fulloutput()}}}
}
\value{
a table you can call results_bydistance,
that is like ejamit()$results_overall, but that
has 1 row per distance (radius or buffer width)
}
\description{
Extract summary from list of ejamit() runs at multiple distances
Get a table, one row per distance. Overall summary, not each site.
}
\details{
This will compile a results_bydistance table from
output of ejamit_compare_distances_fulloutput(),
using the ejamit()$results_overall for each distance.
}
\seealso{
\code{\link[=ejamit_compare_distances]{ejamit_compare_distances()}} \code{\link[=ejamit_compare_distances_fulloutput]{ejamit_compare_distances_fulloutput()}}
}
\keyword{internal}
