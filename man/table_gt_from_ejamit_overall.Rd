% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_gt_.R
\name{table_gt_from_ejamit_overall}
\alias{table_gt_from_ejamit_overall}
\title{Create a formatted table of results from EJAM overall summary stats}
\usage{
table_gt_from_ejamit_overall(
  ejamit_results_1row = NULL,
  type = c("demog", "envt")[1]
)
}
\arguments{
\item{ejamit_results_1row}{1-row data.table like testoutput_ejamit_100pts_1miles$results_overall,
as would come from ejamit(testpoints_10)$results_overall}

\item{type}{Must be "demog" or "envt" -- Creates one of these at a time}
}
\value{
Provides table in gt format from the R package called gt
}
\description{
Uses the results_overall element of ejamit() output
}
\examples{
 x <- table_gt_from_ejamit_overall(testoutput_ejamit_100pts_1miles$results_overall)
 
}
