% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ejam2ratios.R
\name{ejam2ratios}
\alias{ejam2ratios}
\title{Quick view of summary stats by type of stat, but lacks rounding specific to each type, etc.}
\usage{
ejam2ratios(ejamitout, sitenumber = NULL, decimals = 1)
}
\arguments{
\item{ejamitout}{list as from ejamit() that includes results_overall}

\item{sitenumber}{if NULL, uses overall results. If an integer, uses that site,
based on just one row from ejamitout$results_bysite}

\item{decimals}{optional number of decimal places to round to}
}
\value{
prints to console and returns a simple data.frame
}
\description{
Quick view of summary stats by type of stat, but lacks rounding specific to each type, etc.
}
\examples{
 ejam2barplot(testoutput_doaggregate_100pts_1miles)
 ejam2ratios(testoutput_ejamit_100pts_1miles)

}
