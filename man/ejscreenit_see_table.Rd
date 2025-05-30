% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ejscreenit_see_table.R
\name{ejscreenit_see_table}
\alias{ejscreenit_see_table}
\title{Utility to get quick view in RStudio viewer of datatable of ejscreenapi results}
\usage{
ejscreenit_see_table(x)
}
\arguments{
\item{x}{output from ejscreenit(), not just the table element of that output,
so it easier to do something like in the example.}
}
\value{
output of DT::datatable()
}
\description{
Utility to get quick view in RStudio viewer of datatable of ejscreenapi results
}
\details{
Also see functions in the EJAM package.
}
\examples{
  # out <- ejscreenit(testpoints_5, radius = 1)
  out <- testoutput_ejscreenit_5
  ejscreenit_see_table(out)
  out$map
  
}
