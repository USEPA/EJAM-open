% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_indexblocks.R
\name{indexblocks}
\alias{indexblocks}
\title{Create localtree (a quadtree index of all US block centroids) in global environment}
\usage{
indexblocks()
}
\value{
Returns TRUE when done. Side effect is it creates the index in memory.
}
\description{
Create localtree (a quadtree index of all US block centroids) in global environment
}
\details{
Note this is duplicated code in .onAttach() and also in global_defaults_*.R

.onAttach() can be edited to create this when the package loads,
but then it takes time each time a developer rebuilds/installs the package or others that load EJAM.

It also has to happen in global_defaults_*.R if it has not already.
}
