% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PROXIMITY_FUNCTIONS.R
\name{calcweight}
\alias{calcweight}
\title{utility - what variable is the weight used to aggregate this variable as a weighted mean?}
\usage{
calcweight(varnames)
}
\arguments{
\item{varnames}{vector like names_d}
}
\value{
vector same length as varnames, like c("pop", "povknownratio", "hhlds")
}
\description{
utility - what variable is the weight used to aggregate this variable as a weighted mean?
}
\examples{
 x = names_these
 cbind(indicator = x, calctype = calctype(x), calcweight = calcweight(x))

}
\keyword{internal}
