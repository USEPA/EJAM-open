% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shapefile_xyz.R
\name{shapefile_from_zip}
\alias{shapefile_from_zip}
\title{read zipped .zip that may contain a geodatabase file or .shp file etc.}
\usage{
shapefile_from_zip(path, cleanit = TRUE, crs = 4269, layer = NULL, ...)
}
\arguments{
\item{path}{path and filename for .zip file}

\item{cleanit}{optional, whether to use \code{\link[=shapefile_clean]{shapefile_clean()}}}

\item{crs}{passed to \code{\link[=shapefile_from_filepaths]{shapefile_from_filepaths()}} etc. and
default is crs = 4269 or Geodetic CRS NAD83}

\item{layer}{optional layer name passed to \code{\link[sf:st_read]{sf::st_read()}}}

\item{...}{passed to \code{\link[sf:st_read]{sf::st_read()}}}
}
\value{
like output of \code{\link[sf:st_read]{sf::st_read()}}
}
\description{
read zipped .zip that may contain a geodatabase file or .shp file etc.
}
