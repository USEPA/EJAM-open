% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shapefile_xyz.R
\name{shape_buffered_from_shapefile}
\alias{shape_buffered_from_shapefile}
\title{shape_buffered_from_shapefile - add buffer around shape}
\usage{
shape_buffered_from_shapefile(shapefile, radius.miles, crs = 4269, ...)
}
\arguments{
\item{shapefile}{spatial object like areas at high risk or areas with facilities to be analyzed}

\item{radius.miles}{width of buffer to add to shapefile
(in case dist is a units object, it should be
convertible to arc_degree if x has geographic coordinates,
and to st_crs(x)$units otherwise)}

\item{crs}{used in st_transform()  default is crs = 4269 or Geodetic CRS NAD83}

\item{...}{passed to st_buffer()}
}
\value{
same format as \code{\link[sf:geos_unary]{sf::st_buffer()}} returns
}
\description{
shape_buffered_from_shapefile - add buffer around shape
}
\details{
Just a wrapper for \code{\link[sf:geos_unary]{sf::st_buffer()}}
}
\examples{
# Within 3 miles of the county borders
fips_counties_from_state_abbrev("DE")[1]
x = shapes_counties_from_countyfips("10001")
xtra = shape_buffered_from_shapefile(x, radius.miles = 3)
map_shapes_leaflet(x) \%>\%
  map_shapes_leaflet_proxy(xtra, color = "black")


}
\seealso{
\code{\link[=get_blockpoints_in_shape]{get_blockpoints_in_shape()}} \code{\link[=shapefile_from_sitepoints]{shapefile_from_sitepoints()}} \code{\link[=shape_buffered_from_shapefile_points]{shape_buffered_from_shapefile_points()}}
}
