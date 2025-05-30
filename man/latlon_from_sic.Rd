% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sic_xyz.R
\name{latlon_from_sic}
\alias{latlon_from_sic}
\alias{regid_from_sic}
\title{Find EPA-regulated facilities in FRS by SIC code (industrial category)}
\usage{
latlon_from_sic(sic, id_only = FALSE)
}
\arguments{
\item{sic}{a vector of SIC codes, or
a data.table with column named code, as with output of \code{\link[=sic_from_any]{sic_from_any()}}}

\item{id_only}{logical optional, set TRUE to get only the vector of REGISTRY_ID
values back instead of a data.frame with lat,lon,SIC columns too.}
}
\value{
A data.table (not just data.frame) with columns called
lat, lon, REGISTRY_ID, SIC (unless the id_only parameter is set to TRUE)
}
\description{
Get lat lon, Registry ID, given SIC industry code(s)
Find all EPA Facility Registry Service (FRS) sites with this exact SIC code (not subcategories)
}
\details{
The EPA also provides a \href{https://www.epa.gov/frs/frs-query#industrial}{FRS Facility Industrial Classification Search tool}
where you can find facilities based on NAICS or SIC.

NOTE: many FRS sites lack SIC code!

Also, this function does not find the sites
identified by FRS data as being in a child SIC (subcategory of your exact query)!

Relies on  frs_by_sic (a data.table)

See info about SIC industry codes at \url{https://www.naics.com/search}
}
\examples{
  regid_from_sic('7300')
  latlon_from_sic('7300')
  latlon_from_sic(sic_from_any("cheese")[,code] )
  head(latlon_from_sic(c('6150', '6300', '5995'), id_only=TRUE))
  # mapfast(frs_from_sic('6150')) # simple map

}
