% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_locate_by_id.R
\name{locate_by_id1}
\alias{locate_by_id1}
\title{Helper function to query FRS API to find 1 EPA facility}
\usage{
locate_by_id1(id, type = "frs", ...)
}
\arguments{
\item{id}{one character string that must be a registry ID (default) or program ID}

\item{type}{either frs (default) which means registry_id or
program which means pgm_sys_id}

\item{...}{passed through to url_by_id()}
}
\value{
a 1 row data.frame, columns as returned by the API,
but lat lon instead of Latitude83 Longitude83
("RegistryId", "FacilityName", "LocationAddress", "CityName",
"CountyName", "StateAbbr", "ZipCode", "FIPSCode",
"lat", "lon")
}
\description{
Uses the Facility Registry Service (FRS) API to find a site
by registry ID or program ID.
This uses an API to find sites, but it is faster to look in a table
if that FRS dataset is already loaded in an app, for example.
}
\seealso{
locate_by_id() and url_by_id()
}
\keyword{internal}
