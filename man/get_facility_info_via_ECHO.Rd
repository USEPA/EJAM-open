% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_draft_get_facility_info_via_ECHO.R
\name{get_facility_info_via_ECHO}
\alias{get_facility_info_via_ECHO}
\title{NOT USED - WAS WORK IN PROGRESS - was to get info on EPA-regulated facilities via EPA ECHO API - query by NAICS etc.}
\usage{
get_facility_info_via_ECHO(
  p_ncs = NULL,
  qcolumns = c(16, 17),
  output = "JSON",
  otherparameters = NULL,
  url_not_query = TRUE,
  testing = FALSE,
  getcsv = FALSE
)
}
\arguments{
\item{p_ncs}{NAICS industrial code}

\item{qcolumns}{vector specifying which variables to return (see varsinfo_ECHO_API).
Column numbers work and are what the ECHO API expects, but here you can instead - or in addition -
also use these words referring to groups of variables defined in this package:
critical  best   useful   programid    ej
to specify variables where, e.g., the word critical would
get variables where varsinfo_ECHO_API$critical == TRUE}

\item{output}{JSON by default, to get output in that format}

\item{otherparameters}{appended text at end of URL}

\item{url_not_query}{logical, just return the URL but not query}

\item{testing}{logical}

\item{getcsv}{logical, use get_download}
}
\value{
Tries to return a table via data.table::as.data.table(), with these columns:
"ObjectId" "FacName" "RegistryID" "FacLat" "FacLong" "lat" "lon" "registry_id"
}
\description{
see EJAM package functions like
latlon_from_naics() latlon_from_siteid()
latlon_from_programid()  latlon_from_program()
that might make this function obsolete.
}
\details{
See info about ECHO web services at \url{https://echo.epa.gov/tools/web-services}.
Use the Metadata service endpoint for a list of available output objects,
their Column Ids, and their definitions to help you build your customized output,
and see examples at
\url{https://echo.epa.gov/tools/web-services/facility-search-all-data#/Metadata} and
\url{https://echodata.epa.gov/echo/echo_rest_services.metadata?output=JSON}

In ECHO, one can search for facilities or permits by
EPA Registry ID (i.e., FRS ID) or by the
Program System ID (CWA, CAA, SDWA, or RCRA).
The web interface at \url{https://echo.epa.gov/facilities/facility-search}
allows data entry of up to 2,000 IDs pasted from spreadsheet column, or comma- or return-separated.
}
\examples{
  x1 = get_facility_info_via_ECHO(562213, url_not_query = F)
  x2 = latlon_from_naics(562213)

}
\seealso{
varsinfo_ECHO_API  get_siteid_from_naics()
}
\keyword{internal}
