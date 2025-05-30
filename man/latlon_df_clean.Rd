% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/latlon_df_clean.R
\name{latlon_df_clean}
\alias{latlon_df_clean}
\title{Get lat lon columns (or create them from geocoding addresses), and clean up those columns in a data.frame}
\usage{
latlon_df_clean(df, invalid_msg_table = FALSE, set_invalid_to_na = TRUE)
}
\arguments{
\item{df}{data.frame With columns lat and lon or names that can be interpreted as such,
or addresses that can be geocoding to create lat lon columns}

\item{invalid_msg_table}{Set to TRUE to add columns "valid" and "invalid_msg" to output}

\item{set_invalid_to_na}{if not set FALSE, it replaces invalid lat or lon with NA values}
}
\value{
Returns the same data.frame but with relevant colnames changed to lat and lon,
or lat,lon added based on addresses,
and invalid lat or lon values cleaned up if possible or else replaced with NA,
and optional columns "valid" and "invalid_msg"
}
\description{
Utility to identify lat and lon columns (or addresses), renaming and cleaning them up.
}
\details{
Tries to figure out which columns seem to have lat lon values,
or addresses that can be converted to lat lon columns, renames those in the data.frame.
Cleans up lat and lon values (removes extra characters, makes numeric)
}
\examples{
#  x <- latlon_df_clean(x)
 latlon_df_clean(testpoints_bad, set_invalid_to_na = F, invalid_msg_table = T)

}
\seealso{
Used by \code{\link[=latlon_from_anything]{latlon_from_anything()}}. Uses \code{\link[=latlon_infer]{latlon_infer()}} \code{\link[=latlon_is.valid]{latlon_is.valid()}} \code{\link[=latlon_as.numeric]{latlon_as.numeric()}}
}
\keyword{internal}
