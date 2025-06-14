% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/echo_colids_from_num_name_group.R
\name{echo_colids_from_num_name_group}
\alias{echo_colids_from_num_name_group}
\title{Interpret and check the list of requested columns to be asked of the ECHO API get_facility_info}
\usage{
echo_colids_from_num_name_group(x = NULL)
}
\arguments{
\item{x}{vector of specifiers of variables needed to get from the ECHO facility query API,
which can be the variable column id that the API needs (1 through about 316),
or the actual ObjectName (variable name) found in varsinfo_ECHO_API,
or a word specifying a group of variables like critical or best
or others that are logical class columns in varsinfo_ECHO_API
or the word all (for all available, about 316).}
}
\value{
Not the same sort order or length as input necessarily! A valid list of numbers that are the ColumnID numbers
}
\description{
Interpret and check the list of requested columns to be asked of the ECHO API get_facility_info
}
\examples{
 
  x <- echo_colids_from_num_name_group(c('critical', 'NC', 'CensusBlockGroup' ))
  x
  varsinfo_ECHO_API$ObjectName[match( x, varsinfo_ECHO_API$ColumnID)]
  echo_colids_from_num_name_group(300:400)
  echo_colids_from_num_name_group(5:1)
  echo_colids_from_num_name_group(c(5:1,1:3))

}
\seealso{
\code{\link[=get_facility_info_via_ECHO]{get_facility_info_via_ECHO()}}
}
\keyword{internal}
