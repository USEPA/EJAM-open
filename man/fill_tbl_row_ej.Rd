% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/community_report_helper_funs.R
\name{fill_tbl_row_ej}
\alias{fill_tbl_row_ej}
\title{Write an EJ or EJ supp index to an html table row}
\usage{
fill_tbl_row_ej(output_df, Rname, longname)
}
\arguments{
\item{output_df, }{single row of results table from doaggregate -
either results_overall or one row of bysite}

\item{Rname, }{variable name of indicator to pull from results,
such as 'pm', 'pctlowinc', 'Demog.Index'}

\item{longname, }{nicer name of indicator to use in table row;
can include HTML sub/superscripts}
}
\description{
Write an EJ or EJ supp index to an html table row
}
\seealso{
used by \code{\link[=build_community_report]{build_community_report()}}
}
\keyword{internal}
