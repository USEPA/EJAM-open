% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/community_report_helper_funs.R
\name{fill_tbl_full}
\alias{fill_tbl_full}
\title{Create full demog. or envt. HTML table of indicator rows}
\usage{
fill_tbl_full(
  output_df,
  title = "EJScreen environmental and socioeconomic indicators data",
  title_top_row = "",
  show_ratios_in_report = TRUE
)
}
\arguments{
\item{output_df, }{single row of results table from doaggregate -
either results_overall or one row of bysite}

\item{title}{Text of overall title of report table}

\item{title_top_row}{text for upper left cell, header row. Can be blank, or e.g., 'SELECTED VARIABLES'}

\item{show_ratios_in_report}{logical, whether to add columns with ratios to US and State overall values}
}
\description{
Create full demog. or envt. HTML table of indicator rows
}
\seealso{
used by \code{\link[=build_community_report]{build_community_report()}}
}
\keyword{internal}
