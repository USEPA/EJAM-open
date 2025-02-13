% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_ejamit_columns.R
\name{format_ejamit_columns}
\alias{format_ejamit_columns}
\title{Format ejamit results for use in tables, charts, etc.}
\usage{
format_ejamit_columns(df, nms = c())
}
\arguments{
\item{df}{data table of ejamit results,}

\item{nms, }{name(s) of columns referring to EJAM indicators, such as "Demog.Index'}
}
\value{
a named vector with formatted values, corresponding to valid column names provided
}
\description{
Applies rounding, sigfig, and percentage info to display columns of ejamit using map_headernames
}
\examples{
  # x <- ejamit(testpoints_10, radius = 1)
  x <- testoutput_ejamit_10pts_1miles
  format_ejamit_columns(x$results_overall, 'Demog.Index')
  format_ejamit_columns(x$results_overall, c('Demog.Index', 'no2'))
  format_ejamit_columns(x$results_overall, names_d)

}
