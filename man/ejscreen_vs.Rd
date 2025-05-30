% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ejscreen_vs_ejam_.R
\name{ejscreen_vs}
\alias{ejscreen_vs}
\title{EJAM/EJSCREEN comparisons - Key function
Best starting point for comparing single-site (EJScreen API) and multisite (EJAM) results}
\usage{
ejscreen_vs(
  defdir = ".",
  n,
  newpts,
  pts = NULL,
  radius = NULL,
  fips = NULL,
  shapefile = NULL,
  savedejscreentableoutput,
  x100fix = TRUE,
  x100varnames = names_pct_as_fraction_ejamit,
  ...
)
}
\arguments{
\item{defdir}{folder to save results files in}

\item{n}{how many places to analyze and compare}

\item{newpts}{logical, if need new set of random locations}

\item{pts}{data.frame of points with columns lat,lon such as testpoints_10}

\item{radius}{miles (when analyzing points)}

\item{fips}{vector of fips codes if relevant (instead of pts, radius, shapefile)}

\item{shapefile}{not implemented directly but shapefiles can be analyzed
if fips provided are for cities, or if newpts are requested to be cities,
which are analyzed using shapefiles in EJAM,
but provided as fips to \code{\link[=ejscreenit]{ejscreenit()}} for the API.
Select new, shape (city) options in interactive mode.}

\item{savedejscreentableoutput}{is a data.table from ejscreenit()$table}
}
\value{
a list of data frames, with names
EJSCREEN, EJAM, EJSCREEN_shown, EJAM_shown, same_shown,
ratio, diff, absdiff, pctdiff, abspctdiff

diff is EJAM - EJSCREEN

ratio is EJAM / EJSCREEN

pctdiff is ratio - 1

abs is absolute value

For each data.frame, colnames are indicators like pop, blockcount_near_site, etc.
and rows represent sites analyzed.
}
\description{
EJAM/EJSCREEN comparisons - Key function
Best starting point for comparing single-site (EJScreen API) and multisite (EJAM) results
}
\details{
THIS IS FOR INTERACTIVE RSTUDIO CONSOLE USE
TO RUN A SET OF POINTS THROUGH BOTH
EJScreen and the EJAM multisite tool
AND SAVE STATS ON THE DIFFERENCES

Also lets you use saved ejscreenapi results and input points
so you can iterate and rerun just the EJAM portion and compare to the saved benchmark data.

The EJAM tool/ function called \code{\link[=ejamit]{ejamit()}}
does not rely on EJScreen's typical single-location approach to do the calculations
and instead tries to replicate what the EJScreen single-site report would do.
As a result, while
\itemize{
\item \emph{\code{\link[=ejamit]{ejamit()}} is much, much faster than \code{\link[=ejscreenit_for_ejam]{ejscreenit_for_ejam()}}} and
\item \emph{provides additional information} (distribution of distances by group, etc.)
\item \emph{features} (histograms, spreadsheet with heatmaps, etc.)
\item \emph{flexibility} (easy for analysts using R to customize analysis, etc.),
\emph{\code{\link[=ejamit]{ejamit()}} does not always exactly replicate EJScreen} --
does not provide 100\% identical results (percentiles, etc.) for
every indicator in every analysis at every location.
For almost all indicators, at 97\% of sites tested, the difference is
either zero difference in the reports or is smaller than a 1\% difference.
There may be edge cases where an indicator differs significantly.
Any differences are due to slight variations in
\item details of the spatial calculations (which blocks are nearby,
sometimes counting 1 extra block as 2.995 miles away while
EJScreen counts it as outside the 3 mile radius, e.g.,
often differing by just <50 feet out of 3 miles).
and possibly other factors like
\item rounding  (how many digits are retained during calculations,
and how many are shown in final reports via rounding and/or significant digits)
\item percentile assignment method should be the same now
(how percentile lookup tables are used,
how ties are treated in percentile lookup tables, etc.)
\item possibly other undocumented small differences in some calculation step
}
}
\examples{
\donttest{
load_all() # so it is easier to use internal functions

vs = ejscreen_vs(pts = testpoints_100, radius = 3)
ejscreen_vs_explain(vs, 1:2)



# To filter that to just the ones where rounded pop disagrees
 table(vs$same_shown$pop)
 vspopoff <- lapply(vs, function(df) df[!vs$same_shown$pop, ])

##  To filter that to just the ones where blockcount was identical,
#   to exclude that as source of difference
vs_blocksmatch = lapply(vs, function(df) df[vs$absdiff[, "blockcount_near_site"] == 0, ])
}
}
\seealso{
Relies on \code{\link[=ejscreen_vs_ejam]{ejscreen_vs_ejam()}} \code{\link[=ejscreen_vs_ejam_alreadyrun]{ejscreen_vs_ejam_alreadyrun()}} \code{\link[=ejscreen_vs_explain]{ejscreen_vs_explain()}}
}
\keyword{internal}
