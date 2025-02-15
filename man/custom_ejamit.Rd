% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PROXIMITY_FUNCTIONS.R
\name{custom_ejamit}
\alias{custom_ejamit}
\title{custom version of ejamit() for calculating user-provided indicators}
\usage{
custom_ejamit(
  sitepoints,
  radius = 3,
  fips = NULL,
  shapefile = NULL,
  custom_blockgroupstats = blockgroupstats,
  countcols = names_wts,
  popmeancols = names_these,
  wtcols = names_wts,
  custom_formulas = NULL,
  custom_cols = NULL,
  custom_map_headernames = map_headernames
)
}
\arguments{
\item{sitepoints}{see \code{\link[=ejamit]{ejamit()}}}

\item{radius}{see \code{\link[=ejamit]{ejamit()}}}

\item{fips}{see \code{\link[=ejamit]{ejamit()}}}

\item{shapefile}{see \code{\link[=ejamit]{ejamit()}}}

\item{custom_blockgroupstats}{like blockgroupstats but with custom
indicators, one value per block group, with colnames bgid, bgfips, pop}

\item{countcols}{vector of colnames in custom_blockgroupstats to be
aggregated as sums of counts, like population counts}

\item{popmeancols}{vector of colnames in custom_blockgroupstats to be
aggregated as weighted means, population weighted or with other weights}

\item{wtcols}{vector of colnames to use as the weights for wtd means,
same length as popmeancols, but not used yet}

\item{custom_formulas}{like formulas_all,  not used yet}

\item{custom_cols}{not used yet}

\item{custom_map_headernames}{like map_headernames but for the
custom indicators}
}
\value{
returns the output of \code{\link[=custom_doaggregate]{custom_doaggregate()}}
}
\description{
custom version of ejamit() for calculating user-provided indicators
}
