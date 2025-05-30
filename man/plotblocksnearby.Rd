% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotblocksnearby.R
\name{plotblocksnearby}
\alias{plotblocksnearby}
\title{plotblocksnearby - Map view of Census blocks (their centroids) near one or more sites
Utility to quickly view one or more facility points on map with the blocks found nearby}
\usage{
plotblocksnearby(
  sitepoints,
  radius = 3,
  sites2blocks,
  siteidvarname = "ejam_uniq_id",
  usemapfast = TRUE,
  returnmap = FALSE,
  overlay_blockgroups = FALSE,
  maxradius = 31.07,
  avoidorphans = FALSE,
  ...
)
}
\arguments{
\item{sitepoints}{table of points with lat, lon in decimal degrees (data.frame or data.table),
but also could just be the output of getblocksnearby() if that has already been done.}

\item{radius}{optional. in miles (Max allowed is 32 miles, or just over 50 kilometers since 31.06856 miles is 50 * 1000 / meters_per_mile).}

\item{sites2blocks}{optional. If provided, used as sites2blocks like \link{testoutput_getblocksnearby_10pts_1miles}
If neither sites2blocks nor sitepoints is provided it cannot plot and returns error.
If sites2blocks and sitepoints are both provided, it uses them both to plot blocks and sites (centers of circles).
If sites2blocks not provided, but sitepoints alone is provided, checks if sitepoints is actually sites2blocks, and uses as such.
If sites2blocks not provided, but sitepoints alone is provided, and sitepoints is really sitepoints, it runs getblocksnearby() to create sites2blocks.
If sites2blocks is provided, but sitepoints is not, it could only do a bad approximation of sitepoints so it will not draw the circle or site at center of the circle.}

\item{siteidvarname}{optional. specifies the column name in sites2blocks that is the unique site id, the values of which should
also be the row numbers of the corresponding sites in sitepoints, with a site appearing once in sitepoints,
and in sites2blocks appearing once per block that is near that site.}

\item{usemapfast}{optional. simpler plot if FALSE}

\item{returnmap}{optional. if set TRUE, returns the leaflet map object instead of tabular info.
That  is needed to pass results to map_blockgroups_over_blocks() for example.}

\item{overlay_blockgroups}{optional. if set TRUE, also plots overlay of blockgroup boundaries.}

\item{maxradius}{optional. see \code{\link[=getblocksnearby]{getblocksnearby()}}}

\item{avoidorphans}{optional. see \code{\link[=getblocksnearby]{getblocksnearby()}}}

\item{...}{optional. passed to mapfast() or plot() depending on usemapfast,
like column_names = "ej" for better map popups on block points}
}
\value{
invisibly returns sites2blocks like getblocksnearby() does
}
\description{
plotblocksnearby - Map view of Census blocks (their centroids) near one or more sites
Utility to quickly view one or more facility points on map with the blocks found nearby
}
\details{
Uses \code{\link[=getblocksnearby]{getblocksnearby()}} if lat,lon points provided as sitepoints,
but skips it if looks like user passed output of getblocksnearby(),
and then displays a map of those blocks near the specified point.
}
\examples{
  #  see all Census Blocks within 1 mile of 1 site, if already had run getblocksnearby()
  getblocks_output <- copy(testoutput_getblocksnearby_10pts_1miles)
  if ("siteid" \%in\% names(getblocks_output)) {
  siteidvarname <- "siteid" # the old default
  eg <- getblocks_output[siteid == 1,]
  eg2 <- getblocks_output[siteid \%in\% c(4,10),]
  } else {
  siteidvarname <- "ejam_uniq_id"
   eg <- getblocks_output[ejam_uniq_id == 1,]
   eg2 <- getblocks_output[ejam_uniq_id \%in\% c(4,10),]
  }
   z <-  plotblocksnearby(sitepoints = testpoints_10, 
     sites2blocks = eg, radius = 1)
   # see two sites if already had run getblocksnearby()
   z2 <-  plotblocksnearby(sitepoints = testpoints_10[c(4,10),], 
     sites2blocks = eg2, radius = 1)
  \donttest{
  # See one randomly selected regulated facility from FRS and all Census Blocks within 2 miles:
    plotblocksnearby(testpoints_n(1), 2) 
  # See two sites and all Census Blocks within 5 kilometers
    plotblocksnearby(testpoints_2, radius = convert_units(5, from = "km", towhat = "miles"))
  # See 100 sites and all blocks within 1 mile of each - 
  # Note you have to specify radius here or it uses default that may not match intent 
  # - and this is a bit slow
  plotblocksnearby(
    testoutput_ejamit_100pts_1miles$results_bysite[, 
        c(siteidvarname, "lat", "lon"), with=FALSE],
   radius = 1)
  }
}
