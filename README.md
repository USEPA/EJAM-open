
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

# EJAM

## EPA’s Environmental Justice Analysis Multi-site tool

EJAM is a user-friendly web app, provided by the US EPA, that can
summarize demographics and environmental conditions for any list of
places in the nation. It provides interactive results and a formatted,
ready-to-share report with written explanations of the results, tables,
and graphics. The report can provide EJ-related information about people
who live in communities near any of the industrial facilities on a list,
for example.

**Note that EJAM is still in active development, not yet released for
anything other than testing, even internally. For an interim, related
tool, please see [EJAM’s ejscreenapi
app.](https://rstudio-connect.dmap-stage.aws.epa.gov/content/374e6403-c660-4692-b62f-61139d1fef69/)**

# What is EJAM?

EJAM is a user-friendly web app, provided by the US EPA, that can
summarize demographics and environmental conditions for any list of
places in the nation. It provides interactive results and a formatted,
ready-to-share report with written explanations of the results, tables,
and graphics. The report can provide EJ-related information about people
who live in communities near any of the industrial facilities on a list,
for example.

To use the tool, one first selects the places to be analyzed. This could
be, for example, everyone within 2 miles of any EPA-regulated facility
in a specific NAICS code (industrial sector). The tool runs a fast
“buffer” or proximity analysis at each location, similar to how EJScreen
provides a standard report for a single location, except EJAM does this
for each of a large number of locations very quickly. Then, most
importantly, EJAM provides a complete overview report, to summarize
environmental conditions and demographics across all the populations and
all of the locations. The results can be explored interactively or
downloaded as a written report with text, tables, and graphics.

An EJAM report can quickly and easily show which demographic groups live
near the selected facilities. It also provides new insights into which
environmental stressors may affect certain demographic subgroups
disproportionately, to varying degrees, near a regulated sector overall
and at individual sites. This allows EJ analysis to move beyond looking
at a small number of indicators for a few demographic groups, at one
site in a single permitting decision, to a more complete picture of
conditions near a whole set of facilities that is the focus of a risk
analysis or a new regulation being considered, for example.

# Related tools and packages

-   [EJScreen](https://www.epa.gov/ejscreen)

-   [EJAM’s ejscreenapi live (internal use) interim
    tool](https://rstudio-connect.dmap-stage.aws.epa.gov/content/374e6403-c660-4692-b62f-61139d1fef69/)

-   EJAM’s code repository for internal EPA use:
    [USEPA/EJAM](https://github.com/USEPA/EJAM#readme) and it relies on
    [EJAMblockdata](https://github.com/USEPA/EJAMblockdata#readme) and
    [EJAMfrsdata](https://github.com/USEPA/EJAMfrsdata#readme)

-   OW’s EJSCREENBatch package repo: [OW’s
    EJSCREENBatch](https://github.com/USEPA/EJSCREENBatch#readme)

## Installation

You can install the development version of EJAM like so:

``` r
#    devtools::install_github('USEPA/EJAM')
## But see ?remotes::install_github 
## To install from a private repo, generate a personal access token (PAT) with at least 
## repo scope in https://github.com/settings/tokens and supply to this argument. 
## This is safer than using a password because you can easily delete a PAT without affecting any others.
## Defaults to the GITHUB_PAT environment variable.
```

## Help / Documentation

``` r
# library(EJAM)
# help("EJAM")
### also see SCRIPT_NONSHINY.R in the package inst folder
```

## Launching the shiny app on your local computer

``` r
# library(EJAM)
# run_app()
##
```

## EJAM features and benefits

EJAM provides a ready-to-use summary report, plus more flexibility,
accuracy, and speed than other tools have. The web-based app quickly
provides a written report plus interactive tables and graphics. Default
indicators will include those in
[EJScreen](https://www.epa.gov/ejscreen) plus a few others (e.g., demog.
subgroups), but user-selected and user-provided indicators could also be
analyzed. The circular buffering module was optimized to be extremely
fast (allowing realtime exploratory work in an app), while still using
the block-population calculation EJScreen uses, making it more
consistent with EJScreen and more accurate than some other approaches.

Building on existing tools such as EJScreen and other environmental and
demographic mapping tools, EJAM provides new levels of flexibility and
power:

### Easy to use with standard default settings, but flexible for power users

The tool uses default indicators but is flexible enough for work with
other environmental and demographic indicators. The default
environmental indicators are \[EJScreen’s environmental indicators\]
(<https://www.epa.gov/ejscreen/overview-environmental-indicators-ejscreen>),
but an analysis can also include other user-selected EPA-hosted data
layers on risks or concentrations (at block group resolution), or
user-provided scores for each block group.

The default demographic indicators are be \[EJScreen’s basic demographic
indicators\]
(<https://www.epa.gov/ejscreen/overview-demographic-indicators-ejscreen>),
with the addition of the 8 race/ethnicity subgroups in ACS5 Table B03002
and % poor as derived from Table C17002. EJAM can also analyze other
demographic indicators, to include user-selected EPA-hosted layers, or
user-provided data. See
[ACSDT5Y2019.B03002](https://data.census.gov/cedsci/table?hidePreview=true&tid=ACSDT5Y2019.B03002)
and
[ACSDT5Y2019.C17002](https://data.census.gov/cedsci/table?hidePreview=true&tid=ACSDT5Y2019.C17002)

When using the tool, one should be able to use various approaches to
defining the areas to be analyzed, using shapefiles rather than just
circular buffers.

The tool also uses a default, standard report, but allows flexibility
beyond that. EJAM provides a standard report (text, graphics, and maps)
to print, download, and use, but that a user could further edit offline
(e.g., in Word). Users also can download individual graphics and data
files (for individual sites and summary statistics).

The results also can be viewed interactively, where one can adjust
certain aspects of the analysis and outputs (and that could possibly
flow into the summary report as well) such as preferred graphics/
tables, indicator thresholds, reference groups, reference areas,
metrics, etc.), to make the outputs fit a given user’s needs.

To provide further flexibility and help avoid duplication of effort,
EJAM’s API would provide access to services such as fast buffering,
summarization, or data.

Also, open source software components will be shared as reusable
well-documented modular tools, to allow developers or others to take
advantage of these resources in running their own analyses or building
or supplementing their own tools, websites, or mobile apps.

### Speed and accuracy

The power of this tool enables faster and more accurate results than
other tools generally have been able to provide. It also should be much
more cost-effective as a public tool than the standard buffering
solution would be, given how GIS analysis credits are used in the
geoplatform. This tool lets any user very quickly see an analysis of a
very large number of places (which EJScreen cannot offer the public),
and immediately get a ready-to-use report that provides perspective on
an entire industrial sector or set of places.

EJAM data updates are meant to match EJScreen’s scheduled updates and
main version numbers, so EJAM 2.1 will use the same basic data as
EJScreen 2.1. The 2.1 version (starting October 2022) has up-to-date
demographic data (e.g., Census 2020 block weights and ACS 2016-2020
block group demographics). Compared to other approaches, EJAM’s
high-resolution buffering provides more accurate information about which
populations live inside a buffer, which is important in rural areas
where a single blockgroup can cover a very large area. For circular
buffers, EJAM 2.1 uses the locations of internal points of Census 2020
blocks, not areal apportionment of block groups, to estimate where
residents live within each block group. This essentially assumes people
are evenly spread out within each block, not each block group, and
treats the block population as if they were all located at the block’s
internal point. There are several million blocks in the US, . The only
more accurate approaches are to use areal apportionment of blocks (not
block groups), but that is very slow, or to use a 30x30 meter grid based
on dasymetric estimates of where people live at even higher resolution
than a block, but  
It also should closely replicate EJScreen’s results for a single
location, to avoid public confusion and inconsistency.

EJAM also takes note of which residences are near which sites, to avoid
double-counting people in the summary statistics but still allow a user
to view results for one site at a time. This is something other tools
and analyses often cannot provide - when they aggregate across sites
they typically do not retain the statistics on individual sites, and
rarely if ever keep track of which communities are near multiple
facilities. Keeping track of this would also allow an analyst to explore
how many people are near multiple sites, or ask which sites in
communities that already have multiple sites nearby.

The intent is for EJAM to be designed so that a later enhancement can
provide a continuous distribution of distances, as distributed across
blocks or people for one or all of the nearby facilities. This would
enable exploration of the complete picture of proximities, rather than
using an arbitrary binary cutoff distance defining near versus far. The
distribution could be sliced later for the summary statistics at any
distance, or could be summarized as a distribution of distances within
each demographic group.

Another goal is for EJAM to be designed with growth in mind, assuming
that the specific indicators of interest will expand or change over
time. It is even possible that multiple resolutions of data will need to
be analyzed, such as block, block group, and tract data for different
indicators. A subsequent refinement might even use a high-resolution
raster grid of population estimates rather than the Census Block counts
currently used for buffering and weighting block group scores for
partially included block groups.