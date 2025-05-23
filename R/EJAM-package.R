

#' @title EJAM - Environment and Residential Population Analysis Multisite tool
#' @name EJAM
#' @aliases EJAM-package
#' 
#' @keywords internal
#' 
#' @md
#'
#' @description
#' EJAM provides a way to run hundreds or thousands of location-specific reports all at once, very quickly.
#' It is also an R package, with functions and data for very fast analysis of areas defined by
#' proximity to points like regulated facilities, areas defined as shapefiles/polygons,
#' or areas specified as Census units (e.g., Counties).
#' 
#' @details  # **What is EJAM and How to do Analysis with the EJAM R Package** ###################
#'
#' [EJAM package documentation](`r EJAM:::repo_from_desc('github.io', get_full_url=T)`) provides
#' an overview of EJAM, a complete reference document on all functions, and
#' articles (vignettes) introducing key functions and how to use them.
#'
#' @details  # **Data Sources -- Residential Population and Environmental Indicators**  ###################
#' 
#'   [Documentation](https://web.archive.org/web/20250118193121/https://www.epa.gov/ejscreen/understanding-ejscreen-results) provides
#'   information about the sources, definitions, and vintage of the
#'   residential population and environmental data and indicators used by
#'   EJAM.
#'   
#'   Each year, EJAM should updated to use newer residential population and environmental data,
#'   and some indicators may be added, dropped, or revised during those updates. 
#'   For details on these updates and changes to datasets and indicators,
#'   please note the information about updates and changes: 
#'   [Data Change Log](https://web.archive.org/web/20250203185136/https://www.epa.gov/ejscreen/ejscreen-change-log).
#'   Residential population data are from the American Community Survey 5-year summary file data that are updated by
#'   the United States Census Bureau annually and then used in EJAM.
#'   
"_PACKAGE"
# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
