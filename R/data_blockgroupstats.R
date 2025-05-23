#' @name blockgroupstats
#' @docType data
#' @title blockgroupstats (DATA) residential population and environmental indicators for Census block groups
#'
#' @description
#'   The dataset of residential population data and environmental indicators originally from EJScreen.
#'
#'   For Summary Indexes, see [bgej]
#'
#' @details
#'   - Version `r as.vector(metadata_mapping$blockgroupstats[['ejam_package_version']])` of EJAM
#'   uses EPA environmental indicators and population indicators derived from
#'   ACS data from `r as.vector(metadata_mapping$blockgroupstats[['acs_version']])`.
#'
#'   Each year this should be re-created as for the latest version.
#'   See attributes(blockgroupstats)
#'
#'   It is a data.table of US Census blockgroups (not blocks).
#'   With PR, and Island Areas
#'
#'   Column names include bgfips, bgid (for join to blockwt$bgid), pop, pctlowinc, etc.
#'
#'
NULL
