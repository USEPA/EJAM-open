#' @name blockgroupstats
#' @docType data
#' @title blockgroupstats (DATA) EJScreen demographic and environmental indicators for Census block groups
#'
#' @description
#'   The EJScreen dataset (demographic, environmental indicators).
#'
#'   For EJ Indexes, see [bgej]
#'
#' @details
#'   - Version `r as.vector(metadata_mapping$blockgroupstats[['ejam_package_version']])` of EJAM / EJScreen uses ACS data from `r as.vector(metadata_mapping$blockgroupstats[['acs_version']])`.
#'
#'   Each year this should be re-created as for the latest version.
#'   See attributes(blockgroupstats)
#'
#'   It is a data.table of US Census blockgroups (not blocks).
#'   With PR, and Island Areas
#'
#'   See <https://www.epa.gov/ejscreen>
#'
#'   Column names include bgfips, bgid (for join to blockwt$bgid), pop, pctlowinc, etc.
#'
#'
NULL
