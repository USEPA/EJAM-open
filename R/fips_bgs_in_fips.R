

#' FIPS - Get unique blockgroup fips in or containing specified fips of any type
#'
#' Convert any FIPS codes to the FIPS of all the blockgroups that are
#'   among or within or containing those FIPS
#'
#' @details  This is a way to get a list of blockgroups, specified by state/county/tract or even block.
#' 
#' Takes a vector of one or more FIPS that could be State (2-digit), County (5-digit),
#'   Tract (11-digit), or blockgroup (12 digit), or even block (15-digit fips).
#'
#'   Returns unique vector of FIPS of all US blockgroups (including DC and Puerto Rico)
#'   that contain any specified blocks, are equal to any specified blockgroup fips,
#'   or are contained within any provided tract/county/state FIPS.
#'
#' @param fips vector of US FIPS codes, as character or numeric,
#'   with or without their leading zeroes, each with as many characters
#' @seealso [fips_lead_zero()]
#' @return vector of blockgroup FIPS (or NA values) that may be much longer than the
#'   vector of fips passed to this function.
#'
#' @examples
#'
#'   # all blockgroups in one state
#'   fips_counties_from_state_abbrev("DE")
#'   fips_bgs_in_fips( fips_counties_from_state_abbrev("DE") )
#'
#'   blockgroupstats[,.N,by=substr(bgfips,1,2)]
#'   length(fips_bgs_in_fips("72"))
#'
#'   # all blockgroups in this one county
#'   fips_bgs_in_fips(30001)
#'   fips_bgs_in_fips("30001")
#'   fips_bgs_in_fips(fips_counties_from_statename("Rhode Island")[1])
#'
#'   # all blockgroups that contain any of these 6 blocks (i.e., just one bg)
#'   ## dataload_from_pins("blockid2fips") # very large file to avoid using unless essential
#'   ## x = blockid2fips$blockfips[1:6] 
#'   x = c("010010201001000", "010010201001001", "010010201001002",
#'    "010010201001003", "010010201001004", "010010201001005")
#'   fips_bgs_in_fips(x)
#'
#'   # 2 counties
#'   fips_bgs_in_fips(c(36009,36011))
#'
#' @export
#' @keywords internal
#' 
fips_bgs_in_fips <- function(fips) {
  
  fips <- fips_lead_zero(fips)
  fips[fipstype(fips) %in% "city"] <- NA # because a 7-digit place/city/town FIPS cannot be neatly broken into blockgroups
  if (anyNA(fips)) {
    howmanyna = sum(is.na(fips))
    warning("NA returned for ", howmanyna," values that failed to match")
    fips <- fips[!is.na(fips)]
  }
  
  # census unit type depends on number of digits (characters) in fips
  #
  # ftype[nchar(fips, keepNA = FALSE) == 15] <- "block"
  # ftype[nchar(fips, keepNA = FALSE) == 12] <- "blockgroup"
  # ftype[nchar(fips, keepNA = FALSE) == 11] <- "tract"
  # ftype[nchar(fips, keepNA = FALSE) ==  7] <- "city"  # e.g, 5560500 is Oshkosh, WI
  # ftype[nchar(fips, keepNA = FALSE) ==  5] <- "county"
  # ftype[!is.na(fips) & nchar(fips) ==  2] <- "state"
  
  len <- nchar(fips)
  if (any(len > 12)) {
    # if len >12 it is a block, so just retain it as the parent blockgroup fips of 12 characters
    fips[len > 12] <- substr(fips[len > 12], 1, 12)
    len <- nchar(fips) # redo this so we recognize as bgs any blocks we just converted to bgs
  }
  # start with the ones that were blocks or blockgroups
  bgs <- unique(fips[len == 12])
  
  # add all blockgroups contained in larger census units
  # if nchar<12, census unit is bigger than bg (i.e., tract, county, state), so return ALL the child bgs
  lens <- unique(len)
  lens <- lens[lens < 12] # because we already got those with nchar 12 or 15 (blockgroups or blocks)
  for (lenx in lens) {
    bgs <- c(bgs, blockgroupstats[substr(bgfips, 1, lenx) %in% fips[len == lenx], bgfips])
  }
  return(unique(bgs))
}
############################################################################# #
