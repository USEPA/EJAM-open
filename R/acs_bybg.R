
### Adding these functions would require the tidycensus pkg and scales and stringr
# ## not already required by EJAM:
# library(tidycensus) # About 2MB (+ other pkgs it uses)
# library(scales)
# library(stringr)
#   and tidycensus imports these:
# httr, (sf), (stringr), tigris, jsonlite,
# purrr, rvest,  rappdirs, readr, xml2, units, utils, rlang, crayon, tidyselect
################################# #

# #  a census api key would be needed here if large number queries needed

################################# #


#' download ACS 5year data from Census API, at block group resolution (slowly if for entire US)
#' @details
#' (Probably) requires [getting and specifying an API key for Census Bureau](https://api.census.gov/data/key_signup.html) ! (at least if query is large).
#'   see [tidycensus package help](https://walker-data.com/tidycensus/)
#'
#' @param variables Vector of variables - see get_acs from tidycensus package
#' @param table  see get_acs from tidycensus package
#' @param cache_table  see get_acs from tidycensus package
#' @param year e.g., 2022  see get_acs from tidycensus package, and
#'   the helper function in the EJAM package called [acsendyear()]
#' @param output   see get_acs from tidycensus package
#' @param state Default is 2-character abbreviations, vector of all US States, DC, and PR.
#' @param county   see get_acs from tidycensus package
#' @param zcta   see get_acs from tidycensus package
#' @param geometry   see get_acs from tidycensus package
#' @param keep_geo_vars   see get_acs from tidycensus package
#' @param summary_var   see get_acs from tidycensus package
#' @param key   see get_acs from tidycensus package
#' @param moe_level   see get_acs from tidycensus package
#' @param survey   see get_acs from tidycensus package
#' @param show_call   see get_acs from tidycensus package
#' @param geography "block group"
#' @param dropname whether to drop the column called NAME
#' @param ...   see get_acs from tidycensus package
#'
#' @examples
#' \donttest{
#' ## All states, full table
#' # newvars <- acs_bybg(table = "B01001")
#'
#' ## One state, some variables
#' newvars <- acs_bybg(c(pop = "B01001_001", y = "B01001_002"), state = "DC")
#'
#' ## Format new data to match rows of blockgroupstats
#'
#' setnames(newvars, "GEOID", "bgfips")
#' dim(newvars)
#' newvars <- newvars[blockgroupstats[,.(bgfips, ST)], ,  on = "bgfips"]
#' dim(blockgroupstats)
#' dim(newvars)
#' newvars
#' newvars[ST == "DC", ]
#'
#' ## Calculate a new indicator for each block group, using ACS data
#'
#' mystates = c("DC", 'RI')
#' newvars <- acs_bybg(variables = c("B01001_001", paste0("B01001_0", 31:39)),
#'   state = mystates)
#' setnames(newvars, "GEOID", "bgfips")
#' newvars[, ST := fips2state_abbrev(bgfips)]
#' names(newvars) <- gsub("E$", "", names(newvars))
#'
#' # provide formulas for calculating new indicators from ACS raw data:
#' formula1 <- c(
#'  " pop = B01001_001",
#'  " age1849female = (B01001_031 + B01001_032 + B01001_033 + B01001_034 +
#'       B01001_035 + B01001_036 + B01001_037 + B01001_038 + B01001_039)",
#'  " pct1849female = ifelse(pop == 0, 0, age1849female / pop)"
#'  )
#' newvars <- calc_ejam(newvars, formulas = formula1,
#'   keep.old = c("bgid", "ST", "pop", 'bgfips'))
#'
#' newvars[, pct1849female := round(100 * pct1849female, 1)]
#' mapfast(newvars[1:10,], column_names = colnames(newvars),
#'      labels = gsub('pct1849female', 'Women 18-49 as % of residents',
#'               gsub('age1849female', 'Count of women ages 18-49',
#'              fixcolnames(colnames(newvars), 'r', 'long'))))
#'
#' }
#' @return data.table (not tibble, and not just a data.frame)
#'
#' @export
#'
acs_bybg <- function(
    variables = c(pop = "B01001_001"),
    table = NULL,
    cache_table = FALSE,
    year = NULL,
    output = "wide",
    state = stateinfo$ST, # has DC,PR, but not "AS" "GU" "MP" "UM" "VI" # state.abb from datasets pkg would lack DC and PR # stateinfo2 would add "US"
    county = NULL,
    zcta = NULL,
    geometry = FALSE,
    keep_geo_vars = FALSE,
    summary_var = NULL,
    key = NULL, ######################## #
    moe_level = 90,
    survey = "acs5",
    show_call = FALSE,
    geography = "block group",
    dropname = TRUE,
    ...
)  {

  if (missing(year) || is.null(year)) {
    year <- acsendyear()
  }
  year <- as.numeric(year)

  # NEED API KEY POSSIBLY, FOR LARGE QUERIES AT LEAST

  if (nchar(Sys.getenv("CENSUS_API_KEY")) == 0) {
    stop("this requires having set up a census api key - see ?tidycensus::census_api_key  ")
  }

  # if (!exists("get_acs")) {  # now in Imports of DESCRIPTION file
  #   stop('requires the tidycensus package be installed and attached')
  #   } else {
  if (!is.null(table) && !is.null(variables)) {
    warning( "Specify variables or a table to retrieve; they cannot be combined. Using table and ignoring variables.")
    variables = NULL
  }
  # x <- load_variables(year, survey) # slow and requires tidycensus package
  # print(x[grepl("b01001_", x$name, ignore.case = T) & grepl("Female", x$label) & grepl("group", x$geography), ], n = 25)
  allstates <- list()

  for (i in 1:length(state)) {
    MYST <- state[i]
    bgs <- tidycensus::get_acs(geography = geography,   # requires tidycensus package - refer to it like this
                               variables = variables,
                               table = table,
                               cache_table = cache_table,
                               year = year,
                               output = output,
                               state = MYST,
                               county = county,
                               zcta = zcta,
                               geometry = geometry,
                               keep_geo_vars = keep_geo_vars,
                               summary_var = summary_var,
                               key = key,
                               moe_level = moe_level,
                               survey = survey,
                               show_call = show_call,
                               ...)
    data.table::setDT(bgs)
    if (dropname) {
      bgs[, NAME := NULL]
    }
    # bgs[ , pct1849f := age1849 / pop]
    # bgs[is.na(pct1849f), pct1849f := NA] # to be NA instead of NaN
    allstates[[i]] <- bgs
  }

  allstates <- data.table::rbindlist(allstates)
  return(allstates)
  # }
  # ?get_acs
}
##################################################################### #


# # EXAMPLE OF SCRIPT TO GET
# # PERCENT OF POPULATION THAT IS WOMEN OF CHILD BEARING AGE
# # FOR ALL US BLOCK GROUPS FROM ACS (but missing PR, VI, other Island Areas probably!)
# # Use ages 18-49, not the more widely used 16-49, since ages 15-17 are all in a single bin.
#
# library(data.table)
# library(tidycensus) # NEED API KEY, FOR LARGE QUERIES AT LEAST
#
# x <- tidycensus::load_variables(2022, "acs5")
# # print(x[grepl("b01001_", x$name, ignore.case = T) & grepl("Female", x$label) & grepl("group", x$geography), ], n = 25)
# allstates <- list()
#
# for (i in 1:length(stateinfo$ST)) {  # but it may not work for DC and PR ?
#   MYST <- stateinfo$ST[i]
#   y <- get_acs(geography = "block group",  output = "tidy",
#                variables = c(
#                  "B01001_001", paste0("B01001_0", 31:39)),
#                state = MYST)
#   setDT(y)
#   bgs     <-  y[variable != "B01001_001" , .(age1849 = sum(estimate)), by = "GEOID"]
#   totals  <-  y[variable == "B01001_001" , .(pop = sum(estimate)),     by = "GEOID"]
#   bgs <- merge(bgs, totals, by = "GEOID")
#   setDT(bgs)
#   bgs[ , pct1849f := age1849 / pop]
#   bgs[is.na(pct1849f), pct1849f := NA] # to be NA instead of NaN
#   allstates[[i]] <- bgs
# }
#
# allstates <- data.table::rbindlist(allstates)
# pctfemale_18_to_49 <- allstates
# rm(allstates, x, MYST, bgs, totals, i, y)
#
# save(pctfemale_18_to_49, file = "pctfemale_18_to_49.rda")
##################################################################### #

