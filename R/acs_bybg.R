
### Adding these functions would require the tidycensus pkg and scales and stringr
# ## not already required by EJAM:
# library(tidycensus)
# library(scales)
# library(stringr)
#   and tidycensus imports these:
# httr, tigris, stringr, jsonlite,
# purrr, rvest,  rappdirs, readr, xml2,
# units, utils, rlang, crayon, tidyselect
################################# #

# #  a census api key would be needed here if large number queries needed
################################# #


#' download ACS 5year data from Census API, at block group resolution (slowly for entire US)
#'
#' @param variables Vector of variables - see get_acs from tidycensus package
#' @param table  see get_acs from tidycensus package
#' @param cache_table  see get_acs from tidycensus package
#' @param year e.g., 2022  see get_acs from tidycensus package
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
#' newvars <- acs_bybg(variables = c("B01001_001", paste0("B01001_0", 31:39)),
#'   state = "DC")
#' setnames(newvars, "GEOID", "bgfips")
#' newvars <- newvars[blockgroupstats[, .(bgfips, ST)], , on = "bgfips"]
#' names(newvars) <- gsub("E$", "", names(newvars))
#' formula1 <- c(
#'  "pop = B01001_001",
#'  "age1849female = (B01001_031 + B01001_032 + B01001_033 + B01001_034 + 
#'  B01001_035 + B01001_036 + B01001_037 + B01001_038 + B01001_039)", 
#'  "pct1849female = ifelse(pop == 0, 0, age1849female / pop)")
#' newvars <- calc_ejam(newvars, formulas = formula1, 
#'   keep.old = c("bgid", "ST", "pop"))
#' newvars[ST == "DC", ]
#' 
#' @return data.table (not tibble, and not just a data.frame)
#' 
#' @export
#'
acs_bybg <- function(
    variables = c(pop = "B01001_001"),
    table = NULL,
    cache_table = FALSE,
    year = 2022,
    output = "wide",
    state = EJAM::stateinfo$ST, # state.abb would lack DC and PR
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
  
  # NEED API KEY POSSIBLY, FOR LARGE QUERIES AT LEAST
  
  if (exists("get_acs")) {
    if (!is.null(table) && !is.null(variables)) {
    warning( "Specify variables or a table to retrieve; they cannot be combined. Using table and ignoring variables.")
      variables = NULL
    }
    # x <- load_variables(year, survey) # slow and requires tidycensus package 
    # print(x[grepl("b01001_", x$name, ignore.case = T) & grepl("Female", x$label) & grepl("group", x$geography), ], n = 25)
    allstates <- list()
    
    for (i in 1:length(state)) {
      MYST <- state[i]
      bgs <- get_acs(geography = geography,   # requires tidycensus package 
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
  }
  # ?get_acs
}
##################################################################### #


# # SCRIPT TO GET 
# # PERCENT OF POPULATION THAT IS WOMEN OF CHILD BEARING AGE
# # FOR ALL US BLOCK GROUPS FROM ACS (but missing PR, VI, other Island Areas probably!)
# # Use ages 18-49, not the more widely used 16-49, since ages 15-17 are all in a single bin.
# 
# library(data.table)
# library(tidycensus) # NEED API KEY POSSIBLY, FOR LARGE QUERIES AT LEAST
# 
# x <- tidycensus::load_variables(2022, "acs5")
# # print(x[grepl("b01001_", x$name, ignore.case = T) & grepl("Female", x$label) & grepl("group", x$geography), ], n = 25)
# allstates <- list()
# 
# for (i in 1:length(state.abb)) {
#   MYST <- state.abb[i]
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

