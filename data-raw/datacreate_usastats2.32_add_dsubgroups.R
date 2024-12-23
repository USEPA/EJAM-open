############################################################################ #

# First, usastats statestats got created via 
# EJAM/data-raw/datacreate_blockgroupstats2.32.R
# EJAM/data-raw/datacreate_usastats2.32.R

# Then, this script below was to add columns to usastats and statestats with info on demographic subgroups
# and maybe other indicators like lowlifex 
# since basic ftp site lookup tables for v2.2 lacked demog subgroups and other variables like lowlifex 
# That is because community report in EJScreen does not report others as percentiles, just raw.
#   but we want to be able to analyze ratio to mean and percentiles for 
# demog subgroups and possibly various other indicators that are extra ones on ejscreen community report.
# All those extra columns and demog subgroups have been added to blockgroupstats via acs22

# also want to sort columns the same way they are ordered in names_d_subgroups_alone and names_d_subgroups_nh
#
############################################################################# #

# ??? need to confirm what States/places to include when calculating US percentiles ####
# Check what States are included in lookup table
#>   setdiff(unique(blockgroupstats$ST), unique(statestats$REGION))
# [1] "AS" "GU" "MP" "VI"

# ideally would create  pctile lookup info for demog subgroups, in us, states, island areas; 
# except that EJScreen community report does not actually report those as percentiles, only as raw percentages,
# even though EJAM was reporting them as percentiles, so either redo EJAM calc of pctiles table for subgroups
# OR just stop reporting those as percentiles.
# Not sure which is easier to do quickly. 
# Simplifies things to stop reporting those as pctiles, just use NA for that cell in a table, 
# but maybe need to have NA values in cols in usastats, statestats for those indicators since code looks for them.
# 
# and same for any other indicators we want to report as percentiles in EJAM 

############################################################################# #

# ADD DEMOG SUBGROUPS to PCTILE LOOKUP ####

#  create pctile lookup columns for the demog subgroups, from the full dataset of all blockgroups.

names_d_subgroups_both <- c(names_d_subgroups_nh, names_d_subgroups_alone)

# # compare to current names
# setdiff(names(  usastats), c(names_e, names_d, names_ej  ,     names_ej_supp      ))
# setdiff(names(statestats), c(names_e, names_d, names_ej_state, names_ej_supp_state))

# missing from usastats right now: ***

EJAM:::setdiff_yx(names(  usastats), c(names_e, names_d, names_ej  ,     names_ej_supp      ))
EJAM:::setdiff_yx(names(statestats), c(names_e, names_d, names_ej_state, names_ej_supp_state))
#
# 
# # compare to installed 
# setdiff(names(statestats), c(EJAM::names_e, EJAM::names_d, EJAM::names_ej_state, EJAM::names_ej_supp_state  ))
# setdiff(names(  usastats), c(EJAM::names_e, EJAM::names_d, EJAM::names_ej  ,     EJAM::names_ej_supp      ))

## missing from statestats now: ***

EJAM:::setdiff_yx(names(statestats), c(EJAM::names_e, EJAM::names_d, EJAM::names_ej_state, EJAM::names_ej_supp_state  ))
EJAM:::setdiff_yx(names(  usastats), c(EJAM::names_e, EJAM::names_d, EJAM::names_ej  ,     EJAM::names_ej_supp      ))
############################################################################# # 

# to add percentile lookup info on demog subgroups, we need that info from blockgroupstats 
library(data.table)
bg <- data.table::copy(blockgroupstats)
bg <- data.table::setDF(bg)

# check those subgroup demog columns in bg
  if (  0 == length(intersect(names_d_subgroups_both, names(bg)))) {
    stop('need subgroup data to create lookup percentile info for subgroups')
  } else {
    print('all variables are columns in bg')
    if (all(0 == bg[ , names_d_subgroups_both])) {
      #placeholder was there but all 0
      stop('need subgroup data to create lookup percentile info for subgroups')
    } else {print('ok not all zero values')}
  }
cbind(percent.NA   =  round(sapply(bg[,names_d_subgroups_both], function(x) sum(is.na(x) )) / NROW(bg), 3) * 100) # count is.na values for each indicator
cbind(percent.zero =  round(sapply(bg[,names_d_subgroups_both], function(x) sum(0 == (x), na.rm = T)) / NROW(bg), 3) * 100) # count zero  values for each indicator
  
  ############################################################################# # 

warning('RESOLVE QUESTION OF WHETHER US PCTILES ARE AMONG 50 STATES PLUS DC, OR ALSO PR, OR EVEN ALSO ISLAND AREAS')
    # need to first RESOLVE QUESTION OF WHETHER US PCTILES ARE AMONG 50 STATES PLUS DC, OR ALSO PR, OR EVEN ALSO ISLAND AREAS.
  # SEEMS LIKE US PCTILE SHOULD BE AMONG 50+DC, EXCLUDING PR?
  # THEN STATE-SPECIFIC PCTILES WILL TREAT DC AND PR AS STATES, 
  # AND NOT SURE IF ISLAND AREAS GET PCTILES CREATED AND WHAT DATA ARE AVAILABLE IN THEM.
# > unique(bg$ST[is.island(bg$ST)])
# note some of ST are among AS, GU, MP, UM, VI
# [1] "AS" "GU" "MP" "VI"

  ############################################################################# # 
  # statestats has some NA values, and some missing columns?
  # 
  #  no d_subgroups numbers yet  
  
############################################################################# # 

updated_usastates = FALSE; updated_statestates = FALSE

# drop the std rows since never used and dropping them again for each indicator in looping use of pctile_from_raw_lookup() is kind of slow.
if (("std" %in% (  usastats$PCTILE))) {  usastats <-   usastats[  usastats$PCTILE != "std", ] ; updated_usastates <- TRUE}
if (("std" %in% (statestats$PCTILE))) {statestats <- statestats[statestats$PCTILE != "std", ] ; updated_statestates <- TRUE}

############################################################################ # 

################################################ #
# Add to demog subgroups to percentile lookup tables #### 

################################################ #
## CREATE USA LOOKUP for subgroups ####
################################################ #

 # names(usastats)

if (all(usastats[,intersect(names_d_subgroups_both, names(usastats))] == 0)  | 
    any(!(names_d_subgroups_both %in% names(usastats))))  {
  
  usastats_subgroups   <- EJAM:::pctiles_lookup_create(data.frame(bg)[ , names_d_subgroups_both]) # function from EJAM package
  usastats_subgroups <- rbind(0, usastats_subgroups); usastats_subgroups$PCTILE[1] <- 0
  usastats_subgroups[1, c("OBJECTID", "REGION")] <- c(0, "USA")
# dim(usastats_subgroups)
# [1] 102  19
  dim(usastats)
  usastats2 <- cbind(usastats, usastats_subgroups[ , setdiff(names(usastats_subgroups), names(usastats))  ])

  # sort cols as sorted in names_d_subgroups_both
  subvars <- intersect(names_d_subgroups_both, names(usastats2) )
  if (length(subvars) > 0) {
    othervars <- setdiff(names(usastats2), subvars)
    usastats2 <- usastats2[ , c(othervars, subvars)]
  }
  all.equal(usastats, usastats2[,1:length(names(usastats))]) # usastats2 has same plus more columns
  usastats <- usastats2
   
  rm( usastats2, usastats_subgroups)
  
} # done with usastats

################################################ #
##  CREATE STATESTATS LOOKUP TABLE for subgroups ####
################################################ #

# if (all(statestats[,names_d_subgroups_both] == 0)  | any(!(names_d_subgroups_both %in% names(statestats)))) {
  
# EJAM::: not needed if did load_all()
  statestats_subgroups <- pctiles_lookup_create(data.frame(bg)[ , names_d_subgroups_both], zone.vector = bg$ST) # from EJAM package
  
  # names(statestats_subgroups)
  morecols = data.frame(as.list(rep(0,length(names_d_subgroups_both))))
  names(morecols) <- names_d_subgroups_both
  zerorowperstate <- data.frame(
    OBJECTID = 0,
    REGION = unique(statestats_subgroups$REGION),
    PCTILE = 0, 
    morecols
  )
  statestats_subgroups <- rbind(statestats_subgroups, zerorowperstate)
  
  statestats_subgroups <- statestats_subgroups[order(statestats_subgroups$REGION, as.numeric(statestats_subgroups$PCTILE)), ]
  # NAs introduced by coercion
  
  statestats_subgroups$OBJECTID <- paste0(statestats_subgroups$REGION, statestats_subgroups$PCTILE) #1:NROW(statestats_subgroups)
  
  if (length(setdiff(names(statestats_subgroups), names(statestats))) > 0) {
    statestats2 <- merge(
      statestats, 
      statestats_subgroups[,  unique(c("PCTILE", "REGION", setdiff(names(statestats_subgroups), names(statestats))))], 
      all.x = TRUE, all.y = FALSE, 
      by = c("PCTILE", "REGION")
    )
    statestats2$OBJECTID.x <- NULL
    statestats2$OBJECTID.y <- NULL
  } else {
    statestats2 <- statestats
  }
  statestats2 <- statestats2[order(statestats2$REGION, as.numeric(statestats2$PCTILE)), ]
  statestats2$OBJECTID <- 1:NROW(statestats2)
   rownames(statestats2) <- paste0(statestats2$REGION, statestats2$PCTILE) # 1:NROW(statestats2) # they had been named based on combo of ST and PCTILE
  
  # sort cols as sorted in names_d_subgroups_both
  subvars <- intersect(names_d_subgroups_both, names(statestats2) )
  if (length(subvars) > 0) {
    othervars <- setdiff(names(statestats2), subvars)
    statestats2 <- statestats2[ , c(othervars, subvars)]
  }
 
  statestats <- statestats2
  # done with state file
 rm(statestats2)
  ################################################ #
  ################################################ #
  rm(morecols, 
     updated_usastates, updated_statestates,
     subvars, statestats_subgroups, zerorowperstate, othervars)
  ########################################################## # 
  
  # 
  # attributes(  usastats)[!("row.names" == names(attributes(usastats)))] 
  # attributes(statestats)[!("row.names" == names(attributes(statestats)))] 
  ########################################################## # 
  
  # fix duplicate name where hisp was in alone and nh versions
  usastats$pcthisp.1 <- NULL
  statestats$pcthisp.1 <- NULL
  usastats$OBJECTID <- NULL
  statestats$OBJECTID <- NULL
  
  
  if (max(usastats[, unique(c(names_d_subgroups_alone, names_d_subgroups_nh))]) > 1) {
    # fix scaling of percentages of new groups:
    usastats[, unique(c(names_d_subgroups_alone, names_d_subgroups_nh))] <-   usastats[, unique(c( names_d_subgroups_alone, 
                                                                                                   names_d_subgroups_nh))]  / 100
    statestats[, unique(c(names_d_subgroups_alone,  names_d_subgroups_nh))] <- statestats[, unique(c( names_d_subgroups_alone, 
                                                                                                      names_d_subgroups_nh))]  / 100
    rm(bg)
  }
  
  # save.image(file = 'work on usastats and statestats 2024-07.rda')
  
  ################################################ #
  
  # stop("next, create avg.in.us (before removing usastats from memory) ")
  ####### next, create avg.in.us (before removing usastats from memory)
  
 #  "EJAM/data-raw/datacreate_avg.in.us.R"

  
  # usastats_statestats  <- datacreate_usastats2.32()
  # usastats_statestats  <- datacreate_usastats2.32_add_dsubgroups.R(usastats_statestats)
  
  # usastats   <- usastats_statestats$usastats
  # statestats <- usastats_statestats$statestats
  
  # metadata and use_data ####
  
  setDF(usastats)    #   do we want it as data.frame??
  setDF(statestats)  #   do we want it as data.frame??
  
  usastats   <- metadata_add(usastats)
  statestats <- metadata_add(statestats)
  
  usethis::use_data(usastats,   overwrite = T)
  usethis::use_data(statestats, overwrite = T)
  ################################################ #
  
  #  UPDATE THE DOCUMENTATION ####
  
  dataset_documenter("usastats",
                     "usastats (DATA) data.frame of 100 percentiles and means",
                     "data.frame of 100 percentiles and means (about 100 rows)
#'   in the USA overall, across all locations (e.g., block groups in [blockgroupstats])
#'   for a set of indicators such as percent low income.
#'   Each column is one indicator (or specifies the percentile).
#'   
#'   This should be similar to the lookup tables in the gdb on the FTP site of EJScreen,
#'   except it also has data for the demographic race/ethnicity subgroups.
#'   
#'   For details on how the table was made, see source package files
#'    EJAM/data-raw/datacreate_usastats2.32_add_dsubgroups.R and
#'    EJAM/data-raw/datacreate_usastats2.32.R
#'   
#'   See also [statestats]")
  
  dataset_documenter("statestats",
                     "statestats (DATA) data.frame of 100 percentiles and means for each US State and PR and DC.",
                     "data.frame of 100 percentiles and means 
#'   for each US State and PR and DC
#'   for all the block groups in that zone (e.g., block groups in [blockgroupstats])
#'   for a set of indicators such as percent low income.
#'   Each column is one indicator (or specifies the percentile).
#'   
#'   This should be similar to the lookup tables in the gdb on the FTP site of EJScreen,
#'   except it also has data for the demographic race/ethnicity subgroups.
#'   See also [usastats] for more details.")
  
  
  print('now need to rebuild EJAM package with those new datasets and push changes')
  
  cat("FINISHED A SCRIPT\n")
  cat("\n In globalenv() so far: \n\n")
  print(ls())
  
  ################################################ #
# }

