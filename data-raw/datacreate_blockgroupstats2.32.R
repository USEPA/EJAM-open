
# SCRIPT TO DOWNLOAD/UPDATE EJSCREEN BLOCKGROUP DATA AND PERCENTILE LOOKUP TABLES FOR EJAM YEARLY

if (!exists("askquestions")) {askquestions <- FALSE}
if (!exists("rawdir")) {rawdir <- './data-raw'}
if (!exists("localfolder")) { localfolder = "~/../Downloads/ejscreen new ftp downloads"}
rm(td) # start fresh with new tempdir
if (!exists("td")) {td <- tempdir() }
if (!dir.exists(td)) {stop("tempdir() called td does not exist")}
############################################################################################ #
# EJScreen initial v 2.3 ftp site files as of 7/5/2024 had problems with ozone (and possibly ties-handling in percentile lookups)
# and team posted version 2.32 on 8/12/24 here:
baseurl = "https://gaftp.epa.gov/EJScreen/2024/2.32_August_UseMe"
# browseURL(baseurl)
needgdb = FALSE
# require(arrow)

#         as of 8/21/24 2.32 August version filenames on ftp site:

# https://gaftp.epa.gov/EJScreen/2024/2.32_August_UseMe/EJSCREEN_2024_BG_with_AS_CNMI_GU_VI.csv.zip   # CAPS
# https://gaftp.epa.gov/EJScreen/2024/2.32_August_UseMe/EJSCREEN_2024_BG_StatePct_with_AS_CNMI_GU_VI.csv.zip # CAPS

# https://gaftp.epa.gov/EJScreen/2024/2.32_August_UseMe/EJScreen_2024_BG_with_AS_CNMI_GU_VI.gdb.zip
# https://gaftp.epa.gov/EJScreen/2024/2.32_August_UseMe/EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.gdb.zip

# https://gaftp.epa.gov/EJScreen/2024/2.32_August_UseMe/EJScreen_2024_BG_National_Lookup.csv
# https://gaftp.epa.gov/EJScreen/2024/2.32_August_UseMe/EJScreen_2024_BG_State_Lookup.csv

# https://gaftp.epa.gov/EJScreen/2024/2.32_August_UseMe/EJScreen_2024_BG_Percentiles_Columns.xlsx
# https://gaftp.epa.gov/EJScreen/2024/2.32_August_UseMe/EJScreen_2024_BG_State_Percentiles_Columns.xlsx

#   data for   blockgroupstats   

blockgroupstats_source_usa.zip   <- "EJSCREEN_2024_BG_with_AS_CNMI_GU_VI.csv.zip"  # capitalization changed from prior  version
blockgroupstats_source_usa.csv   <- "EJScreen_2024_BG_with_AS_CNMI_GU_VI.csv"   # inside .zip not for download - it will be available after unzip

blockgroupstats_source_state.zip <- "EJSCREEN_2024_BG_StatePct_with_AS_CNMI_GU_VI.csv.zip"  # capitalization changed from prior
blockgroupstats_source_state.csv <- "EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.csv"  # inside .zip

blockgroupstats_source_usa.gdb.zip   <- "EJScreen_2024_BG_with_AS_CNMI_GU_VI.gdb.zip"           # gdb is not essential
blockgroupstats_source_usa.gdb       <- "EJScreen_2024_BG_with_AS_CNMI_GU_VI.gdb"           # inside .zip
blockgroupstats_source_state.gdb.zip <- "EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.gdb.zip"  # gdb is not essential
blockgroupstats_source_state.gdb     <- "EJScreen_2024_BG_StatePct_with_AS_CNMI_GU_VI.gdb" # inside .zip

#   data for   usastats and statestats

usastats_source.csv              <- "EJScreen_2024_BG_National_Lookup.csv"
statestats_source.csv            <- "EJScreen_2024_BG_State_Lookup.csv"

usastats_new_explained.xlsx      <- "EJScreen_2024_BG_Percentiles_Columns.xlsx"
statestats_new_explained.xlsx    <- "EJScreen_2024_BG_State_Percentiles_Columns.xlsx"

## some are .zip some .xlsx some .csv and some are found inside the .zip files

############################################################################################ #

if (!dir.exists(localfolder)) {
  if (interactive() && askquestions) {
    localfolder <- rstudioapi::selectDirectory("Save local copies where?")
    if (is.na(localfolder) || !dir.exists(localfolder)) {stop(localfolder, " folder does not exist")}
  } else {
    localfolder <- getwd() 
  }
}
cat("Using this FTP folder as source of data: ", baseurl, "\n")
cat("Using this folder as local folder to save copies in: ", localfolder, "\n")

#   # if this were a function not a script  ... 
# x = function(localfolder = "~/../Downloads/ejscreen new ftp downloads", 
#              baseurl = "https://gaftp.epa.gov/EJScreen/2024/2.32_August_UseMe"
# ) {
#   if (!dir.exists(localfolder)) {dir.create(localfolder)}
# }
############################################################################################ #
## older scripts:
# /dev/notes_datasets
# [1] "0_SCRIPT_overview_get_ejscreendata.R"             "1_SCRIPT_EJAMejscreen_download.R"                
# [3] "2_SCRIPT_FOR_FIPS_ST_TRACT_CNTY.R"                "3_SCRIPT_create_bgDemog_ejscreen2.1_andtracts.R" 
# [5] "4_SCRIPT_ADD_PUERTORICO_DEMOG_SUBGROUPS.R"        "5_SCRIPT_merge_demogsubgroups_v2.1.R"            
# [7] "6_SCRIPT_create_blockgroupstats.R"                "8_SCRIPT_make_MeansByGroup_and_Ratios_RRS.US22.R"
# [9] "9_SCRIPT_PCTILELOOKUPS_READ-CSVS-MID-2022.R"      "NOTES_which_states_are_in_which_datasets.R"      
# [11] "PINSURLTRY.R"     
############################################################################################ #

# DOWNLOAD ZIP and CSV ####

fnames <- c(
  blockgroupstats_source_usa.zip,
  blockgroupstats_source_state.zip, # we need this only for its state versions of EJ indexes (used to create state pctiles)
  usastats_source.csv,
  statestats_source.csv,
  usastats_new_explained.xlsx,
  statestats_new_explained.xlsx
  # and the other  .csv files are found inside zip files after download
)
options(timeout = max(300, getOption("timeout"))) # default is 60 seconds 
zurls = (  file.path(baseurl, fnames) )
print(cbind(zurls))

# are they all there as expected??? 
# browseURL(baseurl)

curl::multi_download(urls = zurls, 
                     destfiles = file.path(td, fnames))
########################################################### #

## if gdb needed:
if (needgdb) {
  curl::multi_download(
    urls = file.path(baseurl, c(blockgroupstats_source_usa.gdb.zip, blockgroupstats_source_state.gdb.zip)),
    destfiles = file.path(td, c(blockgroupstats_source_usa.gdb.zip, blockgroupstats_source_state.gdb.zip))
  )
  unzip(          file.path(td, blockgroupstats_source_usa.gdb.zip),    exdir =  td, overwrite = TRUE)
  unzip(          file.path(td, blockgroupstats_source_state.gdb.zip),  exdir =  td, overwrite = TRUE)
  gdbpath_usa   = file.path(td, blockgroupstats_source_usa.gdb)
  gdbpath_state = file.path(td, blockgroupstats_source_state.gdb)
  file.copy(gdbpath_usa,   file.path(mydir, blockgroupstats_source_usa.gdb))
  file.copy(gdbpath_state, file.path(mydir, blockgroupstats_source_state.gdb))
  gdbpath_usa   = file.path(mydir, blockgroupstats_source_usa.gdb)
  gdbpath_state = file.path(mydir, blockgroupstats_source_state.gdb)
  cat("Downloads: ", gdbpath_usa,   " -- saved in tempdir successfully? ", file.exists(gdbpath_usa),   "\n")
  cat("Downloads: ", gdbpath_state, " -- saved in tempdir successfully? ", file.exists(gdbpath_state), "\n")
}
######################################################### #

# ARCHIVE  .xlsx files in case needed ####
savex = FALSE
if (interactive() && askquestions) {
  savex = askYesNo("Save usastats_new_explained.xlsx in localfolder?")
  if (!is.na(savex) && savex) {
    file.copy(file.path(td, usastats_new_explained.xlsx),   file.path(localfolder, usastats_new_explained.xlsx), overwrite = TRUE)
    file.copy(file.path(td, statestats_new_explained.xlsx), file.path(localfolder, statestats_new_explained.xlsx), overwrite = TRUE)
  }
}
########################################################### #

# UNZIP files ####

znames = fnames[ tools::file_ext(fnames) == "zip"]
pth = 1:length(znames)
for (i in 1:length(znames)) {
  pth[i] =  unzip(file.path(td, znames[i]),  exdir =  td, overwrite = TRUE)
}
# print(pth)
# print(basename(pth))
## check
# setequal(basename(pth), c(blockgroupstats_source_usa.csv, blockgroupstats_source_state.csv))
## TRUE 7/24
########################################################### #

# READ CSV FILES ####

getfile <- function(fname, folder = td) {as.data.frame(readr::read_csv(file.path(folder, fname)))}

blockgroupstats_new       <- getfile(blockgroupstats_source_usa.csv, td)
blockgroupstats_new_state <- getfile(blockgroupstats_source_state.csv, td) 

usastats_new   <- getfile(usastats_source.csv, td)
statestats_new <- getfile(statestats_source.csv, td)

rm(blockgroupstats_source_usa.zip,   blockgroupstats_source_usa.csv,
   blockgroupstats_source_state.zip, blockgroupstats_source_state.csv)
rm(usastats_source.csv, statestats_source.csv)
rm(pth, i, getfile, znames, fnames, baseurl, td)
gc()
# all that takes roughly 1 minute

print(ls())

########################################################### #
# >  blockgroupstats_new ####
# > usastats_new, statestats_new ####

# archive unaltered versions (just for convenience, in case needed, to avoid downloading again)
# Later they will get saved for the package as data, or maybe put in pins board

# save.image(file = file.path(localfolder, "save.image just after ftp downloads original colnames.rda"))
# load(file = file.path(localfolder, "save.image just after ftp downloads original colnames.rda"))

varnames <- c("blockgroupstats_new", 
              "blockgroupstats_new_state",
              "usastats_new",
              "statestats_new")
########################### #
SAVELOCAL <- TRUE
if (interactive() && askquestions) {
  SAVELOCAL = askYesNo("Save copies locally now?")
  if (is.na(SAVELOCAL)) {SAVELOCAL <- FALSE}
}
if (SAVELOCAL) {
  
  if (interactive() && askquestions) {
    ASARROW = askYesNo("Save copies as .arrow ? (not .rda)", default = TRUE)
    if (is.na(ASARROW)) {ASARROW <- FALSE}
  } else {
    ASARROW <- TRUE
  }
  if (ASARROW) {
    fnames = paste0(paste0(varnames,  "_as_on_ftp"), ".arrow")
    datawrite_to_local(varnames = varnames, fnames = fnames, localfolder = localfolder, overwrite = TRUE)
    for (i in seq_along(fnames)) {
      cat(file.path(localfolder, fnames[i]), " saved: "); cat(file.exists(file.path(localfolder, fnames[i]))); cat("\n")
    }
    # file.exists(file.path(localfolder,"blockgroupstats_new_as_on_ftp.arrow"))
    # file.exists(file.path(localfolder,"usastats_new_as_on_ftp.arrow"))
    # file.exists(file.path(localfolder,"statestats_new_as_on_ftp.arrow"))
  } else {
    ## to save as .rda files
    # ext <- ".rda"
    fnames = paste0(paste0(varnames,  "_as_on_ftp"), ".rda")
    save(blockgroupstats_new,       file = file.path(localfolder, "blockgroupstats_new_as_on_ftp.rda"))  # about 118 MB on disk
    save(blockgroupstats_new_state, file = file.path(localfolder, "blockgroupstats_new_state_as_on_ftp.rda"))
    save(usastats_new,              file = file.path(localfolder, "usastats_new_as_on_ftp.rda"))
    save(statestats_new,            file = file.path(localfolder, "statestats_new_as_on_ftp.rda"))
    for (i in seq_along(fnames)) {
      cat(file.path(localfolder, fnames[i]), " saved: "); cat(file.exists(file.path(localfolder, fnames[i]))); cat("\n")
    }
    # file.exists(file.path(localfolder,"blockgroupstats_new.rda"))
    # file.exists(file.path(localfolder,"usastats_new.arrow"))
    # file.exists(file.path(localfolder,"statestats_new.arrow"))
  }
  rm(varnames,   ASARROW, i)
  browseURL(localfolder)
  # if (!silent) {
  cat("\n So far in globalenv() are these: \n\n")
  print(ls())
  # }
}
########################################################### #
########################################################### #

# load(file.path(localfolder, "blockgroupstats_new_as_on_ftp.rda"))
# load(file.path(localfolder, "blockgroupstats_new_state_as_on_ftp.rda"))
if (!exists('blockgroupstats_new_as_on_ftp') && file.exists(file.path(localfolder, "blockgroupstats_new_as_on_ftp.arrow"))) {
  blockgroupstats_new_as_on_ftp <- arrow::read_ipc_file(file.path(localfolder, "blockgroupstats_new_as_on_ftp.arrow"))
}

########################################################### #

## rename colnames ####

############################ #
# 1st, check  map_headernames info  
EJAM:::nacounts(blockgroupstats_new)
cbind(sort(names(blockgroupstats_new)[fixcolnames(names(blockgroupstats_new),'csv','r') == names(blockgroupstats_new)]))
#         bins (colors on map)
# [1,] "B_D2_DWATER"    
# [2,] "B_D2_NO2"       
# [3,] "B_D5_DWATER"    
# [4,] "B_D5_NO2"       
# [5,] "B_DISABILITYPCT"
# [6,] "B_DWATER"       
# [7,] "B_NO2" 
# etc etc
# [8,] "REGION"         
# [9,] "Shape_Length"   
#           text for popups in ejscreen app
# [10,] "T_D2_DWATER"    
# [11,] "T_D2_NO2"       
# [12,] "T_D5_DWATER"    
# [13,] "T_D5_NO2"       
# [14,] "T_DISABILITYPCT"
# [15,] "T_DWATER"       
# [16,] "T_NO2"      
#   etc etc
############################ #

# now rename columns

names(blockgroupstats_new)       <-  fixcolnames(names(blockgroupstats_new),       oldtype = 'csv', newtype = 'r') # 
names(blockgroupstats_new_state) <-  fixcolnames(names(blockgroupstats_new_state), oldtype = 'csv', newtype = 'r') # 

names(usastats_new)    <- fixcolnames(names(usastats_new),   oldtype = "csv", newtype = "r")
names(statestats_new)  <- fixcolnames(names(statestats_new), oldtype = "csv", newtype = "r")

## drop columns not needed by EJAM ####

##  drop all the map bin and text popup columns
##  drop all the percentile columns, since we lookup those using an aggregated (wtdavg) raw indicator value for each location analyzed.
# grep("^pctile|state.pctile|^bin", names(EJAM::blockgroupstats), value = TRUE) # those are not in blockgroupstats, not needed
cols2drop <- grep("^pctile|state.pctile|^bin", names(blockgroupstats_new), value = TRUE)
# extra ones not renamed by fixcolnames() since did not bother to put all Text and Bin columns names in map_headernames
cols2drop <- c(cols2drop,
               grep("^T_|^B_", names(blockgroupstats_new), value = TRUE)
               
               # c("B_D2_DWATER", "B_D2_NO2", "B_D5_DWATER", "B_D5_NO2",  "B_DISABILITYPCT", "B_DWATER", "B_NO2", 
               #              "T_D2_DWATER", "T_D2_NO2",  "T_D5_DWATER", "T_D5_NO2", "T_DISABILITYPCT", "T_DWATER", "T_NO2")
)
cat("\n Dropping these columns: \n\n")
print(cbind(cols2drop))

blockgroupstats_new       <- blockgroupstats_new[,       !(names(blockgroupstats_new)       %in% cols2drop)]
blockgroupstats_new_state <- blockgroupstats_new_state[, !(names(blockgroupstats_new_state) %in% cols2drop)]

usastats_new              <- usastats_new[,              !(names(usastats_new)              %in% cols2drop)]
statestats_new            <- statestats_new[,            !(names(statestats_new)            %in% cols2drop)]
rm(cols2drop)
# dim(usastats_new);   dim(EJAM::usastats)
# dim(statestats_new); dim(EJAM::statestats)

dim(blockgroupstats_new); dim(EJAM::blockgroupstats)
# [1] 243022     81 as of 8/13/24 and 8/21/24
# [1] 243021    112  v2.2
dim(usastats_new);   dim(EJAM::usastats)
# [1] 103  52  as of 8/13/24
# [1] 102 rows in v2.2
dim(statestats_new); dim(EJAM::statestats)
# [1] 5356   52  # for now
# [1] 5304 rows in v2.2

setdiff(          names(blockgroupstats_new), names(EJAM::blockgroupstats))
# EJAM:::setdiff_yx(names(blockgroupstats_new), names(EJAM::blockgroupstats))

# names(blockgroupstats_new)
# names(usastats_new)

############################################################## #

## archive island areas ####
## but drop them from blockgroupstats_new and blockgroupstats_new_state

names(blockgroupstats_new)       <- gsub("id", "OBJECTID", names(blockgroupstats_new))
names(blockgroupstats_new_state) <- gsub("id", "OBJECTID", names(blockgroupstats_new_state))

bg_islandareas <- blockgroupstats_new[                      !EJAM:::fips_valid(blockgroupstats_new$OBJECTID),
                                                            EJAM:::nacounts(
                                                              blockgroupstats_new[!EJAM:::fips_valid(blockgroupstats_new$OBJECTID), ],
                                                              showall = T)$other > 0]
library(arrow)
save(          bg_islandareas, file = file.path(localfolder, "bg_islandareas.rda"  ))
write_ipc_file(bg_islandareas, sink = file.path(localfolder, "bg_islandareas.arrow"))
#  ISLAND AREAS ARE IN blockgroupstats_new but HAVE NO DATA! (except FIPS, ST, etc.)
#  and are not at all in bgpts, block tables, etc.
# fips2state_abbrev(unique(substr(blockid2fips$blockfips,1,2)))  -- only DC,PR,states
# fips2state_abbrev(unique(substr(blockgroupstats_new$OBJECTID ,1,2))) -- also had AS, GU, MP, VI
#  No EJScreen reports or maps are available in the Island Areas.
# (we have data and reports in all States and PR and DC, but not GU AS VI MP).
## Therefore we should just drop the Island Areas from blockgroupstats_new etc. tables in EJAM for now.
## 
# nacounts = EJAM:::nacounts
# > length(unique(blockgroupstats_new$OBJECTID))
# [1] 243022
# > NROW(blockgroupstats_new )
# [1] 243022
# nacounts(blockgroupstats_new$OBJECTID)
# nacounts(blockgroupstats_new[,1:20]) # so far OBJECTID and ST are not NA here at all
## but current fips_valid() fails to recognize as valid 686 of the fips currently
## because it relied on bgpts which was based on the weights table 
# table(EJAM:::fips_valid(blockgroupstats_new$OBJECTID))
# FALSE   TRUE 
#   686 242336
#
### THAT IS BECAUSE ISLAND AREAS HAVE FIPS OF NONSTANDARD NUMBER OF DIGITS:
# > table(nchar(blockgroupstats_new$OBJECTID))
#   7     10     12 
# 270    416 242336   The 686 cases are FIPS of 7 or 10 digits.
# > unique(blockgroupstats_new$ST[nchar(blockgroupstats_new$OBJECTID) == 10])
# [1] "VI"
# > unique(blockgroupstats_new$ST[nchar(blockgroupstats_new$OBJECTID) == 7])
# [1] "AS" "GU" "MP"

# head(nacounts(blockgroupstats_new[blockgroupstats_new$ST %in% 'VI', ], showall = T))
## nas other
## OBJECTID     0   416
## statename    0   416
## ST           0   416
## countyname   0   416
## REGION       0   416
## pop        416     0
# head(nacounts(blockgroupstats_new[blockgroupstats_new$ST %in% 'AS', ], showall = T))
# head(nacounts(blockgroupstats_new[blockgroupstats_new$ST %in% 'GU', ], showall = T))
# head(nacounts(blockgroupstats_new[blockgroupstats_new$ST %in% 'MP', ], showall = T))
# head(nacounts(blockgroupstats_new[blockgroupstats_new$ST %in%   'DE', ], showall = T))

################################################### # 

## drop island areas  ####
##   AS, GU, MP, VI are in the new blockgroupstats table but bgid will be set to NA for those if they 
## are not found in bgpts. 

# table(blockgroupstats_new$ST)
blockgroupstats_new       <- blockgroupstats_new[!(blockgroupstats_new$ST %in% c('AS', 'GU', 'MP', 'VI')), ]
blockgroupstats_new_state <- blockgroupstats_new_state[!(blockgroupstats_new_state$ST %in% c('AS', 'GU', 'MP', 'VI')), ]
blockgroupstats_new_state <- blockgroupstats_new_state[!is.na(blockgroupstats_new_state$ST),]

# AS OF 8/14/24 THERE ARE STILL 19 Connecticut blockgroups FIPS in bgfips (derived from the block weights file provided directly by the EJScreen team)
#  that are NOT in blockgroupstats (derived from the geodatabase on the FTP site)
# ***

dim(blockgroupstats_new)         # 242,336  (missing 19 blockgroups compared to bgpts)   ***
dim(blockgroupstats_new_state)   # 242,336  (missing 19 blockgroups compared to bgpts)
dim(bgpts)                       # 242,355 rows at this point it had  

################################################# # 

## create bgfips and bgid columns ####

blockgroupstats_new$bgfips       <- fips_lead_zero(blockgroupstats_new$OBJECTID)
blockgroupstats_new_state$bgfips <- fips_lead_zero(blockgroupstats_new$OBJECTID)

#  table(EJAM:::fips_valid(blockgroupstats_new$bgfips))

blockgroupstats_new$OBJECTID <- NULL
blockgroupstats_new_state$OBJECTID <- NULL


# table(fips2state_abbrev(  bgpts[bgfips %in% (setdiff(bgpts$bgfips, blockgroupstats_new$bgfips)), substr(bgfips,1,2)]))
# *** 19 bg in CT are somehow missing in the new blockgroupstats_new table but were in the supposedly correct bgpts$bgfips...
# that was made from the july 2024 weights table  ? 

# mapfast(bgpts[bgfips %in% (setdiff(bgpts$bgfips, blockgroupstats_new$bgfips)),])

################################################# # 

#### THIS REQUIRES USING THE UPDATING THE bgpts data table first!                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          PDATED/LATEST VERSION OF bgpts and bgid2fips ! 
#### e.g., from v2.2 to v2.32, FIPS changed in Connecticut!
# e.g., the v2.32 blockgroupstats$bgfips had about 2,717 new bgfips that failed to match any old v2.2 bgpts$bgfips.

# dataload_from_local('bgid2fips')
# > attributes(bgid2fips)$ejscreen_version
# [1] "2.1"
# > attributes(bgpts)$ejscreen_version
# NULL
dataload_from_pins('bgid2fips')
if (!all(blockgroupstats_new$bgfips %in% bgpts$bgfips))     {stop("not all new bgfips can be found in the version of bgpts available/attached")}
if (!all(blockgroupstats_new$bgfips %in% bgid2fips$bgfips)) {stop("not all new bgfips can be found in the version of bgid2fips available/attached")}

# table( blockgroupstats_new[blockgroupstats_new$bgfips %in% (EJAM:::setdiff_yx(bgpts$bgfips, blockgroupstats_new$bgfips)), "ST"])
#  These used to be there but are gone now: 
# AS  GU  MP  VI 
# 77  58 135 416   # 686 total Island Area  bgfips were in _new but NOT in the   bgpts$bgfips...




blockgroupstats_new$bgid       <- bgpts$bgid[match(blockgroupstats_new$bgfips,       bgpts$bgfips)]
blockgroupstats_new_state$bgid <- bgpts$bgid[match(blockgroupstats_new_state$bgfips, bgpts$bgfips)]
# table(is.na(blockgroupstats_new$bgfips))

nacounts(blockgroupstats_new)
#  
#                                     nas  other  # 8/21/24
# pctunemployed                       424 241912
# lowlifex                          17406 224930
# pm                                 4142 238194
# o3                                 4142 238194
# dpm                                1898 240438
# rsei                                644 241692
# traffic.score                      1393 240943
# proximity.npl                      1393 240943
# proximity.rmp                      1393 240943
# proximity.tsdf                     1393 240943
# ust                                 560 241776
# proximity.npdes                    1897 240439
# no2                                 632 241704
# drinking                          19208 223128
# EJ.DISPARITY.pm.eo                 4142 238194
# EJ.DISPARITY.pm.supp               4142 238194
# EJ.DISPARITY.o3.eo                 4142 238194
# EJ.DISPARITY.o3.supp               4142 238194
# EJ.DISPARITY.dpm.eo                1898 240438
# EJ.DISPARITY.dpm.supp              1898 240438
# EJ.DISPARITY.rsei.eo                644 241692
# EJ.DISPARITY.rsei.supp              644 241692
# EJ.DISPARITY.traffic.score.eo      1393 240943
# EJ.DISPARITY.traffic.score.supp    1393 240943
# EJ.DISPARITY.proximity.npl.eo      1393 240943
# EJ.DISPARITY.proximity.npl.supp    1393 240943
# EJ.DISPARITY.proximity.rmp.eo      1393 240943
# EJ.DISPARITY.proximity.rmp.supp    1393 240943
# EJ.DISPARITY.proximity.tsdf.eo     1393 240943
# EJ.DISPARITY.proximity.tsdf.supp   1393 240943
# EJ.DISPARITY.ust.eo                 560 241776
# EJ.DISPARITY.ust.supp               560 241776
# EJ.DISPARITY.proximity.npdes.eo    1897 240439
# EJ.DISPARITY.proximity.npdes.supp  1897 240439
# EJ.DISPARITY.no2.eo                 632 241704
# EJ.DISPARITY.no2.supp               632 241704
# EJ.DISPARITY.drinking.eo          19208 223128
# EJ.DISPARITY.drinking.supp        19208 223128
################################################################################ #
########################################################### #
################################################################################ #

# > bgej - Create bgej here.####

###### MOVE EJ INDEXES FROM blockgroupstats_new and blockgroupstats_new_state
## to  a consolidated bgej  table 

## merge US EJ with the state.EJ.DISPARITY columns ####
# from the  blockgroupstats_new_state  table

# dataload_from_pins("bgej")
# > names( bgej) 

blockgroupstats_new_state <- blockgroupstats_new_state[, c("bgid", "bgfips", names_ej, names_ej_supp)]
data.table::setDT(blockgroupstats_new_state)
data.table::setDT(blockgroupstats_new)
data.table::setnames(blockgroupstats_new_state,
                     old =  c("bgid", "bgfips", names_ej, names_ej_supp), 
                     new =  c("bgid", "bgfips", c(names_ej_state, names_ej_supp_state))
)
# > all.equal(blockgroupstats_new$bgid, blockgroupstats_new_state$bgid)
# [1] TRUE
data.table::setDF(blockgroupstats_new)
data.table::setDF(blockgroupstats_new_state)

bgej <- data.table(
  blockgroupstats_new[ , c("bgid", "bgfips", 
                           "ST", "pop", 
                           names_ej, 
                           names_ej_supp)],
  blockgroupstats_new_state[, c(names_ej_state, 
                                names_ej_supp_state)]
)
# all.equal(data.frame(bgej)[, names_ej], blockgroupstats_new[,names_ej])

## metadata_add ####
bgej = metadata_add(bgej)

## do not save via  usethis::use_data(bgej, overwrite = TRUE) - it is a large file
cat("bgej created in globalenv but not saved yet - will try to save to pins board in later script... \n")

## documentation for bgej ####

dataset_documenter("bgej", 
                   title = "bgej (DATA) EJScreen EJ Indexes for Census block groups",
                   description = "bgej is a table of all blockgroups, with the raw scores of the EJ Indexes
#'   and supplemental EJ Indexes for all the environmental indicators.",
                   details = "This file is not stored in the package, but is obtained via [dataload_from_pins()].
#'   
#'   For documentation on the demographic and environmental data and indicators,
#'   see [EJScreen documentation](https://www.epa.gov/ejscreen/understanding-ejscreen-results).
#'   
#'   See 
#'     
#'     dataload_from_pins('bgej')
#'     
#'     names(bgej)
#'   
#'   The column names are these:
#'   
#'     c('bgfips', 'bgid', 'ST', 'pop', 
#'     names_ej, 
#'     names_ej_supp, 
#'     names_ej_state,
#'     names_ej_supp_state
#'     )",
                   saveinpackage = FALSE)



rm(blockgroupstats_new_state)
blockgroupstats_new[, c(names_ej, names_ej_supp)] <- NULL
setDT(blockgroupstats_new)
setcolorder(blockgroupstats_new, c("bgid", "bgfips", "statename", "ST", "countyname", "REGION",
                                   "pop",
                                   names_d, names_e), before = 1)



########################################################## # 
SAVELOCAL <- TRUE
if (interactive() && askquestions) {
  SAVELOCAL = askYesNo("Save bgej locally now?")
  if (is.na(SAVELOCAL)) {SAVELOCAL <- FALSE}
}
if (SAVELOCAL) {
  
  ## save bgej local copy for convenience ####
  datawrite_to_local("bgej", localfolder = localfolder)
  
  # ASARROW = TRUE
  # if (ASARROW) {
  #   fnames = paste0(paste0(varnames,  "_as_on_ftp"), ".arrow")
  #   datawrite_to_local(varnames = varnames, fnames = fnames, localfolder = localfolder, overwrite = TRUE)
  #   for (i in seq_along(fnames)) {
  #     cat(file.path(localfolder, fnames[i]), " saved: "); cat(file.exists(file.path(localfolder, fnames[i]))); cat("\n")
  #   }
  #   # file.exists(file.path(localfolder,"blockgroupstats_new_as_on_ftp.arrow"))
  #   # file.exists(file.path(localfolder,"usastats_new_as_on_ftp.arrow"))
  #   # file.exists(file.path(localfolder,"statestats_new_as_on_ftp.arrow"))
  # }  
  rm(   ASARROW)
  rm(i)
}
########################### #
rm(fnames)
## blockgroupstats_new_state is no longer needed but
## blockgroupstats_new is still needed for the next script

## bgej is left in globalenv by this script -
# later can Save bgej to pins board as .arrow file
#   
cat("FINISHED A SCRIPT\n")
cat("\n In globalenv() so far: \n\n")
print(ls())

########################################################### ############################################################ #

### (next will do script in
#   "EJAM/data-raw/datacreate_blockgroupstats2.32_add_d_acs22columns.R" )
