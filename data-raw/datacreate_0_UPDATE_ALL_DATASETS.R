############################################################### #
## Scripts to update / create latest versions of datasets 
# - ANNUAL blockgroup data from ACS and EJScreen
# - NON-ANNUAL (frequent, episodic, etc.) other datasets
# also see EJAM pkg github issues about this.

# SETUP ####

rm(list = ls())

#localfolder <- "~/../Downloads/ejscreen new ftp downloads"
localfolder  <- "~/../Downloads/EJAMbigfiles"
if (interactive()) {localfolder <- rstudioapi::selectDirectory("Confirm where to archive .arrow and other files locally", path = localfolder) }
if (!dir.exists(localfolder)) {stop(paste0("need valid localfolder - ", localfolder, " was not found"))}
if (!exists("td")) {td <- tempdir() }
if (!exists("rawdir")) {rawdir <- './data-raw'}
if (!dir.exists(rawdir)) {stop("need to do this from source package folder, from where it can find a folder at ", rawdir)}
if (!exists("askquestions")) {askquestions <- FALSE}
if (interactive()) {
  askquestions <- askYesNo("Do you want to answer questions interactively like this about what to save where, etc.? (vs running all scripts without pauses)")
  if (is.na("askquestions")) {askquestions <- FALSE}
}
######### #
consoleclear <- function() {
  if (interactive() & rstudioapi::isAvailable()) {rstudioapi::executeCommand("consoleClear")}}
loadall <- function() {
  cat("\nReloading from source so that the updated datasets will get lazyloaded instead of previously loaded or installed versions...\n\n")
  devtools::load_all()}
rmost2 <- function(notremove = c(
  c("askquestions", "localfolder", "td", "rawdir", 
    "source_maybe", "consoleclear" ,  "reload", "rmost2", "loadall"),
  "blockwts", "blockpoints", "blockid2fips", "quaddata", "localtree",
  "bgej", "bgid2fips",
  "frs", "frs_by_programid", "frs_by_naics", "frs_by_sic", "frs_by_mact"
)) {rmost(notremove = notremove)}
######################################### #
source_maybe <- function(scriptname = NULL,
                         DOIT = TRUE, 
                         question = paste0("Do ", scriptname, "?"),
                         folder = NULL) {
  if (is.null(scriptname)) {stop("requires scriptname")}
  if (missing(folder)) {
    if ( exists("rawdir")) {folder <- rawdir}
    if (!exists("rawdir")) {folder <- "./data-raw"}
  }
  if (!exists('askquestions')) {askquestions <- TRUE}
  if (askquestions && interactive()) {
    DOIT <- utils::askYesNo(question)
    if (!is.na(DOIT) && DOIT) {DOIT <- TRUE}
  }
  if (DOIT) {
    spath <- file.path(folder, scriptname)
    if (!file.exists(spath)) {stop(paste0("requires valid folder and scriptname. Tried: ", spath))}
    cat(paste0("Doing ", scriptname, " \n"))
    source(spath)
  } else {
    cat("Skipping ", scriptname, "\n")
  }
}
######################################### #
## DESCRIPTION / VERSION ####

desc::desc_print()
cat('Version metadata as found in DESCRIPTION file \n')
print(desc::desc_get_version())
if (askquestions && interactive()) {
  y <- askYesNo("Do you first need to update metadata (version, etc.), which is in DESCRIPTION file? ")
} else {y <- FALSE}
if (y) {
  usethis::edit_file('DESCRIPTION')
}
#   metadata_mapping() uses DESCRIPTION info and gets done via devtools::load_all() or library(EJAM)
## loadall() and requires ####
# Get latest source functions and data: 
# from  EJAM/R/*.R and EJAM/data/*.rda 
# Attaches exported + internal functions & data 
# like metadata_add(), newly saved .rda files, etc.
#  Otherwise internal functions don't work in scripts, and it would use installed not new source versions.
golem::detach_all_attached()

require(devtools)
require(rstudioapi)

loadall()

######################################### ########################################## #
#
## List of datacreate_ files ####
## & when to use each 
# fnames <- dir(rawdir, pattern = 'datacreate_')
# cat("\n \n\n", "To open & edit one of the datacreate_ files,
#     you can source a line below\n\n",
#     paste0(paste0(
#       "\t documentOpen('", rawdir, "/", fnames, "')"), collapse = "\n"))
if (0 == 1) {  # collapsable list
  ####   THESE ARE SORTED INTO GROUPS THAT GO TOGETHER : 
  x <- c("datacreate_0_UPDATE_ALL_DATASETS.R", "datacreate_0_UPDATE_ALL_DOCUMENTATION_pkgdown.R",
         "datacreate_map_headernames.R", "datacreate_names_of_indicators.R", "datacreate_names_pct_as_fraction.R", "datacreate_metadata4pins.R", 
         "datacreate_blockwts.R", "datacreate_bg_cenpop2020.R", "datacreate_bgpts.R", "datacreate_states_shapefile.R", "datacreate_stateinfo.R", "datacreate_stateinfo2.R", "datacreate_islandareas.R", "datacreate_censusplaces.R", 
         "datacreate_blockgroupstats2.32.R", "datacreate_blockgroupstats2.32_add_d_acs22columns.R",  "datacreate_blockgroupstats2.32_recalc_language.R",
         "datacreate_usastats2.32.R", "datacreate_usastats2.32_add_dsubgroups.R", "datacreate_avg.in.us.R", "datacreate_high_pctiles_tied_with_min.R", "datacreate_formulas.R", "datacreate_test_address_table.R", "datacreate_testpoints_testoutputs.R", 
         "datacreate_default_points_shown_at_startup.R", "datacreate_testpoints_5_50_500.R", "datacreate_ejscreenRESTbroker2table_na_filler.R", "datacreate_testoutput_ejscreenit_or_ejscreenapi_plus_50.R",
         "datacreate_frs_.R", "datacreate_frs_by_mact.R", "datacreate_frs_by_sic.R", "datacreate_frsprogramcodes.R", "datacreate_epa_programs.R", 
         "datacreate_epa_programs_defined.R", "datacreate_testids_program_sys_id.R", "datacreate_testids_registry_id.R", "datacreate_naics_counts.R", "datacreate_naicstable.R", "datacreate_SIC.R", "datacreate_sic_counts.R", "datacreate_sictable.R", 
         "datacreate_lat_alias.R", "datacreate_ejampackages.R", "datacreate_meters_per_mile.R"
  )
  setdiff(x, dir(rawdir, pattern = 'datacreate_') )   # confirm the organized list x is completely reflecting current actual files
  setdiff( dir(rawdir, pattern = 'datacreate_'), x )
  cat("\n \n\n", "To open & edit one of the datacreate_ files,
    you can source a line below\n\n",
      paste0(paste0(
        "\t documentOpen('", rawdir, "/", x, "')"), collapse = "\n"))
  # cbind(x)
  rm(x)
  
  ####################################### # 
  # overall
  documentOpen('./data-raw/datacreate_0_UPDATE_ALL_DATASETS.R')
  documentOpen('./data-raw/datacreate_0_UPDATE_ALL_DOCUMENTATION_pkgdown.R')
  
  # with annual census fips codes or boundaries changes (when EJScreen incorporates those)
  #
  # To create and save the datasets from within the EJAM source package root folder,
  # 
  ##  new indicators, variable names
  documentOpen('./data-raw/datacreate_map_headernames.R')       # ok
  documentOpen('./data-raw/datacreate_names_of_indicators.R')   # ok
  documentOpen('./data-raw/datacreate_names_pct_as_fraction.R') # ok
  
  #   blocks
  documentOpen('./data-raw/datacreate_blockwts.R')           # needs Island Areas added
  #    and be sure to obtain correct version either from census or directly from ejscreen team
  
  #   blockgroups
  documentOpen('./data-raw/datacreate_bg_cenpop2020.R')      # confirm if changed since 2020
  documentOpen('./data-raw/datacreate_bgpts.R')              # redundant w bg_cenpop2020, pick one to use
  #   states
  documentOpen('./data-raw/datacreate_states_shapefile.R')   # check if want 2020 or 2022+ file
  documentOpen('./data-raw/datacreate_stateinfo.R')          # ok (missing Island Areas)
  documentOpen('./data-raw/datacreate_stateinfo2.R')         # ok (has Island Areas)
  #   other geo
  documentOpen('./data-raw/datacreate_islandareas.R')        # ok
  documentOpen('./data-raw/datacreate_censusplaces.R')       # not used yet
  
  # with annual ejscreen data updates
  # 
  ##  ejscreen demog and envt data on every blockgroup
  ##  + pctile and avg lookup tables
  documentOpen("./data-raw/datacreate_metadata4pins.R") # ok
  documentOpen('./data-raw/datacreate_blockgroupstats2.32.R') # and bgej      # ok
  documentOpen('./data-raw/datacreate_blockgroupstats2.32_add_d_acs22columns.R')   # ok
  rstudioapi::documentOpen("./data-raw/datacreate_blockgroupstats2.32_recalc_language.R")
  
  documentOpen('./data-raw/datacreate_usastats2.32.R')                 # ok
  documentOpen('./data-raw/datacreate_usastats2.32_add_dsubgroups.R')  # ok
  documentOpen('./data-raw/datacreate_avg.in.us.R')                   # ok
  documentOpen('./data-raw/datacreate_high_pctiles_tied_with_min.R')  # ok
  ##  calculations and examples of outputs
  documentOpen('./data-raw/datacreate_formulas.R')                    # was in progress; maybe not used yet
  documentOpen('./data-raw/datacreate_test_address_table.R')       # ok
  documentOpen('./data-raw/datacreate_testpoints_testoutputs.R')      # confirm new datasets/functions/indicators work here
  # from the original  EJAM ejscreenapi  test data
  documentOpen('./data-raw/datacreate_default_points_shown_at_startup.R')            
  documentOpen('./data-raw/datacreate_testpoints_5_50_500.R')            
  documentOpen('./data-raw/datacreate_ejscreenRESTbroker2table_na_filler.R')         
  documentOpen('./data-raw/datacreate_testoutput_ejscreenit_or_ejscreenapi_plus_50.R')
  
  # when frs info is updated
  
  documentOpen('./data-raw/datacreate_frs_.R')            #  BUT SEE IF THIS HAS BEEN REVISED/ REPLACED  ***
  documentOpen('./data-raw/datacreate_frs_by_mact.R')     #  BUT SEE IF THIS HAS BEEN REPLACED  ***
  documentOpen('./data-raw/datacreate_frs_by_sic.R')      #  BUT SEE IF THIS HAS BEEN REPLACED  ***
  
  documentOpen('./data-raw/datacreate_frsprogramcodes.R') #
  documentOpen('./data-raw/datacreate_epa_programs.R')    #
  documentOpen('./data-raw/datacreate_testids_program_sys_id.R')  # 
  documentOpen('./data-raw/datacreate_testids_registry_id.R')     #
  # NAICS/SIC
  documentOpen('./data-raw/datacreate_naics_counts.R')    # script
  documentOpen('./data-raw/datacreate_naicstable.R')      # script. does date_saved_in_package & use_data
  documentOpen('./data-raw/datacreate_SIC.R')             
  documentOpen('./data-raw/datacreate_sic_counts.R')      
  documentOpen('./data-raw/datacreate_sictable.R')        
  
  # misc
  documentOpen('./data-raw/datacreate_lat_alias.R')
  documentOpen('./data-raw/datacreate_ejampackages.R')
  documentOpen('./data-raw/datacreate_meters_per_mile.R')
  
  ### and then datawrite_to_pins() if those datasets were updated. 
  
} # outline/list of datacreate_ files
######################################### ########################################## #
## metadata notes ####
#
## use simple metadata for data not related to EJScreen or Census, like just frs-related, naics-related, etc.
# attr(x, "date_downloaded")       <- as.character(Sys.Date()) # if relevant
# attr(x, "date_saved_in_package") <- as.character(Sys.Date())

## use full metadata if related to ejscreen or census/acs
# x <- metadata_add(x)
######################################### #
## Verify pins board access ####
x <- datawrite_to_pins(justchecking = T) # load_all() first or use EJAM:::
if (!is.null(x)) {
  cat(" Must use VPN to have access to pins board \n\n" )
  cat("\n As of", as.character(Sys.Date()), "\n\n")
  x = x[order(x$created), ]
  rownames(x) <- NULL
  print(x)  
  
  pin_seen <- x$name
  pin_expected = c(
    'blockwts', 'blockpoints', 'blockid2fips', "quaddata",
    'bgej', 'bgid2fips', # note that 'bg_cenpop2020' and 'bgpts' are in EJAM/data/ not pins
    'frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact"
  )
  if (length(setdiff2(pin_seen, pin_expected)) > 0 ) {
    message("Expected to see on pin board but not there: ", paste0(setdiff(pin_expected, pin_seen), collapse = ", "))
    message("See on on pin board but not expected: ", paste0(setdiff(pin_seen, pin_expected), collapse = ", "))
  }
  rm(pin_seen, pin_expected, x)
}
  
  # As of 2024-08-29 
  
  #                name                                        title  type file_size             created ejscreen_version varnames

  # 1               frs              frs data from EJScreen for EJAM arrow   146.01M 2024-08-05 14:42:49             2.32     TRUE
  # 2       frs_by_mact      frs_by_mact data from EJScreen for EJAM arrow     4.63M 2024-08-05 14:43:17             2.32     TRUE
  # 3        frs_by_sic       frs_by_sic data from EJScreen for EJAM arrow    20.25M 2024-08-05 14:43:21             2.32     TRUE
  # 4      frs_by_naics     frs_by_naics data from EJScreen for EJAM arrow    14.68M 2024-08-05 14:43:28             2.32     TRUE
  # 5  frs_by_programid frs_by_programid data from EJScreen for EJAM arrow    154.7M 2024-08-05 14:43:33             2.32     TRUE
  # 6         bgid2fips                      bgid2fips data for EJAM arrow     2.98M 2024-08-22 18:34:28             2.32     TRUE
  # 7      blockid2fips                   blockid2fips data for EJAM arrow    98.17M 2024-08-22 18:34:34             2.32     TRUE
  # 8       blockpoints                    blockpoints data for EJAM arrow   155.97M 2024-08-22 18:34:56             2.32     TRUE
  # 9          blockwts         blockwts data from EJScreen for EJAM arrow    68.64M 2024-08-22 18:35:34             2.32     TRUE
  # 10         quaddata                       quaddata data for EJAM arrow   218.36M 2024-08-22 18:35:52             2.32     TRUE
  # 11             bgej             bgej data from EJScreen for EJAM arrow    84.94M 2024-08-22 18:54:56             2.32     TRUE
  

######################################### ########################################## #
######################################### ########################################## #
# ~------------------------------------------- ####

# *** NAMES OF INDICATORS/ VARIABLES etc. ANNUAL UPDATES ####
######################################### #
### datacreate_map_headernames.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_map_headernames.R")
source_maybe("datacreate_map_headernames.R", DOIT = TRUE)
######################################### #
### datacreate_names_of_indicators.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_names_of_indicators.R")
source_maybe("datacreate_names_of_indicators.R")   # NOTE THAT   THIS TAKES A LONG TIME, ACTUALLY

consoleclear()
ls()
loadall()

### that will create but also assign metadata to and save for pkg via use_data() 
### It is a script that mostly uses a function so that
### all the variables created do not show up in the global environment - they get saved in pkg ready for lazy-loading if/when needed
### BUT any subsequent scripts that depend on those will not use the correct new versions unless we do load.all() anyway... 
### metadata is assigned inside this  
### use_data is done inside this  
######################################### #
### datacreate_names_pct_as_fraction.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_names_pct_as_fraction.R")
source_maybe("datacreate_names_pct_as_fraction.R")
######################################### #
### datacreate_metadata4pins.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_metadata4pins.R")
source_maybe("datacreate_metadata4pins.R") # does use_data()
# this just stores title, description of each dataset that gets put in pins board - no dates info
# dates info for pins is generated right when being written to pins board.
######################################### #
### Must use load_all() or build/install, to make available those new variable name lists 
#  and possibly modified  metadata4pins.rda
#  (the source package as just updated, not the version installed)
#  and so all functions will use the new source version 

rmost2()
loadall()

######################################### ########################################## #

# *** FIPS CODES/ Census Boundaries - ANNUAL UPDATES (if EJScreen incorporates those) ####

######################################### #
## blocks  ####
# documentOpen('./data-raw/datacreate_blockwts.R')           # needs Island Areas added

######################################### #
### datacreate_blockwts.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_blockwts.R")
### this requires package called ejanalysis/census2020download, which is not on CRAN!

# THIS TAKES A VERY LONG TIME:

dataload_from_pins('bgid2fips')

source_maybe('datacreate_blockwts.R', DOIT = FALSE) # script that can include metadata_add() and use_data()
#    and be sure to obtain correct version either from census or directly from ejscreen team
# Creates mylistoftables, a list that includes tables blockwts, blockpoints, bgid2fips, etc.,
#   gets updated when FIPS codes or boundaries change for blocks or blockgroups
#  such as in Connecticut for v2.2 change to v2.32 !
#  and then datawrite_to_pins() if those datasets were updated.
# bgej  is not ready yet here... it is made when blockgroupstats is made.  
# note that 'bg_cenpop2020' and 'bgpts' are in EJAM/data/ not pins

# take a look/ check 
length(unique(substr(blockid2fips$blockfips,1,2)))
nacounts(bgid2fips, showall = T)
nacounts(blockwts, showall = T)
nacounts(blockpoints, showall = T)
nacounts(quaddata, showall = T)
nacounts(blockid2fips, showall = T)
## check blockid values in all these datasets
stopifnot(
  all(
    setequal(blockid2fips$blockid, blockpoints$blockid), 
    setequal(blockid2fips$blockid, quaddata$blockid),
    setequal(blockid2fips$blockid, blockwts$blockid)
  ),
  all(
    !anyDuplicated(blockid2fips$blockid),
    !anyDuplicated(blockwts$blockid),
    !anyDuplicated(blockpoints$blockid),
    !anyDuplicated(quaddata$blockid)
  ),
  all(
    !anyNA(blockid2fips),
    !anyNA(blockwts),
    !anyNA(blockpoints),
    !anyNA(quaddata)
  )
)
# blockid2fips : blockid, blockfips 
# blockpoints :  blockid,             lat, lon 
# blockwts :     blockid, bgid, blockwt, block_radius_miles 
# quaddata :      blockid   and      BLOCK_X, BLOCK_Z, BLOCK_Y

### SAVE LOCALLY AND TO PINS BOARD ####

these <- c("bgid2fips",   "blockid2fips", "blockpoints", "blockwts", 'quaddata')
datawrite_to_local(these)
datawrite_to_pins(varnames = these) # it asks interactively to confirm which ones to save to pins

# ONE COULD LOAD FROM LOCAL OR PINS THE EXISTING VERSIONS OF THESE DATASETS IF available INSTEAD OF UPDATING THEM

######################################### #
## blockgroups ####
# documentOpen('./data-raw/datacreate_bgpts.R')              # USED BY datacreate_blockgroupstats2.32.R !! otherwise redundant w bg_cenpop2020
# documentOpen('./data-raw/datacreate_bg_cenpop2020.R')      # confirm if changed since 2020

######################################### #
### datacreate_bgpts.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_bgpts.R")

cat( "Is it loaded/attached already? "); cat("bgpts" %in% ls(), '\n'); 
cat("Is it a dataset in installed EJAM pkg? "); junk <- capture.output({XYZ <- datapack("EJAM")$Item}); cat("bgpts" %in% XYZ, '\n'); 
cat('Is it loadable and/or attached already, per "exists()" ? ', exists("bgpts"), '\n'); rm(junk, XYZ)
# dataload_from_pins("bgpts", justchecking = TRUE)# bgpts is in EJAM/data/  not on pins board.
#  attributes2(bgpts)

source_maybe("datacreate_bgpts.R", DOIT = FALSE, folder = rawdir) 
nacounts(bgpts)
# it gets saved with package as data

### datacreate_bg_cenpop2020.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_bg_cenpop2020.R")       IS IT USED AT ALL BY EJAM THOUGH??
source_maybe("datacreate_bg_cenpop2020.R", DOIT = FALSE, folder = rawdir)


######################################### #
## states ####
# documentOpen('./data-raw/datacreate_states_shapefile.R')   # check if want 2020 or 2022+ file
# documentOpen('./data-raw/datacreate_stateinfo.R')          # ok (missing Island Areas)
# documentOpen('./data-raw/datacreate_stateinfo2.R')         # ok (has Island Areas)

### datacreate_states_shapefile.R ####
# documentOpen('./data-raw/datacreate_states_shapefile.R')   # check if want 2020 or 2022+ file
source_maybe("datacreate_states_shapefile.R", DOIT = FALSE, folder = rawdir)
######################################### #
### datacreate_stateinfo.R ####
### datacreate_stateinfo2.R ####
# documentOpen('./data-raw/datacreate_stateinfo.R')          # ok (missing Island Areas)
# documentOpen('./data-raw/datacreate_stateinfo2.R')         # ok (has Island Areas)
## ok to update metadata whenever - these should never really change but want to note version 2.32 etc.
source_maybe('datacreate_stateinfo.R', DOIT = FALSE, folder = rawdir)
source_maybe('datacreate_stateinfo2.R', DOIT = FALSE, folder = rawdir)
######################################### #

## other geo ####
# documentOpen('./data-raw/datacreate_islandareas.R')        # ok
# documentOpen('./data-raw/datacreate_censusplaces.R')       # not used yet

### datacreate_islandareas.R ####
# documentOpen('./data-raw/datacreate_islandareas.R')        # ok
source_maybe("datacreate_islandareas.R", DOIT = FALSE, folder = rawdir)
######################################### #
### datacreate_censusplaces.R ####
# documentOpen('./data-raw/datacreate_censusplaces.R')       # not used yet
source_maybe("datacreate_censusplaces.R", DOIT = FALSE, folder = rawdir)

######################################### ########################################## #


## updated block-related datasets should be on local disk now but not yet in pins,
## and updated names_xyz and map_headernames should be in globalenv and in /data/ but not in installed pkg yet.
## so maybe best to rm(list = ls()) and load_all() again to get all new versions of everything
rmost2()
cat("Running load_all() but you may want to rebuild/install now \n")
loadall()


# ~------------------------------------------- ####
# *** EJSCREEN BLOCKGROUP DATA - ANNUAL UPDATES ####

## Demog + Envt data on blockgroups ####
## + pctile & avg lookup tables (usastats, statestats) ####

######################################### #
### datacreate_blockgroupstats2.32.R (also starts making usastats,statestats!!) ####
### ACS22 via datacreate_blockgroupstats2.32_add_d_acs22columns ####
# rstudioapi::documentOpen("./data-raw/datacreate_blockgroupstats2.32.R")
if (askquestions && interactive()) {
  y = askYesNo("Did you already update bgpts via new block weights and fips dataset? (required before updating blockgroupstats)")
  if (is.na(y) || !y) {
    rm(y)
    stop("Need to update bgpts via new block weights and fips dataset before updating blockgroupstats")
  }
}

source_maybe("datacreate_blockgroupstats2.32.R") # (also starts making usastats,statestats!!)
# created bgej (with metadata and documentation, and saved it locally but not to pins yet)
### bgej to pins ####
######################################### #
if (askquestions && interactive()) {
  pinej = askYesNo("write to bgej pins? ")
  if (!is.na(pinej) && pinej) {
    ## do not save via  usethis::use_data(bgej, overwrite = TRUE) - it is a large file
    ## Save bgej to pins board as .arrow file
    datawrite_to_pins("bgej", type = "arrow")
    # defaults should work but anyone doing this needs authentication, access to pins board !
  }}
# created blockgroupstats_new as interim object
# created usastats, statestats but not final versions yet

################################################################################ #
if (interactive() && askquestions) {
  SAVEIMAGE = askYesNo("Save globalenv() as an .RData file now?")
  if (is.na(SAVEIMAGE)) {SAVEIMAGE <- FALSE}
}
if (SAVEIMAGE) { # ARCHIVE as IMAGE?  
  cat("\n SAVING IMAGE OF WORK IN PROGRESS... \n\n")
  save.image(file = file.path(localfolder, "save.image work on NEW blockgroupstats usastats statestats.rda"))
}
################################################################################ #

# rstudioapi::documentOpen("./data-raw/datacreate_blockgroupstats2.32_recalc_language.R")
source_maybe("datacreate_blockgroupstats2.32_recalc_language.R", DOIT = TRUE) # this just creates a function that is later used to fix language data

# rstudioapi::documentOpen("./data-raw/datacreate_blockgroupstats2.32_add_d_acs22columns.R")  
# reads ACS22 extra file of demographics not on ftp site
source_maybe("datacreate_blockgroupstats2.32_add_d_acs22columns.R")  # reads ACS22 extra file of demographics not on ftp site



# created blockgroupstats (now with demog subgroups from ACS22 extra file of demographics not on ftp site)


################ ################# ################# ################# #
################ ################# ################# ################# ################# #
## check bgid values in all these datasets
# blockgroupstats :  bgfips, bgid, statename, ST, etc.
# bgej :                     bgid,   bgfips,  ST, etc.
# bgid2fips :                bgid,   bgfips
# bgpts                      bgid, + bgfips, etc.
# bg_cenpop2020              bgid (not bgfips) ST, etc.

# + blockwts :      blockid, bgid, etc.

# data.table(blockgroupstats)[is.na(bgfips), table(ST)]
# AS  GU  MP  - had been this but now zero since those were dropped
# 77  58 135 
# data.table(blockgroupstats)[is.na(bgid), table(ST)]
# - had been this but now zero since those were dropped
# AS   CT   GU   MP   VI 
# 77 2717   58  135  416 

# nacounts(blockgroupstats[, .(bgfips,bgid,pop)])
# exists("bgid2fips")


stopifnot(
  all(
    !anyDuplicated(blockgroupstats$bgid),
    # !anyDuplicated(bgej$bgid),
    !anyDuplicated(quaddata$bgid),
    !anyDuplicated(bgid2fips$bgid),
    !anyDuplicated(bgpts),
    !anyDuplicated(blockwts)
  )
)

stopifnot(
  all(
    !anyNA(blockgroupstats$bgid),
    !anyNA(bgej$bgid),
    !anyNA(quaddata$bgid),
    !anyNA(bgid2fips$bgid),
    !anyNA(bgpts),
    !anyNA(blockwts)
  )
)

stopifnot(
  all(
    setequal(blockgroupstats$bgid, bgej$bgid),    # ok
    setequal(blockgroupstats$bgid, quaddata$bgid)   , # false due to CT 19 as of 8/14/24
    setequal(blockgroupstats$bgid, bgid2fips$bgid)  , # false
    setequal(blockgroupstats$bgid, bgpts$bgid)      , # false
    setequal(blockgroupstats$bgid, bg_cenpop2020$bgid)  , # false
    setequal(blockgroupstats$bgid, blockwts$bgid)         # false
  )
)
################ ################# ################# ################# #
################ ################# ################# ################# #

######################################### #
### datacreate_usastats2.32.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_usastats2.32.R")
source_maybe("datacreate_usastats2.32.R")
# now usastats and statestats exist
######################################### #
### datacreate_usastats2.32_add_dsubgroups.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_usastats2.32_add_dsubgroups.R")
source_maybe("datacreate_usastats2.32_add_dsubgroups.R")
print(nacounts(usastats))
print(nacounts(statestats))

######################################### #
### datacreate_avg.in.us.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_avg.in.us.R")
source_maybe("datacreate_avg.in.us.R")
######################################### #
### datacreate_high_pctiles_tied_with_min.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_high_pctiles_tied_with_min.R")
source_maybe("datacreate_high_pctiles_tied_with_min.R")
######################################### #
### datacreate_formulas.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_formulas.R")
source_maybe("datacreate_formulas.R")
######################################### #

## Test data & examples of outputs ####
######################################### #
### datacreate_test_address_table.R #### 
# rstudioapi::documentOpen('./data-raw/datacreate_test_address_table.R')  
source_maybe("datacreate_test_address_table.R")
# creates several objects

############################### pause here
############################## # 

# save.image(file.path(localfolder, "work in progress.rda"))


# may want to rebuild/ reinstall the package here,
# or at least load_all()  ?
 
# system.time({
#   #  installing  TAKES  ~4 MINUTES  EVEN IF .onAttach() changed to say 
#   # asap_aws   <- FALSE  # download large datasets now?           Set to FALSE while Testing/Building often
#   # asap_index <- FALSE  
#   # asap_bg    <- FALSE 
#   # and install(  reload = TRUE, upgrade = "never" , quick = TRUE 
#   
#   devtools::install(reload = TRUE, upgrade = "never", quick = TRUE)
# })
 
# system.time({
#   #  THIS TAKES < 30 seconds   if reset = FALSE and not loading block datasets on attach
#   devtools::load_all(reset = FALSE)
#   
# })

system.time({
  #  THIS TAKES 20 seconds even though supposed to be slower if reset = T  and not loading block datasets on attach
  devtools::load_all(reset = TRUE)
})


# devtools::check() 


# devtools::test()

# rstudioapi::navigateToFile("./tests/manual_nonalphabetical.R")
# system.time({
#   #    ABOUT 10-20 MINUTES TO RUN all TESTS (if large datasets had not yet been loaded)
   source("./tests/manual_nonalphabetical.R") # answering Yes to running ALL tests
 biglist <- test_interactively(ask = askquestions)
## but should do AFTER updating test data 

# })

############################## # 
############################## # 


######################################### #
### datacreate_testpoints_testoutputs.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_testpoints_testoutputs.R")
source_maybe("datacreate_testpoints_testoutputs.R")

 ######################################### #
 ### datacreate_testshapes_2.R ####
 # rstudioapi::documentOpen("./data-raw/datacreate_testshapes_2.R")
 source_maybe("datacreate_testshapes_2.R")
 
 
# ~------------------------------------------- ####
## related to ejscreenapi  ####
######################################### #

### datacreate_default_points_shown_at_startup.R ####
source_maybe('datacreate_default_points_shown_at_startup.R')
### datacreate_testpoints_5_50_500.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_testpoints_5_50_500.R")
source_maybe('datacreate_testpoints_5_50_500.R')

### datacreate_ejscreenRESTbroker2table_na_filler.R ####
# rstudioapi::documentOpen("./data-raw/datacreate_ejscreenRESTbroker2table_na_filler.R")
source_maybe('datacreate_ejscreenRESTbroker2table_na_filler.R')

### datacreate_testoutput_ejscreenit_or_ejscreenapi_plus_50.R  ####
# rstudioapi::documentOpen("./data-raw/datacreate_testoutput_ejscreenit_or_ejscreenapi_plus_50.R")
source_maybe('datacreate_testoutput_ejscreenit_or_ejscreenapi_plus_50.R')

######################################### ########################################## #


document()

devtools::install(quick = TRUE)

######################################### ########################################## #

######################################### #
######################################### #
# ~------------------------------------------- ####
# *** FRS (regulated facilities) FREQUENT UPDATES (incl. NAICS/SIC) ####

########################################## #
#

## >>> frs functions need cleanup here <<< ####
cat(                                        "frs functions need cleanup here  \n")
warning("frs functions need cleanup here")





#                            TO BE CHECKED/ REVISED HERE

rmost() # ??

loadall() # needed to enable frs functions below that need 



## frs_by_ (lat,lon, regid,program,mact) ####

### ? datacreate_frs_.R ####
# rstudioapi::documentOpen('./data-raw/datacreate_frs_.R')            #  BUT SEE IF THIS HAS BEEN REVISED/ REPLACED  ***
# THAT SCRIPT USES EJAM:::frs_update_datasets() to download data, create datasets for pkg, 
# and save them locally, and read them into memory.
# That creates frs, frs_by_programid, frs_by_naics, frs_by_sic, frs_by_mact

source_maybe("datacreate_frs_.R", DOIT = FALSE, folder = rawdir)


### ? datacreate_frs_by_sic.R - is it redundant with frs_update_datasets() ?  SEE IF THIS HAS BEEN REPLACED ? ####
# documentOpen('./data-raw/datacreate_frs_by_sic.R')      #

### ? datacreate_frs_by_mact.R - is it redundant with frs_update_datasets() ?  SEE IF THIS HAS BEEN REPLACED ? ####
# documentOpen('./data-raw/datacreate_frs_by_mact.R')   #  BUT SEE IF THIS HAS BEEN REPLACED  ***
# Manually also need to save updated frsp .... [TRUNCATED] 
# Error in eval(ei, envir) : object 'folder_save_as_arrow' not found
# In addition: Warning messages:
#   1: Expected 2 pieces. Missing pieces filled with `NA` in 941 rows [30455, 30457, 30496, 30497, 30527, 30561, 30607, 30669, 30682, 30696, 30777, 30806, 30833, 30848, 30855, 30870, 30981,
#                                                                      31035, 31036, 31038, ...]. 
# 2: In frs_make_naics_lookup(x = frs) : NAs introduced by coercion
# 3: One or more parsing issues, call `problems()` on your data frame for details, e.g.:
#   dat <- vroom(...)
# problems(dat) 

### datacreate_frsprogramcodes.R ####
# documentOpen('./data-raw/datacreate_frsprogramcodes.R') #
## needs loaded metadata_add)() etc.
source_maybe('datacreate_frsprogramcodes.R')

### datacreate_epa_programs.R ####
# documentOpen('./data-raw/datacreate_epa_programs.R')    #
source_maybe('datacreate_epa_programs.R')

### datacreate_epa_programs_defined.R ####
# documentOpen('./data-raw/datacreate_epa_programs_defined.R')    #
source_maybe('datacreate_epa_programs_defined.R')

### datacreate_testids_program_sys_id.R ####
# documentOpen('./data-raw/datacreate_testids_program_sys_id.R')  # 
source_maybe('datacreate_testids_program_sys_id.R')

### datacreate_testids_registry_id.R ####
# documentOpen('./data-raw/datacreate_testids_registry_id.R')     #
source_maybe('datacreate_testids_registry_id.R')
########################################## #

# NAICS/ SIC Counts from FRS, etc. ####

## >>>             ADD SCRIPTS HERE? <<< ####
cat(                                        "naics functions not here yet? ... \n")
warning("naics functions not here yet")




#                            TO BE ADDED HERE



######################################### ########################################## #


stop('these need work...')


# THESE BELOW JUST DO COUNTS BY CODE - they dont actually update the NAICS/SIC info from the FRS data 
# (nor the names of industries by code that change maybe every 3 yrs for NAICS)

### datacreate_naics_counts.R ####
# documentOpen('./data-raw/datacreate_naics_counts.R')    # bad script
source_maybe('datacreate_naics_counts.R')

### datacreate_naicstable.R ####
# documentOpen('./data-raw/datacreate_naicstable.R')      #  #ok script. does date_saved_in_package & use_data
source_maybe('datacreate_naicstable.R')

### datacreate_SIC.R ####
# documentOpen('./data-raw/datacreate_SIC.R')
source_maybe('datacreate_SIC.R')
### datacreate_sic_counts.R ####
# documentOpen('./data-raw/datacreate_sic_counts.R')
source_maybe('datacreate_sic_counts.R')
### datacreate_sictable.R ####
# documentOpen('./data-raw/datacreate_sictable.R')
source_maybe('datacreate_sictable.R')

######################################### ########################################## #

# misc ####
# probably do not need to update these often or ever, but ok to do so
######################################### #
### datacreate_lat_alias.R ####
source_maybe('datacreate_lat_alias.R')
######################################### #
### datacreate_ejampackages.R ####
source_maybe('datacreate_ejampackages.R')
######################################### #
### datacreate_meters_per_mile.R ####
# documentOpen('./data-raw/datacreate_meters_per_mile.R')
source_maybe("datacreate_meters_per_mile.R")
######################################### # 

######################################### #
######################################### #
# ~------------------------------------------- ####
# *** PINNED DATA UPDATES ####
# For any of the datasets stored on the pins board server,
# Upload the new versions of those (large) data objects whenever they get updated. 

## datawrite_to_pins() ####

################## #
# pindates() & pinned() helper functions were NOT WORKING YET - date format is messed up
# 
# pindates  <- function(varnames =  c(
#   'blockwts', 'blockpoints', 'blockid2fips', "quaddata",
#   'bgej', 'bgid2fips',
#   'frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact"
# )) {
#   junk <- capture.output({
#     x <- dataload_from_pins(justchecking = TRUE, silent = TRUE, 
#                             varnames = varnames)
#   })
#   x <- x[, c("name", "created", "ejscreen_version")]
#   if (!missing(varnames))  {
#     # only show info for the specific ones queried, not all pinned
#     xshell <- data.frame(name = varnames, 
#                          # still need to fix date  format... 
#                          created = 0, ejscreen_version = NA)
#     for (i in seq_along(varnames)) {
#       if (varnames[i] %in% x$name) {
#         xshell[i, ] <- x[x$name == varnames[i], ]
#       }
#     }
#     x <- xshell
#   }
#   return(x) 
# }
################## #   NOT WORKING YET 
# pinned <- function(varnames = c(
#   'blockwts', 'blockpoints', 'blockid2fips', "quaddata",
#   'bgej', 'bgid2fips',
#   'frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact"
# )) {
#   x <- pindates(varnames)
#   x$created[is.na(x$created)] <- 0
#   x <- x[x$created != 0,]
#   y = (varnames %in% x$name)
#   names(y) <- varnames[varnames %in% x$name]
#   return(y)
# }
################## #
# data.frame(varname = varnames, 
#   pinned = pinned(varnames),
#   datepinned = pindates(varnames)$created,
#   attached = varnames %in% ls(), 
#   loaded_or_lazyloadable = sapply(varnames, exists), 
#   in_data_folder = varnames %in% tools::file_path_sans_ext(basename(dir("./data")) )
#   )
################## #

# Review the datasets / pins

cat("Which datasets are attached or can be lazyloaded in memory, of those you may want to pin?")
x = dataload_from_pins(justchecking = TRUE, silent = TRUE, 
                       varnames = c(
                         "blockwts", "blockpoints", "blockid2fips", "quaddata", # "localtree",
                         "bgej", "bgid2fips",
                         "frs", "frs_by_programid", "frs_by_naics", "frs_by_sic", "frs_by_mact"
                       )
)
cat("These datasets are currently seen on the pins board: \n")
x[,c("name", "created", "ejscreen_version")]

cat("These datasets get written to pins board by the function dataload_from_pins() \n")
print(formals(dataload_from_pins)$varnames)
cat("\n\n")

## Example if manually obtained and saved these locally, then reading them into memory:
# dataload_from_local(folder_local_source = "~/../Downloads/EJAMbigfiles", varnames = c("frs", "frs_by_mact", "frs_by_sic", "frs_by_naics", "frs_by_programid")  )
## then saving to pins
# EJAM:::datawrite_to_pins(varnames = c("frs", "frs_by_mact", "frs_by_sic", "frs_by_naics", "frs_by_programid"))

these = c("blockwts", "blockpoints", "blockid2fips", "quaddata",  
          "bgid2fips"
)
# , "bgej")

datawrite_to_pins(varnames = these) # it will ask interactively to confirm which ones among defaults to save to pins


x = dataload_from_pins(justchecking = TRUE, silent = TRUE, 
                       varnames = c(
                         "blockwts", "blockpoints", "blockid2fips", "quaddata", # "localtree",
                         "bgej", "bgid2fips",
                         "frs", "frs_by_programid", "frs_by_naics", "frs_by_sic", "frs_by_mact"
                       )
)
cat("These datasets are currently seen on the pins board: \n")
x[,c("name", "created", "ejscreen_version")]

######################################### #
######################################### #
# ~------------------------------------------- ####
# ~ ####
# CLEANUP - Remove most objects ####

rmost2()
cat("Running load_all() but you may want to rebuild/install now \n")
loadall()


######################################### #
# ~------------------------------------------- ####
# ~ ####
# DOCUMENTATION WEBSITE UPDATE #### 
cat("\n\n You may want to use 'datacreate_0_UPDATE_ALL_DOCUMENTATION_pkgdown.R' now \n\n")
#  rstudioapi::documentOpen("./data-raw/datacreate_0_UPDATE_ALL_DOCUMENTATION_pkgdown.R")
source_maybe("datacreate_0_UPDATE_ALL_DOCUMENTATION_pkgdown.R")
########################################## ######################################### # 
