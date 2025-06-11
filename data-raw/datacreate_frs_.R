################################################################################## #
# SCRIPT TO READ AND CLEAN LATEST FRS (and FRS BY SIC) DATASETS
################################################################################## #

# Note: some key frs files are not stored as part of the package in EJAM/data/ but downloaded for use

# Note: compare frsprogramcodes, epa_programs, epa_programs_defined, etc.

################################################################################ # 
## DOWNLOAD FRS info AND UPDATE/CREATE & SAVE LOCAL FILES for frs-related datasets
################################################################################ # 

if (!exists("mydir") && interactive()) {
  mydir <- choose.dir(".", "Select where to save large files being downloaded and modified/prepared")
# mydir <- "~/../Downloads/EJAMbigfiles" #   where you want to save them locally
}
if (!dir.exists(mydir)) {dir.create(mydir)}
if (!exists("alreadygot")) {
  alreadygot <- FALSE
  mytemp <- tempdir()
}
cat("Starting frs_update_datasets(), which invisibly returns frs data.table and related tables are saved too \n")
# This function frs_update_datasets() used to be in a separate pkg but now in EJAM pkg 

x = EJAM:::frs_update_datasets(folder = mytemp, # default would use a tempdir() but not return its name
                    downloaded_and_unzipped_already = alreadygot,
                    folder_save_as_arrow = mydir,
                    save_as_arrow_frs              = TRUE,
                    save_as_arrow_frs_by_programid = TRUE,
                    save_as_arrow_frs_by_mact      = TRUE,
                    save_as_arrow_frs_by_naics     = TRUE,
                    save_as_arrow_frs_by_sic       = TRUE,
                    save_as_data_frs              = FALSE,
                    save_as_data_frs_by_mact      = FALSE,
                    save_as_data_frs_by_naics     = FALSE,
                    save_as_data_frs_by_programid = FALSE,
                    save_as_data_frs_by_sic       = FALSE)
alreadygot <- TRUE
# dir(folder_save_as_arrow)
cat("Finished frs_update_datasets() \n")
##################################### # 
# frsprogramcodes.rda
#
cat("
See EJAM/data-raw/datacreate_frsprogramcodes.R
May need to manually save updated frsprogramcodes.rda
May need to update counts too! 
")
################################################################################ # 
##  LOAD dataset FILES INTO MEMORY (If saved as .arrow locally but not kept in memory)
################################################################################ # 
#
fold <- mydir # folder_save_as_arrow
frs_vars <- c('frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact")
for (varname in frs_vars) {
  fname <- paste0(varname, ".arrow")
  assign(varname, value = arrow::read_ipc_file(file = file.path(fold, fname)))
}

################################################################################ # 
## obsolete- used to WRITE .arrow FILES TO pins BOARD on Posit Connect server, once loaded in memory
################################################################################ # 
# USED TO BE DONE BY  datawrite_to_pins() 

# and copy to any local folder being used to cache them, e.g., EJAM/data folder

cat("Note this should not be saved as a dataset in the package.\n")

cat("
NOW, UPDATE THE DOCUMENTATION MANUALLY in relevant files like data_frs.R,
since dataset_documenter() only works well for simple documentation and these are complicated to explain.
REMEMBER TO USE a NULL AT THE END of the .R file that documents each,
SINCE frs etc. are NOT dataset OBJECTS (so they are not STORED IN THE PACKAGE EJAM/data/  folder).\n")
if (rstudioapi::isAvailable()) {
  for (myvar in frs_vars) {
    rstudioapi::documentOpen(paste0('./R/data_', myvar, '.R'))
  }
}
# not... 
# for (myvar in frs_vars) {
# dataset_documenter(myvar, 
#                    #  these docs are complicated and best edited in the doc itself
#                    saveinpackage = FALSE)
# }
