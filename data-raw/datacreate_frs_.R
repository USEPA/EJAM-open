################################################################################## #
# SCRIPT TO READ AND CLEAN LATEST FRS (and FRS BY SIC) DATASETS
################################################################################## #

# Note: some key frs files are not stored as part of the package in EJAM/data/ but downloaded for use

# Note: compare frsprogramcodes, epa_programs, epa_programs_defined, etc.

################################################################################ # 
## DOWNLOAD FRS info AND UPDATE/CREATE & SAVE LOCAL FILES for frs-related datasets
################################################################################ # 
#

mydir <- "~/../Downloads/EJAMbigfiles" # or where you want to save them locally once updated

if (!dir.exists(mydir)) {dir.create(mydir)}
if (!exists("alreadygot")) {
  alreadygot <- FALSE
  mytemp <- tempdir()
}

# This function frs_update_datasets() was in a separate pkg but moving it to EJAM pkg 

frs_update_datasets(folder = mytemp, # default would use a tempdir() but not return its name
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


##################################### # 
# frsprogramcodes.rda
#
# Manually also need to save updated frsprogramcodes.rda
#  see EJAM/data-raw/datacreate_frsprogramcodes.R
#
# and may need to update counts! ... see ???


################################################################################ # 
##  LOAD dataset FILES INTO MEMORY (If saved as .arrow locally but not kept in memory)
################################################################################ # 
#
fold <- folder_save_as_arrow
frs_vars <- c('frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact")
for (varname in frs_vars) {
  fname <- paste0(varname, ".arrow")
  assign(varname, value = arrow::read_ipc_file(file = file.path(fold, fname)))
}

################################################################################ # 
## WRITE .arrow FILES TO pins BOARD on Posit Connect server, once loaded in memory
################################################################################ # 
# 
# THAT IS DONE BY  datawrite_to_pins() 

# and copy to any local folder being used to cache them, e.g., EJAM/data folder

cat("Note this should not be saved as a dataset in the package.\n")

cat("UPDATE THE DOCUMENTATION MANUALLY in data_frs.R BUT USE NULL AT THE END SINCE IT IS NOT AN OBJECT STORED IN THE PACKAGE\n")
if (rstudioapi::isAvailable()) {
  for (myvar in frs_vars) {
    rstudioapi::documentOpen(paste0('./R/data_', myvar, '.R'))
  }
}
# or... 
# for (myvar in frs_vars) {
# dataset_documenter(myvar, 
#                    # but these docs are complicated and best edited in the doc itself
#                    saveinpackage = FALSE)
# }
