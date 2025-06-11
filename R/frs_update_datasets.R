#' Main function that updates several FRS datasets for use in EJAM
#' 
#' @details
#' This function is used by someone maintaining the EJAM package, 
#' to obtain updated Facility Registry Service (FRS) data such as
#' the locations, IDs, etc. for hundreds of thousands of EPA-regulated sites.
#'   
#'  This function is only for a package maintainer/updater
#'  (or analyst who wants to get the latest information).
#'  It would normally be called from a script like 
#'  `EJAM/data-raw/datacreate_0_UPDATE_ALL_DATASETS.R`
#' 
#'  These datasets are obtained from EPA servers, reformatted for this package, and
#'  then stored in a separate repository - see [updating data for package](../articles/dev_update-datasets.html).
#'  The save_as_data_ parameters here are set to FALSE because the files are 
#'  not saved in the source package or its repository like 
#'  typical package datasets would be saved in the data folder of the source package.
#'  
#'  The files later get downloaded for local use during the process
#'  of installing the EJAM package.
#'   
#' @param folder optional folder for where to download to; uses temp folder by default
#' @param folder_save_as_arrow optional folder where to save any .arrow files
#' @param downloaded_and_unzipped_already optional, set to TRUE if already downloaded latest 
#'   and folder will be specified or can be assumed to be current working directory
#' @param csvname optional, passed to frs_get()
#' 
#' @param save_as_arrow_frs Whether to save as .arrow in getwd()
#' @param save_as_arrow_frs_by_programid Whether to save as .arrow in getwd()
#' @param save_as_arrow_frs_by_naics Whether to save as .arrow in getwd()
#' @param save_as_arrow_frs_by_sic Whether to save as .arrow in getwd()
#' @param save_as_arrow_frs_by_mact Whether to save as .arrow in getwd()
#' 
#' @param save_as_data_frs Whether to save as .rda in ./data/
#' @param save_as_data_frs_by_programid Whether to save as .rda in ./data/
#' @param save_as_data_frs_by_naics Whether to save as .rda in ./data/
#' @param save_as_data_frs_by_sic Whether to save as .rda in ./data/
#' @param save_as_data_frs_by_mact Whether to save as .rda in ./data/
#' 
#' @return Creates saved copies of datasets for the R package, overwriting old ones, using
#'   [frs_get()] and [frs_inactive_ids()] and other functions, and invisibly returns [frs].
#'
#' @seealso [frs_get()] [frs_inactive_ids()] [frs_drop_inactive()]
#'    [frs_make_programid_lookup()] [frs_make_naics_lookup()] [frs_make_sic_lookup()] [frs_make_mact_lookup()] 
#' 
frs_update_datasets <- function(folder = NULL,
                                folder_save_as_arrow = '.',
                                downloaded_and_unzipped_already = FALSE,
                                csvname = "NATIONAL_SINGLE.CSV",

                                save_as_arrow_frs              = TRUE,
                                save_as_arrow_frs_by_programid = TRUE,
                                save_as_arrow_frs_by_naics     = TRUE,
                                save_as_arrow_frs_by_sic       = TRUE,
                                save_as_arrow_frs_by_mact      = TRUE,
                                
                                save_as_data_frs               = FALSE,
                                save_as_data_frs_by_programid  = FALSE,
                                save_as_data_frs_by_naics      = FALSE,
                                save_as_data_frs_by_sic        = FALSE,
                                save_as_data_frs_by_mact       = FALSE) {
  

  commas <- function(x) {
    prettyNum(x, big.mark = ",")
  }
  if (is.null(folder)) 
    folder <- tempdir()
  ###################################################### #
  cat("\nTrying to get frs datasets\n")
  cat("This takes a *LONG* time to download, unzip, and read the large files! Please wait!\n")
  frs <- frs_get(folder = folder, csvname = csvname, downloaded_and_unzipped_already = downloaded_and_unzipped_already)
  closedidlist <- frs_inactive_ids()
  cat("frs rows total: ", commas(NROW(frs)), '\n')
  cat("frs clearly inactive IDs: ", commas(length(closedidlist)), "\n")
  frs <- frs_drop_inactive(frs = frs, closedid = closedidlist)
  cat("frs rows actives: ", commas(NROW(frs)), "\n")
  
  # validate lat lon values 
  cat('checking latlon values are valid \n')
  bad <- sum(!latlon_is.valid(frs$lat, frs$lon))
  if (bad > 0) {warning(bad, " LAT LON VALUES APPEAR TO BE INVALID (e.g., in US Island Areas, where demographic data may be lacking")}
  
  # validate regid
  cat('checking REGISTRY_ID is never NA \n')
  bad <- sum(is.na(frs$REGISTRY_ID))
  if (bad > 0) {warning(bad, "REGISTRY_ID values appear to be NA")}
  
  metadata_add(frs)
  cat("Saving .arrow file \n")
  if (save_as_arrow_frs) {
    arrow::write_ipc_file(frs, sink = file.path(folder_save_as_arrow, 
                                                "frs.arrow"))
  }
  if (save_as_data_frs) {
    usethis::use_data(frs, overwrite = TRUE)
  }
  ###################################################### #
  cat("\nTrying to create frs_by_programid\n")
  frs_by_programid <- frs_make_programid_lookup(x = frs)
  # dropped invalid ones, in that function.
  metadata_add(frs_by_programid)
  if (save_as_arrow_frs_by_programid) {
    cat("Saving .arrow file \n")
    arrow::write_ipc_file(frs_by_programid, sink = file.path(folder_save_as_arrow, 
                                                             "frs_by_programid.arrow"))
  }
  if (save_as_data_frs_by_programid) {
    usethis::use_data(frs_by_programid, overwrite = TRUE)
  }
  ###################################################### #
  cat("\nTrying to create frs_by_naics\n")
  frs_by_naics <- frs_make_naics_lookup(x = frs)
  cat("frs_by_programid rows: ", commas(NROW(frs_by_programid)), 
      "\n")
  cat("frs_by_naics rows: ", commas(NROW(frs_by_naics)), "\n")
  metadata_add(frs_by_naics)
  if (save_as_arrow_frs_by_naics) {
    cat("Saving .arrow file \n")
    arrow::write_ipc_file(frs_by_naics, sink = file.path(folder_save_as_arrow, 
                                                         "frs_by_naics.arrow"))
  }
  if (save_as_data_frs_by_naics) {
    usethis::use_data(frs_by_naics, overwrite = TRUE)
  }
  ###################################################### #
  
  stop('see EJAM/data-raw/datacreate_frs_by_sic.R and compare to this script ***')
  
  cat("\nTrying to create frs_by_sic\n")
  frs_by_sic <- frs_clean_sic(frs)
  frs_by_sic <- frs_make_sic_lookup(frs_by_sic)
  metadata_add(frs_by_sic)
  if (save_as_arrow_frs_by_sic) {
    arrow::write_ipc_file(frs_by_sic, sink = file.path(folder_save_as_arrow, 
                                                       "frs_by_sic.arrow"))
  }
  if (save_as_data_frs_by_sic) {
    usethis::use_data(frs_by_sic, overwrite = TRUE)
  }
  ###################################################### #
  cat("Trying to create frs_by_mact\n")
  dataload_from_local('frs_by_programid', folder_local_source = folder)
  x <- frs_make_mact_lookup(frs_by_programid, folder = folder)
  frs_by_mact <- x$frs_by_mact
  metadata_add(frs_by_mact)
  if (save_as_arrow_frs_by_mact) {
    arrow::write_ipc_file(frs_by_mact, sink = file.path(folder_save_as_arrow, 
                                                        "frs_by_mact.arrow"))
  }
  if (save_as_data_frs_by_mact) {
    usethis::use_data(frs_by_mact, overwrite = TRUE)
  }
  ###################################################### #
  cat("Trying to create mact_table\n")
  mact_table <- x$mact_table
  rm(x)
  metadata_add(mact_table)
  cat("\n\n**** HANDLE mact_table DIFFERENTLY ? \n\n")
  arrow::write_ipc_file(mact_table, sink = file.path(folder_save_as_arrow, 
                                                     "mact_table.arrow"))
  save(mact_table, file = file.path(folder_save_as_arrow, 
                                    "mact_table.rda"))
  if (interactive()) {
    oldone <- getwd()
    x = rstudioapi::selectDirectory("What folder is root of source package in which to store mact_table as data?", 
                                    label = "Save")
    setwd(x)
    usethis::use_data(mact_table, overwrite = TRUE)
    setwd(oldone)
  }
  ###################################################### #
  
  if (any(save_as_data_frs, 
          save_as_data_frs_by_programid, 
          save_as_data_frs_by_naics, 
          save_as_data_frs_by_sic, 
          save_as_data_frs_by_mact)) {
    cat("You can now rebuild/install the package from source to update it.\n")
  }
  ############################################################# #
  
  print(Sys.time()) 
  invisible(frs)
}
############################################################# #
############################################################# #
############################################################# #

# what this looks like in console:

# devtools::load_all()
# > frs_update_datasets()

# This takes a *LONG* time to download, unzip, and read the large files! Please wait!
#   [1] "2023-12-11 10:33:36 EST"
# Downloading... Trying to download from  https://ordsext.epa.gov/FLA/www3/state_files//national_single.zip
#   to save as    \AppData\Local\Temp\RtmpQHgVhc/national_single.zip 
# trying URL 'https://ordsext.epa.gov/FLA/www3/state_files//national_single.zip'
# Content type 'application/zip' length 312820183 bytes (298.3 MB)
# downloaded 298.3 MB
# 
# Finished download to  AppData\Local\Temp\RtmpQHgVhc/national_single.zip 
# [1] "2023-12-11 10:34:05 EST"
# Unzipping...done unzipping
# [1] "2023-12-11 10:34:25 EST"
# Reading... This takes something like 30 seconds. please wait.
# |--------------------------------------------------|
#   |==================================================|
#   Finished reading file.
# [1] "2023-12-11 10:34:55 EST"
# Cleaning...  Finished cleaning file.
# Total rows:  4809978 
# Rows with lat/lon:  3490113 
# [1] "2023-12-11 10:35:00 EST"
# 
# To use in package,  usethis::use_data(frs, overwrite=TRUE)  
# Also see frs_make_naics_lookup() and frs_make_programid_lookup()
# Downloading national dataset to temp folder... Takes a couple of minutes! 
#   Trying to download from  https://ordsext.epa.gov/FLA/www3/state_files/national_combined.zip 
# to save as  AppData\Local\Temp\RtmpQHgVhc/national_combined.zip 
# trying URL 'https://ordsext.epa.gov/FLA/www3/state_files/national_combined.zip'
# Content type 'application/zip' length 1320371459 bytes (1259.2 MB)
# downloaded 1259.2 MB
# 
# Finished download to   AppData\Local\Temp\RtmpQHgVhc/national_combined.zip 
# Reading unzipped file...
# |--------------------------------------------------|
#   |==================================================|
#   Complete list of unique ids is 4775797 out of 7,558,760 rows of data.
# Count of    all REGISTRY_ID rows:   7,558,760
# Count of unique REGISTRY_ID values: 4,775,797
# Clearly inactive unique IDs:      1,511,111
# Assumed   active unique IDs:      3,264,686
# 
# Codes assumed to mean site is closed: 
#   CLOSED 
# PERMANENTLY CLOSED 
# PERMANENTLY SHUTDOWN 
# INACTIVE 
# TERMINATED 
# N 
# RETIRED 
# OUT OF SERVICE – WILL NOT BE RETURNED 
# CANCELED, POSTPONED, OR NO LONGER PLANNED 
# 
# frs rows total:  3,490,113 
# frs clearly inactive IDs:  1,511,111 
# frs rows actives:  2,576,588 
# [1] "To use in package,  usethis::use_data(frs_by_naics, overwrite=TRUE)  "
# frs_by_programid rows:  3,438,163 
# frs_by_naics rows:  697,444 
# Error in `[.data.table`(frs, , ..usefulcolumns) : 
#   column(s) not found: LATITUDE83, LONGITUDE83, SIC_CODES
# In addition: Warning messages:
#   1: Expected 2 pieces. Missing pieces filled with `NA` in 1160 rows [31360, 31362, 31395, 31396, 31426, 31428, 31461,
#                                                                       31548, 31567, 31569, 31685, 31711, 31732, 31841, 31896, 31897, 31899, 31918, 31919, 31929, ...]. 
# 2: In frs_make_naics_lookup(x = frs) : NAs introduced by coercion
# Called from: `[.data.table`(frs, , ..usefulcolumns)
# 

