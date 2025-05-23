
creatingnew_testpoints_data   <- FALSE  
resaving_testpoints_rda       <- TRUE # updates metadata too
resaving_testpoints_excel     <- FALSE

resaving_testpoints_helpdocs  <- TRUE


## code to prepare datasets 
if (creatingnew_testpoints_data) {
  testpoints_5                 <- read_csv_or_xl('./inst/testdata/latlon/testpoints_5.xlsx')
  testpoints_50                <- read_csv_or_xl('./inst/testdata/latlon/testpoints_50.xlsx')
  testpoints_500               <- read_csv_or_xl('./inst/testdata/latlon/testpoints_500.xlsx')
  
  testpoints_5$ejscreenmap <- NULL  
  testpoints_50$ejscreenmap <- NULL # works ok in excel but not once saved as dataset and imported for use in ejscreenit() etc.
  testpoints_500$ejscreenmap <- NULL 
}

if (resaving_testpoints_rda) {

  testpoints_5   <- metadata_add(testpoints_5)
  testpoints_50  <- metadata_add(testpoints_50)
  testpoints_500 <- metadata_add(testpoints_500)
  
  usethis::use_data(testpoints_5,                 overwrite = TRUE)
  usethis::use_data(testpoints_50,                overwrite = TRUE)
  usethis::use_data(testpoints_500,               overwrite = TRUE)
  # 
  # # load the existing test points
  # data("testpoints_5")
  # data("testpoints_50")
  # data("testpoints_500")
  # 
  # file.exists('./R/testpoints_5.R')
  # file.exists('./R/testpoints_50.R')
  # file.exists('./R/testpoints_500.R')
}
#################################### #
# helper function to save datasets as .xlsx

savex <-  function(x, folder = "./inst/testdata", fname = "example.xlsx")  {
  
  # use openxlsx::write.xlsx() instead of writexl package function called write_xlsx()
  # writexl is zero dependency package for writing xlsx files that is light weight,
  # but we already have imported openxlsx package to use more features like formatting it offers for xlsx download in app,
  # so may as well just use that to write xlsx and maybe can avoid dependency on writexl.
  
  if (!dir.exists(folder)) {stop("tried to save .xlsx but folder does not exist: ", folder)}
  fpath <- file.path(folder, fname)
  openxlsx::write.xlsx(x, file = fpath, overwrite = TRUE)
  if (!file.exists(fpath)) {stop("tried but could not save ", fpath)} else {cat("saved ", fpath, "\n")}
}
#################################### #

# SAVE AS EXCEL FILE  ####
if (resaving_testpoints_excel) { 
  p =  paste0("./inst/testdata/latlon/")
  savex(testpoints_5,   p, "testpoints_5.xlsx")
  savex(testpoints_50,  p, "testpoints_50.xlsx")
  savex(testpoints_500, p, "testpoints_500.xlsx")
  
  # writexl::write_xlsx(list(testpoints = testpoints_5),   path = paste0("./inst/testdata/latlon/", "testpoints_5",   ".xlsx"))    ############# #
  # writexl::write_xlsx(list(testpoints = testpoints_50),  path = paste0("./inst/testdata/latlon/", "testpoints_50",  ".xlsx"))    ############# #
  # writexl::write_xlsx(list(testpoints = testpoints_500), path = paste0("./inst/testdata/latlon/", "testpoints_500", ".xlsx"))    ############# #
}

#################################### #
# SAVE DOCUMENTATION AS A FILE ####
if (resaving_testpoints_helpdocs) { 
  for (testpoints_name in c("testpoints_5", "testpoints_50", "testpoints_500")) {
    
    dataset_documenter(testpoints_name,
                       title = "test points data.frame with columns sitenumber, lat, lon",
                       description = "Examples of what could be inputs to functions that need points specified by lat lon",
                       details = "Just for convenience, these are installed with the package, and are
#'  the equivalent of results of reading the .xlsx test data files.",
                       seealso = "[testpoints_5] [testpoints_50] [testpoints_500]
#'   
#'   [testoutput_ejscreenit_5] [testoutput_ejscreenit_50] [testoutput_ejscreenit_500]"
    )
  }
  
  ## confirm documentation exists  ####
  if (!all(
    file.exists("./R/data_testpoints_5.R"),
    file.exists("./R/data_testpoints_50.R"),
    file.exists("./R/data_testpoints_500.R")
    
  )) {stop("documentation file(s) not created")}
}
