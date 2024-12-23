# update the test ponts and test output examples to latest version of package

# 20 MINUTES FOR THE 500 POINTS, ROUGHLY
######################################################################################################## # 

make_testoutput_ejscreenRESTbroker_1pts_1miles <- TRUE

make_testoutput_ejscreenapi_1pts_1miles        <- TRUE

make_testoutput_ejscreenapi_plus_5   <- TRUE  # used in some examples still 
make_testoutput_ejscreenapi_plus_50  <- FALSE  # not really needed
make_testoutput_ejscreenapi_plus_500 <- FALSE  # not really needed ## 20 MINUTES FOR THE 500 POINTS, ROUGHLY

make_testoutput_ejscreenit_5   <-   TRUE
make_testoutput_ejscreenit_50  <-   TRUE
make_testoutput_ejscreenit_500 <-   TRUE   ## 20 MINUTES FOR THE 500 POINTS, ROUGHLY

######################################################################################################## # 

# ejscreenRESTbroker  ##################

if (make_testoutput_ejscreenRESTbroker_1pts_1miles) {
  
  # > testpoints_5$lon[1] # [1] -111.904
  # > testpoints_5$lat[1] # [1] 33.56042
  testoutput_ejscreenRESTbroker_1pts_1miles <- ejscreenRESTbroker(lon = testpoints_5$lon[1], lat = testpoints_5$lat[1], radius = 1)
  testoutput_ejscreenRESTbroker_1pts_1miles <- EJAM:::metadata_add(testoutput_ejscreenRESTbroker_1pts_1miles)
  usethis::use_data(testoutput_ejscreenRESTbroker_1pts_1miles,   overwrite = TRUE)
  
  ## _Documentation ####
  
  dataset_documenter("testoutput_ejscreenRESTbroker_1pts_1miles",
  title = "test data, output from this function",
  details = "Just for convenience, installed with the package,
#'  the equivalent of results of 
#'   `ejscreenRESTbroker(lon = testpoints_5$lon[1], lat = testpoints_5$lat[1], radius = 1)`"    ,
  seealso = "[testpoints_5] [testpoints_50] [testpoints_500]
#'   
#'   [testoutput_ejscreenit_5] [testoutput_ejscreenit_50] [testoutput_ejscreenit_500]"
  )

  
}
######################################################################################################## # 

# ejscreenapi  ##################

if (make_testoutput_ejscreenapi_1pts_1miles) {
  
  testoutput_ejscreenapi_1pts_1miles <- ejscreenapi(lon = testpoints_5$lon[1], lat = testpoints_5$lat[1], radius = 1, unit = "miles", wkid = 4326,
                                                    report_every_n = 25, # report_every_n = 1000,
                                                    save_when_report = FALSE, format_report_or_json = "pjson", on_server_so_dont_save_files = FALSE, ipurl = "ejscreen.epa.gov",
                                                    updateProgress = NULL, drop_redundant_indicators = FALSE, verbose = FALSE)
  
  testoutput_ejscreenapi_1pts_1miles <- EJAM:::metadata_add(testoutput_ejscreenapi_1pts_1miles)
  usethis::use_data(testoutput_ejscreenapi_1pts_1miles,   overwrite = TRUE)
  
  ## _Documentation ####
  dataset_documenter("testoutput_ejscreenapi_1pts_1miles",
    title = "test data, output from this function",
    details = "Just for convenience, installed with the package,
#'  the equivalent of results of 
#'  
#'  ejscreenapi(lon = testpoints_5$lon[1], lat = testpoints_5$lat[1],
#'     
#'      radius = 1, unit = 'miles', wkid = 4326,
#'     
#'      report_every_n = 25, # report_every_n = 1000,
#'     
#'      save_when_report = FALSE, format_report_or_json =  'pjson', on_server_so_dont_save_files = FALSE, 
#'      ipurl = 'ejscreen.epa.gov',
#'     
#'      updateProgress = NULL, drop_redundant_indicators = FALSE)",
    seealso = "[testpoints_5] [testpoints_50] [testpoints_500]
#'   
#'   [testoutput_ejscreenit_5] [testoutput_ejscreenit_50] [testoutput_ejscreenit_500]"
  )
  
}
######################################################################################################## # 

# ejscreenapi_plus?? ##################

object_nameset = NULL # c('testoutput_ejscreenapi_plus_5', 'testoutput_ejscreenapi_plus_50', 'testoutput_ejscreenapi_plus_500')
# skip (most of?) these now - just do ejscreenit() examples

if (make_testoutput_ejscreenapi_plus_5) { 
  object_nameset <- c(object_nameset, 'testoutput_ejscreenapi_plus_5')
  testoutput_ejscreenapi_plus_5   <- ejscreenapi_plus(testpoints_5 ,  radius = 1, save_when_report = F, on_server_so_dont_save_files = T, verbose = F)
  testoutput_ejscreenapi_plus_5 <- EJAM:::metadata_add(testoutput_ejscreenapi_plus_5)
  usethis::use_data(testoutput_ejscreenapi_plus_5,   overwrite = TRUE)
}
if (make_testoutput_ejscreenapi_plus_50) {
  object_nameset <- c(object_nameset, 'testoutput_ejscreenapi_plus_50')
  testoutput_ejscreenapi_plus_50  <- ejscreenapi_plus(testpoints_50,  radius = 1, save_when_report = F, on_server_so_dont_save_files = T, verbose = F)
  testoutput_ejscreenapi_plus_50  <- EJAM:::metadata_add(testoutput_ejscreenapi_plus_50)
  usethis::use_data(testoutput_ejscreenapi_plus_50,  overwrite = TRUE)
}
if (make_testoutput_ejscreenapi_plus_500) {
  object_nameset <- c(object_nameset, 'testoutput_ejscreenapi_plus_500')
  testoutput_ejscreenapi_plus_500 <- ejscreenapi_plus(testpoints_500, radius = 1, save_when_report = F, on_server_so_dont_save_files = T, verbose = F)
  testoutput_ejscreenapi_plus_500 <- EJAM:::metadata_add(testoutput_ejscreenapi_plus_500)
  usethis::use_data(testoutput_ejscreenapi_plus_500, overwrite = TRUE)
}
## _Documentation ####

for (object_name in object_nameset) {
  
dataset_documenter(object_name,
                   title = paste0("test data examples of output from [ejscreenapi_plus()] using testpoints", gsub('testoutput_ejscreenapi_plus','', object_name),", radius = 1"),
details = "Just for convenience, installed with the package.
 #'  Has header row, plus a row for each point, and about 300+ columns of buffer summary results.",
 seealso = "[testpoints_5] [testpoints_50] [testpoints_500]
#'
#'   [testoutput_ejscreenit_5] [testoutput_ejscreenit_50] [testoutput_ejscreenit_500]")
}
################################################################################################### # 

# ejscreenit ##################

object_nameset = NULL # c('testoutput_ejscreenit_5', 'testoutput_ejscreenit_50', 'testoutput_ejscreenit_500')

if (make_testoutput_ejscreenit_5) {
  object_nameset <- c(object_nameset, 'testoutput_ejscreenit_5')
  testoutput_ejscreenit_5               <- ejscreenit(testpoints_5,   radius = 1, nosee = T, nosave = T, save_map = F, save_plot = F, save_table = F, interactiveprompt = F)
  testoutput_ejscreenit_5 <- EJAM:::metadata_add(testoutput_ejscreenit_5)
  usethis::use_data(testoutput_ejscreenit_5,         overwrite = TRUE)
}
if (make_testoutput_ejscreenit_50) {
  object_nameset <- c(object_nameset, 'testoutput_ejscreenit_50')
  testoutput_ejscreenit_50              <- ejscreenit(testpoints_50,  radius = 1, nosee = T, nosave = T, save_map = F, save_plot = F, save_table = F, interactiveprompt = F)
  testoutput_ejscreenit_50 <- EJAM:::metadata_add(testoutput_ejscreenit_50)
  usethis::use_data(testoutput_ejscreenit_50,        overwrite = TRUE)
}
if (make_testoutput_ejscreenit_500) {
  object_nameset <- c(object_nameset, 'testoutput_ejscreenit_500')
  testoutput_ejscreenit_500             <- ejscreenit(testpoints_500, radius = 1, nosee = T, nosave = T, save_map = F, save_plot = F, save_table = F, interactiveprompt = F)
  testoutput_ejscreenit_500 <- EJAM:::metadata_add(testoutput_ejscreenit_500)
  usethis::use_data(testoutput_ejscreenit_500,       overwrite = TRUE)
}
## _Documentation ####

for (object_name in object_nameset) {
  
  dataset_documenter(object_name,
                     title = paste0("test data examples of output from [ejscreenit()] using testpoints", gsub('testoutput_ejscreenit','', object_name),", radius = 1"),
                     details = "Just for convenience, installed with the package.
#'  A list of outputs, named 'table', 'map', and 'plot'",
                     seealso = "[testpoints_5] [testpoints_50] [testpoints_500]
#'
#'   [testoutput_ejscreenit_5] [testoutput_ejscreenit_50] [testoutput_ejscreenit_500]"
                       )
  

  
}


# clean up ###############################
cat('
    REMEMBER TO RECREATE PACKAGE DOCUMENTATION:
    devtools::document()  # for .Rd help files. or Clean and INSTALL package
    see EJAM/data-raw/datacreate_0_UPDATE_ALL_DOCUMENTATION_pkgdown.R  for the documentation website
    devtools::build_manual()  # for pdf manual
    postdoc::render_package_manual()  # for html manual
    \n')

rm(
  object_name,
  filecontents,
  
  testpoints_5,  
  testpoints_50,  
  testoutput_ejscreenit_50, 
  testoutput_ejscreenapi_plus_50, 
  
  testpoints_500,
  testoutput_ejscreenit_500,
  testoutput_ejscreenapi_plus_500
)
