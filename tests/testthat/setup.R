############################### #
cat("Starting setup.R for testing \n")

# # This script gets run before any test, so fixtures created here will be available to all the tests.
# The file already does library(EJAM) and that should do .onAttach() and dataload_from_pins() and indexblocks()

# When tests try to test the shiny app, the app should handle doing source(system.file("global.R", package = "EJAM")).

############################### #
# keep track of global envt side effects ####
# Keep track and alert us if any functions in tests have
#  changed global options, a side effect we probably want functions to avoid

set_state_inspector(function() {
  list(options = options())
})
############################### #
# internet available? ####
offline_warning("NO INTERNET CONNECTION AVAILABLE - SOME TESTS MAY FAIL WITHOUT CLEAR EXPLANATION")
offline_cat("\n\nNO INTERNET CONNECTION AVAILABLE - SOME TESTS MAY FAIL WITHOUT CLEAR EXPLANATION\n\n")
# skip_if_offline()

################################## #
# GET DATA AND BUILD INDEX JUST IN CASE ####
# to run tests interactively, you also need to do
# require(testthat)
# require(data.table)
# require(magrittr)
require(mapview)

#   require(EJAM) ############## does testthat already attach or load the package?
##  if you use require or library, then it does not have access to internal functions like latlon_infer()
## so those tests fail unless you use load_all() or if test were changed to say EJAM:::latlon_infer() but that would ONLY test installed version, never the source version if it differs

suppressMessages({suppressWarnings({
  dataload_from_pins("all", silent = TRUE, folder_local_source = file.path(.libPaths()[1],'EJAM','data')) # needs frs, etc.
})})
if (!exists("frs")) {stop('needs frs etc.')}
suppressMessages({suppressWarnings({
  indexblocks()
})})


## needs these? from global?
# default_hide_advanced_settings
# html_header_fmt

############################### #
# Create ejamoutnow here in setup.R, since some tests are using it. ####

if (exists("ejamit") & exists("blockgroupstats") & exists("testpoints_10")) {
  if (!exists("ejamoutnow")) {
  cat("creating ejamoutnow in setup.R\n")
    suppressMessages(  suppressWarnings({  ejamoutnow <- try(
      ejamit(testpoints_10, radius = 1,
             quiet = TRUE, silentinteractive = TRUE,
             include_ejindexes = TRUE)
      ) # include_ejindexes = FALSE was the default but we want to test with them included
    }))
  }
  # NOTE THE DEFAULT VALUES OF ejamit() !
  
} else {
  warning("missing ejamit() or blockgroupstats, so using pre-calculated results in tests")
  if (exists("testoutput_ejamit_10pts_1miles")) {
    ejamoutnow <- testoutput_ejamit_10pts_1miles
  } else {
    stop("cannot run tests - see file setup.R")
  }
}

############################### #
## Create some test cases we can use for inputs error checking: ####

bad_numbers <- list(
  num0len          = numeric(0L),  # these might be OK
  matrix_1x1       = matrix(1),    #
  array1           = array(1),     #
  NA1              = NA,
  NULL1            = NULL,
  TRUE1            = TRUE, # these  might be acceptable if you need a single number, for some functions, since can do math/ they could be coerced
  text1            = "1",
  character1       = "A",
  list1            = list(1),
  listempty        = list(),
  df1              = data.frame(1),
  vector2          = 1:2,
  array2           = array(1:2),
  matrix_1row_4col = matrix(1:4, nrow = 1),
  matrix_4row_1col = matrix(1:4, nrow = 4),
  matrix_2x2       = matrix(1:4, nrow = 2)
)

### to look at this list of objects:

#nix <- sapply(1:length(bad_numbers), function(z) {cat( "\n\n\n------------------------\n\n  ", names(bad_numbers)[z], "\n\n\n" ); print( bad_numbers[z][[1]] )}); rm(nix)

## to look at which ones are length >1, numeric, atomic, or ok to use in math:

# x <- data.frame(
#   length0or1 = sapply(bad_numbers, function(z) try(length(z) < 2)),
#   isnumeric  = sapply(bad_numbers, function(z) try(is.numeric(z))),
#   isatomic   = sapply(bad_numbers, function(z) try(is.atomic( z))),
#   canadd     = sapply(bad_numbers, function(z) try(is.numeric(z + 9)))
#   )
# x
# rm(x)
############################### #

# some test data to use for ejscreenapi1(), ejscreenapi(), ejscreenRESTbroker(), ejscreenRESTbroker2table() 
testradius <- 1
testlat <-  38.8959  # testpoints_50$lat[1]
testlon <- -77.02985 # testpoints_50$lon[1]
test2lat <- c(33.943883,    39.297209)
test2lon <- c(-118.241073, -76.641674)
pts <- data.frame(lat = test2lat, lon = test2lon)

## now done only in one test file to avoid repeating it each time setup.R is run, in test-ejscreenit.R
# apinow_list <- ejscreenit(testpoints_5, radius = 1, nosave = T, nosee = T, interactiveprompt = F, calculate_ratios = T) # defaults to verbose=FALSE via ejscreenapi_plus() ?
# apinow = apinow_list$table
# apinow$timeSeconds <- NULL # these vary
# apinow$`Seconds elapsed obtaining data` <- NULL

## some test output from ejscreenit 
# SLOW FOR API to run several points

apiref_list <- testoutput_ejscreenit_5 # 5 points, 1 mile radius
# apinow_list <- ejscreenit(testpoints_5, radius = 1, nosave = T, nosee = T, interactiveprompt = F, calculate_ratios = T)
apiref = apiref_list$table
# apinow = apinow_list$table
apiref$timeSeconds <- NULL # these vary
# apinow$timeSeconds <- NULL # these vary
apiref$`Seconds elapsed obtaining data` <- NULL
# apinow$`Seconds elapsed obtaining data` <- NULL

# outrest       <- ejscreenRESTbroker(lon = testlon, lat = testlat, radius = testradius)
# outrest2table <- ejscreenRESTbroker2table(outrest, getstatefromplacename = TRUE)
# out1          <- ejscreenapi1(lon = testlon,  lat = testlat, radius = testradius) # CAN SOMETIMES TAKE 30 SECONDS, SOMETIMES 5 SECONDS

if (!exists("out_api", envir = globalenv())) { # should be there if test_interactively() was used 
  cat("creating out_api in setup.R\n")
  # this might speed up testing & make console output less verbose
  # where setup was being sourced over and over again by manual_nonalphabetical.R
  suppressMessages({
    junk <- capture_output({
      out_api       <- ejscreenapi(lon = test2lon, lat = test2lat, radius = testradius,
                                   verbose = TRUE,
                                   on_server_so_dont_save_files = TRUE, save_when_report = FALSE)
    })
    # x <- try(ejscreenRESTbroker(lon = testpoints_5$lon[1], lat = testpoints_5$lat[1], radius = testradius))
    # missing_api_results <- inherits(x, "try-error")
  })
}
############################### #
# >>> cleanup after testing?? #### 
# # Run after all tests
# # Setup code is typically best used to create external resources that are needed by many tests. It’s best kept to a minimum because you will have to manually run it before interactively debugging tests.
# # But, is this right?  it is from the help example but what is cleanup() ?? ***
# # Needs to be fixed:
#
# withr::defer(cleanup(), teardown_env())




#############################################################################  #
## Notes: to profile parts of the shiny app for performance, etc.
## see shinytest2 package, and see profiler:
# shiny::callModule(profvis_server, "profiler")
## and also see  /EJAM/tests/testthat/test-ui_and_server.R
## and see  https://shiny.posit.co/r/articles/improve/debugging/
## etc.
