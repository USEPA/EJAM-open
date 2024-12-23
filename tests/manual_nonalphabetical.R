
#  test_interactively()

#' run group(s) of unit tests, interactively or not, get compact summary
#' asks user in RStudio what tests to run and how
#' @param ask logical, whether it should ask in RStudio what parameter values to use
#' @param noquestions logical, whether to avoid questions later on about where to save shapefiles
#' @param useloadall logical, TRUE means use load_all(), FALSE means use library()
#' @param y_basic logical, whether to only run some basic ejamit() functions, not do unit tests
#' @param y_latlon logical, if y_basic=T, whether to run the basic ejamit() using points
#' @param y_shp logical, if y_basic=T, whether to run the basic ejamit() using shapefile
#' @param y_fips logical, if y_basic=T, whether to run the basic ejamit() using FIPS
#' @param y_runsome logical, whether to run only some groups of tests (so y_runall is FALSE)
#' @param tname if y_runsome = T, a vector of group names like 'fips', 'naics', etc.
#'   see source code for list 
#' @param y_runall logical, whether to run all tests instead of only some groups
#'   (so y_runsome is FALSE)
#' @param y_seeresults logical, whether to show results in console
#' @param y_save logical, whether to save files of results
#' @param y_tempdir logical, whether to save in tempdir
#' @param mydir optional folder
#' @examples
#' \dontrun{
#' biglist1 <- test_interactively()
#' biglist2 <- test_interactively(ask = F, 
#'       y_runsome = T, tname = c('test', 'maps'),  
#'       mydir = "~/../Downloads/unit testing")
#'       }
#' 
#' @return a named list of objects like data.tables, e.g., named 
#'   'bytest', 'byfile', 'bygroup', 'params', 'passcount' and other summary stats, etc.
#'
#' @keywords internal
#'
test_interactively = function(ask = TRUE, 
                              noquestions = TRUE, # just for shapefile folder selections
                              useloadall = TRUE,
                              
                              y_basic = FALSE, y_latlon=TRUE, y_shp=TRUE, y_fips = TRUE,
                              
                              y_runsome = FALSE, # if T, need to also create partial_testlist
                              tname = NULL, 
                              # c("test_fips", "test_naics", "test_frs", "test_latlon", "test_maps", 
                              # "test_shape", "test_getblocks", "test_fixcolnames", "test_doag", 
                              # "test_ejamit", "test_ejscreenapi", "test_mod", "test_app", "test_test")
                              
                              y_runall = TRUE,
                              
                              y_seeresults = TRUE,
                              y_save = TRUE,
                              y_tempdir = TRUE,
                              mydir = NULL
) {
  
  # to make a sound when an error is hit and when it finishes
  if (interactive()) {require(beepr)} # using beepr::beep(10) since utils::alarm() may not work 
  
  ########################################## # ########################################## # 
  if (missing(y_basic) & ask) {
    if (missing(y_basic)) {
      y_basic = askYesNo("Do ONLY basic quick checks (no unit tests, then STOP) ?", default = y_basic)
    }}
  if (is.na(y_basic)) {stop("cancelled")}
  if (y_basic) {
    if (missing(y_latlon) & ask) {y_latlon = askYesNo("quick tests for latlon?", default = y_latlon)}
    if (is.na(y_latlon)) {stop("cancelled")}
    if (missing(y_shp)    & ask) {y_shp    = askYesNo("quick tests for shp?",    default = y_shp)}
    if (is.na(y_shp))    {stop("cancelled")}
    if (missing(y_fips)   & ask) {y_fips   = askYesNo("quick tests for fips?",   default = y_fips)}
    if (is.na(y_fips))   {stop("cancelled")}
  }
  # if only doing basic non-unit-testing then do not ask about other details and do not find groups of test files, etc. - 
  #  just skip way ahead to load/library and do those quick checks
  
  ########################################## # ########################################## # 
  # Setup ####
  
  if (!y_basic) {
    
    # !diagnostics off ## to disable diagnostics in this document
    #        thisfile = "./tests/manual_nonalphabetical.R"
    # source(thisfile) to test the local source pkg, by group of functions, quietly, summarized
    # test_local()     to test the local source pkg
    # test_package()   to test installed version of package
    # test_check()     to test installed version of package, the way used by R CMD check or check()
    library(testthat)
    library(data.table) # used in functions here
    library(magrittr)
    library(dplyr)
    consoleclear <- function() {if (interactive() & rstudioapi::isAvailable()) {rstudioapi::executeCommand("consoleClear")}}
    # consoleclear()
    
    ########################################## #
    
    ## FIND tests ####
    update_list_of_tests <- TRUE
    if (update_list_of_tests) {
      sdir <- getwd()
      test_files_found <-  basename(list.files(path = file.path(sdir, "tests/testthat"), full.names = TRUE, pattern = "test-"))
      ########################################## # 
      
      # GROUP the tests ####
      
      testlist = list( 
        
        test_fips = c(
          "test-FIPS_FUNCTIONS.R",
          "test-state_from_fips_bybg.R",  
          "test-state_from_latlon.R",
          "test-is.numeric.text.R"
        ),
        test_naics = c(
          "test-naics_categories.R",   
          "test-naics_findwebscrape.R", 
          "test-naics_from_any.R",   
          "test-naics_from_code.R",  
          "test-naics_from_name.R",   
          "test-naics_subcodes_from_code.R",
          "test-naics_validation.R",  
          "test-naics2children.R"    
        ),
        test_frs = c(
          "test-regid_from_naics.R", 
          "test-frs_from_naics.R",    
          "test-frs_from_programid.R",  
          "test-frs_from_regid.R",  
          "test-frs_from_sic.R",   
          "test-frs_is_valid.R"   
        ),
        test_latlon = c(
          "test-latlon_infer.R",        
          "test-latlon_as.numeric.R",
          "test-latlon_df_clean.R",
          "test-latlon_is.valid.R",   
          "test-latlon_from_anything.R",   
          "test-latlon_from_sic.R",
          "test-address_xyz.R", 
          "test-latlon_from_address.R",
          "test-latlon_from_vectorofcsvpairs.R",
          "test-state_from_sitetable.R"
        ),
        test_maps = c(
          "test-MAP_FUNCTIONS.R"
        ),
        test_shape = c(
          "test-shapefile_xyz.R",
          "test-shapes_from_fips.R",
          "test-ejam2shapefile.R",
          "test-shape2zip.R"
        ),
        test_getblocks = c(
          "test-radius_inferred.R",              # this is SLOW THOUGH
          "test-getblocks_summarize_blocks_per_site.R",  
          "test-getblocksnearby.R",        
          "test-getblocksnearby_from_fips.R", 
          "test-getblocksnearbyviaQuadTree.R"    #    -------------- NEEDS MORE TESTS ***
        ),
        test_fixcolnames = c(
          "test-fixcolnames.R",
          "test-fixnames.R",
          "test-fixnames_to_type.R",
          "test-fixcolnames_infer.R",
          "test-varinfo.R",
          "test-utils_metadata_add.R"
        ),
        test_doag = c(
          "test-pctile_from_raw_lookup.R",  
          "test-doaggregate.R"     
        ),
        test_ejamit = c(
          "test-ejamit.R",
          "test-ejam2barplot_sites.R",
          "test-ejamit_compare_distances.R",
          "test-ejamit_compare_types_of_places.R",
          "test-ejamit_sitetype_from_input.R",
          "test-ejamit_sitetype_from_output.R"
        ),
        test_ejscreenapi = c(
          "test-ejscreenapi.R",
          "test-ejscreenapi_plus.R",
          "test-ejscreenapi1.R",
          "test-ejscreenit.R",
          "test-ejscreenRESTbroker-functions.R"
        ),
        test_mod = c(
          "test-mod_save_report.R",    
          "test-mod_specify_sites.R",  
          "test-mod_view_results.R"    
        ),
        test_app = c(
          #"test-report_residents_within_xyz.R",  # maybe belongs in a separate group about reports/tables?
          "test-ui_and_server.R",
          "test-FIPS-shiny-functionality.R", "test-latlon-shiny-functionality.R", "test-NAICS-shiny-functionality.R", "test-shapefile-shiny-functionality.R"
        ),
        test_test = c(
          "test-test.R"#, #   fast way to check this script via  biglist <- test_interactively(ask = FALSE, y_runsome = T, tname = 'test')
          #"test-test2.R",  #   fast way to check this script
          #"test-test.R"
        ),
        test_golem = c(
          "test-golem_utils_server.R", # not used
          "test-golem_utils_ui.R"      # not used
        )
      )
      ########################################## # 
      # groupnames <- names(testlist)
      test_all <- as.vector(unlist(testlist))
      ########################################## # 
      ## confirm all grouped ####
      {
        
        if (!all(TRUE == all.equal(sort(test_all), sort(test_files_found)))) {
          cat("\n\n   test files found in folder does not match test_files_found list  \n")
          print(all.equal(sort(test_all), sort(test_files_found))) 
          cat("\n\n")
        }
        
        if (length(setdiff(test_files_found, test_all)) > 0) {
          cat("These are in test folder as files but not in list of groups above: \n\n")
          print(setdiff(test_files_found, test_all))
          cat("\n")
          stop("fix list of files")
        }
        
        if (length(setdiff(test_all, test_files_found)) > 0) {
          cat("These are in list of groups above but not in test folder as files: \n\n")
          print(setdiff(test_all, test_files_found))
          cat("\n")
          stop("fix list of test files")
        }
        
        if (any(duplicated(test_all))) {
          cat("some are listed >1 group\n")
          stop("some are listed >1 group")
        }
        
        cat("\n\n")
        ########################################## # 
      }
    } # end if, update_list_of_tests
    
    
    cat("\n\nAVAILABLE UNIT TEST FILES, IN GROUPS:\n\n")
    
    
    ## count of test per group ####
    count_available_files_bygroup = data.frame(groupnames = names(testlist),
                                               shortgroupnames = gsub("^test_(.*)","\\1", names((testlist))), 
                                               filecount = sapply(testlist, length)
                                               #, `filenames as test-___.R` = as.vector(unlist(lapply(testlist, function(z) paste0(gsub("^test-|.R$", "", unlist(z)), collapse = ", "))))
    )
    rownames(count_available_files_bygroup) = NULL
    print(testlist) # long list of vectors
    
    cat("\n   COUNTS OF AVAILABLE FILES IN EACH GROUP OF TESTS\n\n")
    print(count_available_files_bygroup)
    cat("\n")
    { #          groupnames shortgroupnames filecount
      # 1         test_fips            fips         3
      # 2        test_naics           naics         8
      # 3          test_frs             frs         6
      # 4       test_latlon          latlon        10
      # 5         test_maps            maps         1
      # 6        test_shape           shape         3
      # 7    test_getblocks       getblocks         5
      # 8  test_fixcolnames     fixcolnames         6
      # 9         test_doag            doag         2
      # 10      test_ejamit          ejamit         6
      # 11 test_ejscreenapi     ejscreenapi         5
      # 12         test_mod             mod         3
      # 13         test_app             app         5
      # 14        test_test            test         1
      # 15       test_golem           golem         2
      # fnames = unlist(testlist)
    }        
    shortgroupnames = gsub("^test_(.*)","\\1", names((testlist)))
    
    ## define Functions that run tests ####
    ########################### #      ########################### #
    { 
      ##     TO TEST 1 GROUP  (WITH SUCCINCT SUMMARY)
      
      ## examples
      # x1 = test1group(c("test-test1.R", "test-test2.R"), groupname = 'test', print4group = F   )
      # x2 = test1group(c("test-test1.R", "test-test2.R"), groupname = 'test', print4group = TRUE)
      # print(x1)
      # print(x2)

      
      test1group <- function(fnames = test_all, groupname = "",
                             reporter = "minimal", # some of the code below now only works if using this setting
                             load_helpers = TRUE,
                             print4eachfile = FALSE, # useless - keep it FALSE
                             print4group = TRUE,
                             add_seconds_bygroup = TRUE,
                             stop_on_failure = FALSE
      ) {
        
        xtable <- list()
        # tfile <- tempfile("junk", fileext = "txt")
        # timing = system.time({
        for (i in 1:length(fnames)) {
          seconds_byfile = system.time({
            cat(paste0("#", i, ' '))
            # cat(".") ## like a counter, one dot per file
            
            suppressWarnings(suppressMessages({
              junk <- testthat::capture_output_lines({
                x <- testthat::test_file(
                  file.path("./tests/testthat/", fnames[i]), 
                  load_helpers = load_helpers,
                  load_package = 'none',
                  # or else  Helper, setup, and teardown files located in the same directory as the test will also be run. See vignette("special-files") for details.
                  reporter = reporter,
                  stop_on_failure = stop_on_failure
                )
              }
              , print = print4eachfile) # here it is a useless param of capture_output_lines()
            }))
            
            x <- as.data.frame(x)
            x$tests <- x$nb
            x$nb <- NULL
            x$flag <- x$tests - x$passed
            x$err  <- x$tests - x$passed - x$warning
            x$error_cant_test <- ifelse(x$error > 0, 1, 0)  ## a problem with counting this?
            x$error <- NULL
            x$skipped <- ifelse(x$skipped, 1, 0)
            x <- x[, c('file',  'test', 
                       'tests', 'passed', 'failed',  'err',
                       'warning', 'flag', 'skipped', 'error_cant_test'
            )]
            x$test <- substr(x$test, 1, 50) # some are long
            xtable[[i]] <- data.table::data.table(x)
          })
          xtable[[i]]$seconds_byfile <- seconds_byfile['elapsed']
        }
        # })
        xtable <- data.table::rbindlist(xtable)
        
        seconds_bygroup <- round(sum(xtable[ , seconds_byfile[1], by = 'file'][,V1]), 0)
        ## can add this shorter time estimate to the results instead of relying on 
        ## the slightly longer time estimate that can be done in testbygroup() 
        if (add_seconds_bygroup) {
          xtable[ , seconds_bygroup := seconds_bygroup]
        }
        cat('done. ')
        cat(' Finished test group', groupname, 'in', seconds_bygroup, 'seconds.\n')
        if (print4group) {
          # print a table of counts
          print(c(
            colSums(xtable[, .(tests, passed, failed, err,
                               warning, flag, 
                               skipped, error_cant_test)]),
            seconds_bygroup = seconds_bygroup
          ))
        }
        
        return(xtable)
      }
      ########################### #      ########################### #
      
      ##     TO LOOP THROUGH GROUPS of tests
      
      ## examples
      #
      # y1 <- testbygroup( list(
      # test_test  = c("test-test1.R", "test-test2.R"),
      # test_golem = c("test-golem_utils_server.R", "test-golem_utils_ui.R")),
      # testing = TRUE
      # )
      # y2 <- testbygroup( list(
      #   test_test  = c("test-test1.R", "test-test2.R"),
      #   test_golem = c("test-golem_utils_server.R", "test-golem_utils_ui.R")),
      #   testing = FALSE,
      #   print4group = FALSE
      # )
      # y3 <- testbygroup( list(
      #   test_test  = c("test-test1.R", "test-test2.R"),
      #   test_golem = c("test-golem_utils_server.R", "test-golem_utils_ui.R")),
      #   testing = FALSE,
      #   print4group = TRUE # probably repeating printouts if  do this
      # )
      # print(y1)
      # print(y2)
      # print(y3)
      
      
      testbygroup <- function(testlist, 
                              print4group = FALSE,
                              testing = FALSE,
                              stop_on_failure = FALSE,
                              reporter = "minimal" # this may be the only option that works now
      ) {
        # probably cannot now, but used to be able to use  reporter=default_compact_reporter()
        
        xtable <- list()
        
        i <- 0
        for (tgroupname in names(testlist)) {
          seconds_bygroup_viasystemtime = system.time({
            i <- i + 1
            if (i == 1) {load_helpers <- TRUE} else {load_helpers <- FALSE}
            fnames = unlist(testlist[[tgroupname]])
            cat("", tgroupname, "group has", length(fnames), "test files. Starting ")
            
            xtable[[i]] <- data.table::data.table(
              
              testgroup = tgroupname,
              
              test1group(testlist[[tgroupname]], 
                         groupname = tgroupname,
                         load_helpers = load_helpers,
                         print4group = print4group,
                         stop_on_failure = stop_on_failure,
                         add_seconds_bygroup = TRUE, #   can be done here by testbygroup() not by test1group()
                         reporter = reporter)
            )
          })
          
          ## time elapsed
          ##
          ## This is the total time including overhead of looping, using test1group() for each group, and compiling.
          secs1 <- round(seconds_bygroup_viasystemtime['elapsed'], 0)
          if (testing) {
            cat('Seconds elapsed based on testbygroup() using system.time() is', secs1, '\n')
            # other ways fail if no test happened in a file like for group golem:
            ## This is a slightly shorter timing estimate could be done in test1group() by using add_seconds_bygroup=T 
            secs2 <- round(xtable[[i]]$seconds_bygroup[1], 0)
            cat('Seconds elapsed based on testbygroup() reporting total reported by test1group() is', secs2, '\n')
            ## or, a similar estimate could be done here, but just like it would be in test1group() :
            secs3 <- round(sum(xtable[[i]][ , seconds_byfile[1], by = 'file'][,V1]), 0)
            cat('Seconds elapsed based on testbygroup() summing seconds_byfile once per file is', secs3, '\n')
          }
          secs <- secs1
          xtable[[i]]$seconds_bygroup <- secs # replaces any estimate done by test1group()
          
          # cat(paste0( '', round(secs, 0), ' seconds elapsed.\n'))
          ## That appears on same line where test1group() had already said "Finished test group xyz"
          ## or, previously, complete phrase here: # cat(paste0(' ', tgroupname, ' group finished, in ', round(secs, 0), ' seconds.\n\n'))
          
          ## Show table of counts for this group of files of tests:
          print(c(
            colSums(xtable[[i]][, .(tests, passed, failed, err,
                                    warning, flag,
                                    skipped, error_cant_test)]),
            seconds = secs
          ))
          
          if (sum(xtable[[i]]$flag) > 0) {
            # using beepr::beep() since utils::alarm() may not work
            # using :: might create a dependency but prefer that pkg be only in Suggests in DESCRIPTION
            if (interactive()) {beepr::beep(10)}
            cat("\n NOT ALL PASSED IN", tgroupname, "\n\n")
          }
          
        } # finished this one group of test files
        
        xtable <- data.table::rbindlist(xtable)
        time_minutes <-   round(sum(xtable[ , (seconds_bygroup[1]) / 60, by = testgroup][, V1]) , 1)
        cat(paste0('\n', time_minutes[1], ' minutes total for all groups\n\n'))
        
        xtable[ , flag_byfile := sum(flag), by = "file"]
        xtable[ , err_byfile  := sum(err),  by = "file"]
        xtable[ , flag_bygroup := sum(flag), by = "testgroup"]
        xtable[ , err_bygroup  := sum(err),  by = "testgroup"]
        setorder(xtable, -err_bygroup, testgroup, -flag, -failed, file)
        setcolorder(xtable, neworder = c('seconds_bygroup', 'seconds_byfile'), after = NCOL(xtable))
        
        return(xtable)
      }
    }   #   done defining functions
    #################################################### # 
    ########################### #  ########################################## #
    
    # >>Ask what to do<< ####
    
    # *** THIS SECTION ASKS ABOUT tname SO IT USES THE LATEST LIST OF TESTS FOUND to ask which ones to use, to know what the options are,
    # WHICH IS WHY THESE QUESTIONS ARE ASKED ONLY AFTER FINDING AND GROUPING TESTS
    
    if (y_runsome) {y_runall =  FALSE} # in case you want to say y_runsome = T and not have to also remember to specify y_runall = F
    
    if (interactive() & ask) {
      
      if (missing(useloadall)) {
        useloadall <- askYesNo(msg = "Do you want to load and test the current source code files version of EJAM (via devtools::load_all() etc.,
                      rather than testing the installed version)?", default = TRUE)
      }
      if (missing(y_runsome)) {
        y_runsome = askYesNo("Run ONLY SOME OF THE tests ?", default = FALSE)}
      if (is.na(y_runsome))  {stop("cancelled")}
      if (y_runsome) {
        if (missing(tname)) { 
          tname = rstudioapi::showPrompt(
            "WHICH TEST OR GROUPS COMMA-SEP LIST",
            paste0(shortgroupnames, collapse = ","),
            #e.g., "fips,naics,frs,latlon,maps,shape,getblocks,fixcolnames,doag,ejamit,ejscreenapi,mod,app"
          )
        }
        
        y_runall <- FALSE
      } else {
        if (missing(y_runall)) {
          y_runall = askYesNo("RUN ALL TESTS NOW?")}
        if (is.na(y_runall)) {stop("cancelled")}
      }
      if (missing(y_seeresults)) {
        y_seeresults = askYesNo("View results of unit testing?")}
      if (is.na(y_seeresults))  {stop("cancelled")}
      if (missing(y_save)) {
        y_save = askYesNo("Save results of unit testing (and log file of printed summaries)?")}
      if (is.na(y_save)) {stop("cancelled")}
      if (y_save) {
        if (missing(y_tempdir) & missing(mydir)) {
          y_tempdir = askYesNo("OK to save in a temporary folder you can see later? (say No if you want to specify a folder)")}
        if (is.na(y_tempdir)) {stop("cancelled")}
        if (y_tempdir & missing(mydir)) {
          mydir <- tempdir()
        } else {
          if (missing(mydir)) {
            mydir <- rstudioapi::selectDirectory()}
        }
      }
    }
    
    if (missing(mydir) && (!exists('mydir') || is.null(mydir))) {
      if (y_tempdir) {
        mydir <- tempdir()
      } else {
        mydir = '.'
      }
    }
    mydir <- normalizePath(mydir)
    if (y_runsome) {
      tname <- unlist(strsplit(gsub(" ", "", tname), ","))
      tname = paste0("test_", tname)
      #    test_file("./tests/testthat/test-MAP_FUNCTIONS.R" )
      partial_testlist <-  testlist[names(testlist) %in% tname] 
    }
    
    logfilename_only = paste0("testresults-", 
                              gsub(" ", "_", gsub("\\.[0-9]{6}$", "", gsub(":", ".", as.character(Sys.time())))), 
                              ".txt")
    logfilename = (  file.path(mydir, logfilename_only) )
    cat("Saving in ", logfilename, ' etc. \n')
    ################################### #  ################################### #
    if (y_runall == FALSE && y_runsome == FALSE) {
      stop('no tests run')
    } else {
      noquestions <- TRUE
      # if (interactive() & ask & (y_runall | ("test_shape" %in% names(testlist)))) {
      #   # ***  note if interactive it normally tries to prompt for shapefile folder in some cases  
      #   if (missing(noquestions)) {
      #     if (askYesNo("run tests where you have to interactively specify folders for shapefiles?")) {
      #       noquestions <- FALSE
      #     }  else {
      #       noquestions <- TRUE
      #     }
      #   } else {
      #     # noquestions  was given as a parameter
      #   }}
    }
  } # end if not just basic
  # finished asking what to do and setting up
  
  ########################### #  ########################################## #  
  # load_all() or library(EJAM) ####
  cat('\n')
  if (useloadall) {
    devtools::load_all()
  } else {
    suppressPackageStartupMessages({   library(EJAM)   }) 
  }
  cat("Downloading all large datasets that might be needed...\n")
  dataload_from_pins("all")
  ## should happen later in the function test1group() via testbygrou
  # if (file.exists("./tests/testthat/setup.R")) {
  #   # rstudioapi::navigateToFile("./tests/testthat/setup.R")
  #   source("./tests/testthat/setup.R") #   asks if need load_all or library
  # } else {
  #   cat("Need to source the setup.R file first \n")    
  # }
  ########################### #  ########################################## #
  
  # RUN BASIC QUICK CHECKS NOT UNIT TESTS   ####
  # for easy/basic case, main functions, without actually running unit tests with testthat
  
  if (y_basic) {
    
    if (y_latlon) {
      # latlon
      x <- ejamit(testpoints_5[1:2,], radius = 1)
      # names(x)
      ejam2table_tall(x)
      ejam2barplot(x)
      ejam2barplot_sites(x)
      ejam2tableviewer(x)
      junk = ejam2excel(x, save_now = F, launchexcel = T) # map and report were missing if not in shiny app
      # ejam2report(x)   #  report not yet working if not in shiny app
      ejam2map(x)
      fname = ejam2shapefile(x, folder = tempdir())
      shpin = shapefile_from_any(fname)
      map_shapes_leaflet(shpin)
      cat("\n\n DONE WITH latlon CHECKS \n\n")
    }
    
    if (y_shp) {
      # shapefile
      
      shp <- shape_buffered_from_shapefile( shapefile_from_sitepoints(testpoints_5[1:2,]), radius.miles = 1)
      # or use test data  shp <- shapefile_from_any()
      shp <- shapefile_from_any(system.file("testdata/shapes/portland_folder_shp/Neighborhoods_regions.shp", package = "EJAM"))[1:3, ]  
      x <- ejamit( shapefile = shp )  
      names(x)
      ejam2table_tall(x)
      ejam2barplot(x)
      ejam2barplot_sites(x)
      ejam2tableviewer(x , fname = file.path(tempdir(), "ejam2tableviewer_test.html")) # should be able to pick name
      junk = ejam2excel(x, save_now = F, launchexcel = T) # map and report were missing if not in shiny app
      ejam2report(x,)   #  report not yet working if not in shiny app
      ejam2map(x) # no latlon or geometry is in output of ejamit() here so this is not working for FIPS or shapefile analysis cases yet, except see  mapfastej_counties()
      ejam2shapefile(x, folder = tempdir()) # no latlon or geometry is in output of ejamit() here so this is not working for FIPS or shapefile analysis cases yet, except see  mapfastej_counties()
      map_shapes_leaflet(x)
      cat("\n\n DONE WITH shp CHECKS \n\n")
    }
    
    if (y_fips) {
      # fips
      x <- ejamit(fips = fips_bgs_in_fips(fips_counties_from_state_abbrev("DE")[1])[1:2]) # just 2 blockgroups
      names(x)
      ejam2table_tall(x)
      ejam2barplot(x)
      ejam2barplot_sites(x)
      ejam2tableviewer(x)
      junk = ejam2excel(x, save_now = F, launchexcel = T) # map and report were missing if not in shiny app
      ejam2report(x)   #  report not yet working if not in shiny app
      ejam2map(x) # no latlon or geometry is in output of ejamit() here so this is not working for FIPS or shapefile analysis cases yet, except see  mapfastej_counties()
      ejam2shapefile(x, folder = tempdir()) # no latlon or geometry is in output of ejamit() here so this is not working for FIPS or shapefile analysis cases yet, except see  mapfastej_counties()
      
      x <- ejamit(fips = fips_counties_from_state_abbrev("DE"))  #   3 Counties
      mapfastej_counties(x$results_bysite) # not (x)
      cat("\n\n DONE WITH fips CHECKS \n\n")
    }
    
    cat("Done with basic checks. Not doing any other testing. \n\n")
    invisible(x)
  } # halts if this gets done - just y_basic done.
  ########################### #  ########################################## #
  ########################### #  ########################################## #
  
  
  ########################### #  ########################################## #
  
  # try to do this once here and not in setup.R 
  # out_api ####
  if (exists("out_api" , envir = globalenv() )) {
    cat("Using the copy of out_api that already is in globalenv() so if that is outdated you should halt and do rm(out_api) now\n")
  } else {
    cat("Creating out_api in the globalenv(), using ejscreenapi()\n\n")
    test2lat <- c(33.943883,    39.297209)
    test2lon <- c(-118.241073, -76.641674)
    pts <- data.frame(lat = test2lat, lon = test2lon)
    testradius = 1
    out_api       <- ejscreenapi(lon = test2lon, lat = test2lat, radius = testradius, on_server_so_dont_save_files = TRUE, save_when_report = FALSE)
    assign(x = "out_api", out_api, envir = globalenv())
  }  
  ########################### #  ########################################## #
  
  # log file started ####
  
  # cat("\n\nStarted testing at", as.character(Sys.time()), '\n')
  
  junk = loggable(file = logfilename, x = {
    cat(logfilename_only, '\n ---------------------------------------------------------------- \n\n')
    cat("Started at", as.character(Sys.time()), '\n')
    
    if (is.null(tname)) {tnameprint = NA} else {
      tnameprint = paste0(tname, collapse = ',')
    }
    
    ## summary of input parameters ####
    # get current values
    paramslist <- list()
    for (i in 1:length(formalArgs(test_interactively))) {
      paramslist[[i]] <- get(formalArgs(test_interactively)[i])
    }
    names(paramslist) <- formalArgs(test_interactively)
    paramslist$tname <- paste0(paramslist$tname, collapse = ",") # easier to view
    params <- paramslist
    ## same as spelling them out:
    # params = list(ask =  ask,
    #               noquestions  =  noquestions,
    #               useloadall   =  useloadall,
    #               y_basic      =  y_basic,
    #               y_latlon     =  y_latlon,
    #               y_shp        =  y_shp,
    #               y_fips       =  y_fips,
    #               y_runsome    =  y_runsome,  
    #               tname        =  paste0(tname, collapse = ","),
    #               y_runall     =  y_runall,
    #               y_seeresults =  y_seeresults,  
    #               y_save       =  y_save,  
    #               y_tempdir    =  y_tempdir,
    #               mydir        =  mydir 
    # )
    paramsdefaults <- formals(test_interactively)
    params_summary = data.frame(
      default = cbind(paramsdefaults),
      current = cbind(params)
    )
    colnames(params_summary) <- c('default', 'current')
    cat("\nParameters (options) being used: \n")
    print(params_summary)
    cat("\n")
    
    # cat("\nParameters (options) being used:
    # 
    #     ask          = ", ask, " 
    #     noquestions  = ", noquestions, " 
    #     useloadall   = ", useloadall, "  
    #     
    #     y_basic      = ", y_basic, "
    #       y_latlon     = ", y_latlon, "
    #       y_shp        = ", y_shp, "
    #       y_fips       = ", y_fips, "
    #     
    #     y_runsome    = ", y_runsome, "  
    #       tname        = ", tnameprint, "
    #     
    #     y_runall     = ", y_runall, "  
    #     
    #     y_seeresults = ", y_seeresults, "  
    #     y_save       = ", y_save, "  
    #     mydir        = ", "[not shown here]" , "
    #     "
    # )
  })
  ########################### #  ########################################## #
  ########################### #  ########################################## #
  
  # RUN JUST 1 FILE OR GROUP ####
  
  if (y_runsome) {
    
    x <- testbygroup(testlist = partial_testlist)
    bytest <- x
    
    junk = loggable(file = logfilename, x = {
      
      cat("TEST RESULTS AS OF "); cat(as.character(Sys.Date()))
      
      cat("\n\n                            RESULTS THAT FAILED/ WARNED/ CANT RUN     \n\n")
      
      if (any(x$flag + x$error_cant_test > 0)) {
        print(x[x$flag + x$error_cant_test > 0,])
      } else {
        cat("All selected tests ran and passed.")
      }
      cat("\n\n")
    })
    ########################### # 
    ## save results of some testing ####
    
    if (y_save) {
      fname <- paste0("results_of_some_unit_testing_", as.character(Sys.Date()), ".rda")
      fname = (  file.path(mydir, fname) )
      save(bytest, file = fname)
      junk = loggable(file = logfilename, x = {
        
        cat('\n  See', fname, ' for results of some unit testing.\n\n') 
      })
    } # end if - save
    
  }
  ########################### #  ########################################## #
  ########################### #  ########################################## #
  
  # RUN ALL TESTS (slow)  ####
  
  if (y_runall) {
    
    z <- system.time({
      
      x <- testbygroup(testlist = testlist)
      bytest <- x
      
    })
    junk = loggable(file = logfilename, x = {
      
      cat("TEST RESULTS AS OF "); cat(as.character(Sys.Date()))
      
      cat("\n\n                            RESULTS THAT FAILED/ WARNED/ CANT RUN     \n\n")
      
      if (any(x$flag + x$error_cant_test > 0)) {
        print(x[x$flag + x$error_cant_test > 0,])
      } else {
        cat("All selected tests ran and passed.")
      }
      cat("\n\n")
    })
    ########################### # 
    ## save results of all testing ####
    
    # y_save = askYesNo("Save results of unit testing?") 
    if (is.na(y_save)) {stop("cancelled")}
    if (y_save) {
      fname <- paste0("results_of_unit_testing_", as.character(Sys.Date()), ".rda")
      fname = (  file.path(mydir, fname) )
      save(bytest, file = fname)
      junk = loggable(file = logfilename, x = {
        # cat("TEST RESULTS AS OF "); cat(as.character(Sys.Date()))
        # cat("\n\n")
        # print(cbind(`PERCENT OF ALL TESTS` = round(100 * colSums(bytest[,4:11]) / 1125, 1)))
        # cat("\n\n")
        cat('\n  See', fname, ' for full results of unit testing.\n\n') 
      })
    } # end if - save
  }
  ########################### #  ########################################## #
  ########################### #  ########################################## #
  
  # SUMMARIZE results ####
  
  # y_seeresults = askYesNo("View results of unit testing?") 
  if (is.na(y_seeresults))  {stop("cancelled")}
  if (y_seeresults) {
    # consoleclear()
    ########################### #  ########################### #
    junk <- loggable(file = logfilename, x = {
      
      # HOW MANY TOTAL PASS/FAIL?
      
      cat("\n\n\n")
      cat("COUNT PASS / FAIL \n\n")
      passcount = colSums(x[, .( err, warning, tests, passed, failed, warning, flag, skipped, error_cant_test)])
      print(passcount)
      cat("\n")
      cat("PERCENT PASS / FAIL ")
      cat("\n\n")
      passpercent = round(100 * colSums(x[, .( err, warning, tests, passed, failed, warning, flag, skipped, error_cant_test)])
                          / sum(x$tests), 1)
      print(passpercent)
      
      # about 1.6 % of unit tests have errors   as of 7/1/24   ***********
      # (and ~1% have warnings)   ***********
      ########################### #  ########################### #
      
      ## KEY GROUPS - WHICH TEST GROUPS or FILES HAVE THE MOST FAILING TESTS?
      
      bygroup <- x[ , .(err = sum(err), warning = sum(warning), flag = sum(flag), 
                        passed = sum(passed), tests = sum(tests), 
                        seconds_bygroup = seconds_bygroup[1]), by = "testgroup"]
      cat("\n\n")
      cat("GROUPS OF FILES")
      cat("\n\n\n")
      print(bygroup)
      ########################### #  ########################### #
      
      ## WHICH FILES HAVE THE MOST FAILING TESTS?
      
      byfile <- x[ , .(
        err_byfile = err_byfile[1], 
        err_bygroup = err_bygroup[1],
        testgroup = testgroup[1]
      ), 
      by = "file"]
      setorder(byfile, -err_bygroup, testgroup, -err_byfile, file)
      setcolorder(byfile, neworder = c("testgroup", "err_bygroup", "file", "err_byfile"))
      byfile_key <- byfile[err_byfile > 0, ]
      cat("\n\n")
      if (NROW(byfile_key) == 0) {
        cat("No test files had errors.\n\n")
      } else { 
        cat("KEY FILES")
        cat("\n\n")
        print(byfile_key)
        
        topfilenames <- as.data.frame(byfile_key)
        topfilenames = topfilenames[order(topfilenames$err_byfile, decreasing = TRUE), ]
        topfilenames = topfilenames$file[topfilenames$err_byfile > 0]
        if (length(topfilenames) > 0) {
          topfilenames <- topfilenames[1:min(5, length(topfilenames))]
          cat("
TO OPEN SOME KEY TEST FILES FOR EDITING, FOR EXAMPLE:
  
" ,
              paste0("rstudioapi::navigateToFile('./tests/testthat/", topfilenames, "')", collapse = "\n "),
              "

")
          # rstudioapi::navigateToFile("./tests/testthat/test-doaggregate.R")
          # rstudioapi::navigateToFile("./tests/testthat/test-ejamit.R")
          # rstudioapi::navigateToFile("./tests/testthat/test-latlon_df_clean.R")
        }
      }
      ########################### #
      
      # WHICH TESTS? 
      
      cat("\n\n\n")
      cat("KEY TESTS")
      cat("\n\n")
      
      bytest_key = x[order(-x$flag, -x$failed), 1:11]
      these = bytest_key$tests != bytest_key$passed | bytest_key$error_cant_test > 0
      if (any(these)) {
        bytest_key <- bytest_key[these, ]
        print(bytest_key)
        cat("\n\n")  
      } else {
        bytest_key = NA
      }
      
    }) # end loggable
  } # end of big if - viewing results
  ########################### #  ########################################## #
  if (!exists("bytest")) {bytest <- NA}
  
  totalseconds = sum(x[ , seconds_bygroup[1], by = "testgroup"][,V1])
  totalminutes = round(totalseconds / 60, 1)  
  
  biglist <- list(
    minutes = totalminutes,
    passcount = passcount,
    passpercent = passpercent,
    bygroup = bygroup, 
    byfile = byfile,
    bytest_key = bytest_key,
    bytest_all = bytest, 
    folder = mydir,
    count_available_files_bygroup = count_available_files_bygroup,
    params = params
  )
  # SAVE results ####
  if (y_save) {
    fname <- paste0("results_SUMMARY_of_unit_testing_", as.character(Sys.Date()), ".rda")
    fname = (file.path(mydir, fname))
    save(biglist, file = fname)
    cat(
      '\n saved ', fname, ' \n\n'
    )
  }
  loggable(file = logfilename, x = {
    cat("Finished at", as.character(Sys.time()), '\n')
    cat(paste0(totalminutes, ' minutes total time for tests\n'))
  })
  
  if (interactive()) {
    browseURL(mydir) # open folder in file explorer / finder
    if (rstudioapi::isAvailable()) {
      # view the file
      rstudioapi::navigateToFile(logfilename) 
    }
  }
  
  cat("Finished at", as.character(Sys.time()), '\n')
  
  if (interactive()) {beepr::beep(10)} # utils::alarm() may not work 
  invisible(
    biglist
  )
} # end of function
################################### #  ################################### #  ################################### #

loggable <- function(x, file = 'will be created using timestamp if not provided and !exists(logfilename)', 
                     append = TRUE, split = TRUE, 
                     y_save_param=NULL) {
  
  if (missing(y_save_param)) {
    if (!exists('y_save')) {
      if (is.null(file)) {
        y_save <- FALSE
      } else {
        y_save <- TRUE
      }
    }
  } else {
    y_save <- y_save_param
  }
  
  if (y_save) {
    if (missing(file)) {
      if (exists('logfilename')) {
        file = logfilename
      } else {
        mydir = tempdir()
        file = paste0("testresults-", 
                      gsub(" ", "_", gsub("\\.[0-9]{6}$", "", gsub(":", ".", as.character(Sys.time())))), 
                      ".txt")
        file = (  file.path(mydir, file) )
      }
    }
    if (is.null(file)) {
      warning("file got set to NULL so NOT saving even though y_save was TRUE.")
    }
  } else {
    if (missing(file)) {
      file = NULL
    } else {
      if (!is.null(file)) {
        warning('file got specified so WILL save even though y_save was FALSE.')
      }
    }
  }
  
  capture.output(x, file = file, append = append, split = split) # this is supposed to print to console and to log file, but...
  
  # cat('\n  Adding to ', file, ' log of results of unit testing.\n\n')
  
  # use file = logfilename  or file = NULL  to override whatever the y_save value was when func was defined
  # file = NULL  will show only in console and not log it
  # split=T  will show output in console, and save to file simultaneously unless file=NULL
  
  ### how to use it    ## example 
  # ## y_save = F will prevent logging unless you also specify a file
  # junk = loggable(file = logfilename, x = {
  #   })
  
  # junk = loggable(file = logfilename, x = { 
  #   # comments do not get logged
  #   #  x  or  1 + 1  is not logged without print() or cat() ?
  #   print(cbind(a=1:3,b=2:4))
  #   cbind(c = 1:3, d = 2:4)
  #   x = 56
  #   print(x)
  #   cat(1234,'\n\n')
  #   
  #   }) 
  ## use file = logfilename  or file = NULL  to override whatever the y_save value is

}
################################### # 
# example of using it ####
#  biglist <- test_interactively()
## or
# mydir = "~/../Downloads/unit testing"
# biglist <- test_interactively(ask = F, mydir=mydir)


################################### #  ################################### #  ################################### #
