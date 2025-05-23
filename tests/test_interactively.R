
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
                              useloadall = TRUE, # might be essential actually

                              y_basic = FALSE, y_latlon=TRUE, y_shp=TRUE, y_fips = TRUE,

                              y_runsome = FALSE, # if T, need to also create partial_testlist
                              tname = NULL,
                              # c("test_fips", "test_naics", "test_frs", "test_latlon", "test_maps",
                              # "test_shape", "test_getblocks", "test_fixcolnames", "test_doag",
                              # "test_ejamit", "test_ejscreenapi", "test_mod", "test_app", "test_test")

                              y_runall = TRUE,
                              y_stopif = FALSE,
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

  logfilename_only = paste0("testresults-",
                            gsub(" ", "_", gsub("\\.[0-9]{6}$", "", gsub(":", ".", as.character(Sys.time())))),
                            ".txt")

  if (!y_basic) {

    # !diagnostics off ## to disable diagnostics in this document
    #        thisfile = "./tests/test_interactively.R"
    # source(thisfile) to test the local source pkg, by group of functions, quietly, summarized
    # test_local()     to test the local source pkg
    # test_package()   to test installed version of package
    # test_check()     to test installed version of package, the way used by R CMD check or check()
    library(testthat)
    library(data.table) # used in functions here
    library(magrittr)
    library(dplyr)
    # consoleclear <- function() {if (interactive() & rstudioapi::isAvailable()) {rstudioapi::executeCommand("consoleClear")}}
    # consoleclear() is an undocumented internal function in the pkg now

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
          "test-getblocksnearbyviaQuadTree.R",
          "test-report_residents_within_xyz.R",
          "test-proxistat.R",
          "test-utils_indexpoints.R"
          #    -------------- NEEDS MORE TESTS? ***
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
          "test-doaggregate.R",
          "test-area_sqmi.R"
        ),
        test_ejamit = c(
          "test-ejamit.R",
          "test-ejam2barplot_sites.R",
          "test-ejamit_compare_distances.R",
          "test-ejamit_compare_types_of_places.R",
          "test-ejamit_sitetype_from_input.R",
          "test-ejamit_sitetype_from_output.R"
        ),

        test_misc = c(
          "test-acs_bybg.R"
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
          "test-FIPS-functionality.R", 
          "test-latlon-functionality.R", 
          "test-NAICS-functionality.R",
          "test-shp-gdb-zip-functionality.R",
          "test-shp-json-functionality.R",
          "test-shp-unzip-functionality.R",
          "test-shp-zip-functionality.R"
        ),
        test_test = c(
          # "test-test.R", #   fast way to check this script via  biglist <- test_interactively(ask = FALSE, y_runsome = T, tname = 'test')
          "test-test2.R",  #   fast way to check this script
          "test-test1.R"
        ),
        test_golem = c(
          "test-golem_utils_server.R", # not used
          "test-golem_utils_ui.R"      # not used
        )
      )

      # Seconds to run each testfile or group ####

      timebyfile <- structure(
        list(
          file = c(
            "test-latlon_df_clean.R", "test-latlon_from_anything.R",
            "test-latlon_from_vectorofcsvpairs.R", "test-address_xyz.R",
            "test-latlon_as.numeric.R", "test-latlon_from_address.R", "test-latlon_from_sic.R",
            "test-latlon_infer.R", "test-latlon_is.valid.R", "test-state_from_sitetable.R",
            "test-utils_metadata_add.R", "test-fixcolnames.R", "test-fixnames_to_type.R",
            "test-varinfo.R", "test-fixcolnames_infer.R", "test-fixnames.R",
            "test-ejscreenRESTbroker-functions.R", "test-ejscreenapi.R",
            "test-ejscreenapi1.R", "test-ejscreenapi_plus.R", "test-ejscreenit.R",
            "test-naics2children.R", "test-naics_categories.R", "test-naics_findwebscrape.R",
            "test-naics_from_code.R", "test-naics_from_name.R", "test-naics_subcodes_from_code.R",
            "test-naics_validation.R", "test-naics_from_any.R", "test-FIPS_FUNCTIONS.R",
            "test-is.numeric.text.R", "test-state_from_latlon.R", "test-state_from_fips_bybg.R",
            "test-MAP_FUNCTIONS.R", "test-FIPS-shiny-functionality.R", "test-NAICS-shiny-functionality.R",
            "test-latlon-shiny-functionality.R", "test-shapefile-shiny-functionality.R",
            "test-ui_and_server.R", "test-doaggregate.R", "test-pctile_from_raw_lookup.R",
            "test-ejamit_sitetype_from_output.R", "test-ejam2barplot_sites.R",
            "test-ejamit.R", "test-ejamit_compare_distances.R", "test-ejamit_compare_types_of_places.R",
            "test-ejamit_sitetype_from_input.R", "test-getblocks_summarize_blocks_per_site.R",
            "test-report_residents_within_xyz.R", "test-getblocksnearby.R",
            "test-getblocksnearby_from_fips.R", "test-getblocksnearbyviaQuadTree.R",
            "test-radius_inferred.R", "test-shapefile_xyz.R", "test-ejam2shapefile.R",
            "test-shape2zip.R", "test-shapes_from_fips.R", "test-frs_from_naics.R",
            "test-frs_from_programid.R", "test-frs_from_regid.R", "test-frs_from_sic.R",
            "test-frs_is_valid.R", "test-regid_from_naics.R", "test-golem_utils_server.R",
            "test-mod_save_report.R", "test-mod_specify_sites.R", "test-mod_view_results.R",
            "test-test1.R", "test-test2.R"),
          seconds_byfile = c(
            4.53999999999996,
            2.58999999999924, 2.72999999999956, 6.10000000000036, 2.36000000000058,
            4.5600000000004, 2.44000000000051, 2.53999999999996, 2.36999999999989,
            7.13999999999942, 2.48000000000047, 2.39999999999964, 2.15999999999985,
            2.21000000000004, 2.5, 2.38000000000011, 67.4399999999996, 6.57999999999993,
            7.82999999999993, 14.2999999999993, 12.9099999999999, 2.5, 2.34999999999945,
            7.80000000000018, 2.39999999999964, 2.50999999999931, 2.5, 2.43000000000029,
            3.03999999999996, 31.54, 2.59000000000015, 17.5900000000001,
            2.30000000000018, 20.1199999999999, 2.17000000000007, 2.25, 2.19999999999982,
            2.31999999999971, 2.72000000000025, 42.25, 2.55000000000018,
            10.9899999999998, 23.3200000000006, 31.4700000000003, 71.0799999999999,
            13.0899999999992, 2.25, 4.40999999999985, 2.18000000000029, 8.83999999999924,
            5.84000000000015, 7.72000000000025, 18.3400000000001, 6.96000000000004,
            3.90999999999985, 3.36999999999989, 4.53000000000065, 30.0600000000004,
            2.42000000000007, 2.5, 6.73999999999978, 2.64000000000033, 9.63000000000011,
            2.35999999999876, 2.28999999999996, 2.22000000000025, 2.26000000000022,
            2.19999999999891, 2.23999999999978)),
        class = "data.frame",
        row.names = c(NA, -69L))

      # timebygroup
      #            testgroup seconds_bygroup
      #               <char>           <num>
      #  1:      test_latlon              70
      #  2: test_fixcolnames              33
      #  3: test_ejscreenapi  ***        123
      #  4:       test_naics              52
      #  5:        test_fips              68
      #  6:        test_maps              22
      #  7:         test_app              29
      #  8:        test_doag              50
      #  9:      test_ejamit  ***        169
      # 10:   test_getblocks              64
      # 11:       test_shape              30shap
      # 12:         test_frs              72
      # 13:       test_golem              11
      # 14:         test_mod              16
      # 15:        test_test              11

      timebygroup = structure(list(
        testgroup = c("test_latlon", "test_fixcolnames", "test_ejscreenapi", "test_naics", "test_fips", "test_maps", "test_app",
                      "test_doag", "test_ejamit", "test_getblocks", "test_shape", "test_frs", "test_golem", "test_mod", "test_test"),
        seconds_bygroup = c(70, 33, 123, 52, 68, 22, 29, 50, 169, 64, 30, 72, 11, 16, 11)
      ),
      row.names = c(NA, -15L), class = c("data.table", "data.frame")
      )

      # data.table(timebyfile)
      #                                           file seconds_byfile
      #                                         <char>          <num>
      #  1:                     test-latlon_df_clean.R           4.54
      #  2:                test-latlon_from_anything.R           2.59
      #  3:        test-latlon_from_vectorofcsvpairs.R           2.73
      #  4:                         test-address_xyz.R           6.10
      #  5:                   test-latlon_as.numeric.R           2.36
      #  6:                 test-latlon_from_address.R           4.56
      #  7:                     test-latlon_from_sic.R           2.44
      #  8:                        test-latlon_infer.R           2.54
      #  9:                     test-latlon_is.valid.R           2.37
      # 10:                test-state_from_sitetable.R           7.14
      # 11:                  test-utils_metadata_add.R           2.48
      # 12:                         test-fixcolnames.R           2.40
      # 13:                    test-fixnames_to_type.R           2.16
      # 14:                             test-varinfo.R           2.21
      # 15:                   test-fixcolnames_infer.R           2.50
      # 16:                            test-fixnames.R           2.38
      # 17:        test-ejscreenRESTbroker-functions.R          67.44
      # 18:                         test-ejscreenapi.R           6.58
      # 19:                        test-ejscreenapi1.R           7.83
      # 20:                    test-ejscreenapi_plus.R          14.30
      # 21:                          test-ejscreenit.R          12.91
      # 22:                      test-naics2children.R           2.50
      # 23:                    test-naics_categories.R           2.35
      # 24:                 test-naics_findwebscrape.R           7.80
      # 25:                     test-naics_from_code.R           2.40
      # 26:                     test-naics_from_name.R           2.51
      # 27:            test-naics_subcodes_from_code.R           2.50
      # 28:                    test-naics_validation.R           2.43
      # 29:                      test-naics_from_any.R           3.04
      # 30:                      test-FIPS_FUNCTIONS.R          31.54
      # 31:                     test-is.numeric.text.R           2.59
      # 32:                   test-state_from_latlon.R          17.59
      # 33:                test-state_from_fips_bybg.R           2.30
      # 34:                       test-MAP_FUNCTIONS.R          20.12
      # 35:            test-FIPS-shiny-functionality.R           2.17
      # 36:           test-NAICS-shiny-functionality.R           2.25
      # 37:          test-latlon-shiny-functionality.R           2.20
      # 38:       test-shapefile-shiny-functionality.R           2.32
      # 39:                       test-ui_and_server.R           2.72
      # 40:                         test-doaggregate.R          42.25
      # 41:              test-pctile_from_raw_lookup.R           2.55
      # 42:         test-ejamit_sitetype_from_output.R          10.99
      # 43:                  test-ejam2barplot_sites.R          23.32
      # 44:                              test-ejamit.R          31.47
      # 45:            test-ejamit_compare_distances.R          71.08
      # 46:      test-ejamit_compare_types_of_places.R          13.09
      # 47:          test-ejamit_sitetype_from_input.R           2.25
      # 48: test-getblocks_summarize_blocks_per_site.R           4.41
      # 49:         test-report_residents_within_xyz.R           2.18
      # 50:                     test-getblocksnearby.R           8.84
      # 51:           test-getblocksnearby_from_fips.R           5.84
      # 52:          test-getblocksnearbyviaQuadTree.R           7.72
      # 53:                     test-radius_inferred.R          18.34
      # 54:                       test-shapefile_xyz.R           6.96
      # 55:                      test-ejam2shapefile.R           3.91
      # 56:                           test-shape2zip.R           3.37
      # 57:                    test-shapes_from_fips.R           4.53
      # 58:                      test-frs_from_naics.R          30.06
      # 59:                  test-frs_from_programid.R           2.42
      # 60:                      test-frs_from_regid.R           2.50
      # 61:                        test-frs_from_sic.R           6.74
      # 62:                        test-frs_is_valid.R           2.64
      # 63:                    test-regid_from_naics.R           9.63
      # 64:                  test-golem_utils_server.R           2.36
      # 65:                     test-mod_save_report.R           2.29
      # 66:                   test-mod_specify_sites.R           2.22
      # 67:                    test-mod_view_results.R           2.26
      # 68:                               test-test1.R           2.20
      # 69:                               test-test2.R           2.24
      #                                           file seconds_byfile

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


    # cat("\n\nAVAILABLE UNIT TEST FILES, IN GROUPS:\n\n")


    ## count of test per group ####
    count_available_files_bygroup = data.frame(groupnames = names(testlist),
                                               shortgroupnames = gsub("^test_(.*)","\\1", names((testlist))),
                                               filecount = sapply(testlist, length)
                                               #, `filenames as test-___.R` = as.vector(unlist(lapply(testlist, function(z) paste0(gsub("^test-|.R$", "", unlist(z)), collapse = ", "))))
    )
    rownames(count_available_files_bygroup) = NULL
    # print(testlist) # long list of vectors

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
                x <- try(
                  testthat::test_file(
                    file.path("./tests/testthat/", fnames[i]),
                    load_helpers = load_helpers,
                    load_package = 'none',
                    # or else  Helper, setup, and teardown files located in the same directory as the test will also be run. See vignette("special-files") for details.
                    reporter = reporter,
                    stop_on_failure = stop_on_failure
                  )
                )
                if (inherits(x, "try-error")) {cat("Stopped on failure in ", fnames[i], "\n")}
              }
              , print = print4eachfile) # here it is a useless param of capture_output_lines()
            }))

            ## old categorization
            # total = err_cant_test + tests.  tests =  passed or flag. flag = warning OR err. err = (skipped OR failed).
            # better categories:
            # total = untested_cant + untested_skipped + tested. tested = passed + warned + failed. flagged = untested_cant + untested_skipped + warned + failed. err=0.

            x <- as.data.frame(x)
            x$tests <- x$nb
            x$nb <- NULL
            x$flag <- x$tests - x$passed
            x$err  <- x$tests - x$passed - x$warning
            x$error_cant_test <- ifelse(x$error > 0, 1, 0)  ## a problem with counting this?
            x$error <- NULL
            x$skipped <- ifelse(x$skipped, 1, 0)

            x$err = NULL
            x$untested_skipped <- x$skipped; x$skipped = NULL
            x$untested_cant <- x$error_cant_test;  x$error_cant_test = NULL
            x$tested = x$tests - x$untested_skipped; x$tests = NULL
            x$total = x$untested_skipped + x$untested_cant + x$tested
            x$warned = x$warning; x$warning = NULL
            x$failed = x$tested - x$passed - x$warned
            x$flagged = x$untested_skipped + x$untested_cant + x$warned + x$failed; x$flag = NULL
            if (sum(x$total) != sum(x$passed + x$flagged)) {stop('math error in counts!')}

            x <- x[, c('file',  'test',
                       'total', 'passed', 'flagged',
                       'untested_cant', 'untested_skipped', 'warned', 'failed'
            )]

            # x <- x[, c('file',  'test',
            #            'tests', 'passed', 'failed',  'err',
            #            'warning', 'flag', 'skipped', 'error_cant_test'
            # )]

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
            colSums(xtable[, .(total, passed, flagged,
                               untested_cant, untested_skipped, warned, failed)]),
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
            colSums(xtable[[i]][, .(total, passed, flagged,
                                    untested_cant, untested_skipped, warned, failed)]),
            seconds = secs

          ))

          if (sum(xtable[[i]]$flagged) > 0) {
            # using beepr::beep() since utils::alarm() may not work
            # using :: might create a dependency but prefer that pkg be only in Suggests in DESCRIPTION
            if (interactive()) {beepr::beep(10)}
            cat(paste0("     ***      SOME UNTESTED OR WARNED OR FAILED IN ", tgroupname, ": ",  
                paste0(unique(xtable[[i]]$file[xtable[[i]]$flagged]), collapse = ","),
                "\n"))
          }

        } # looped over groups of test files

        xtable <- data.table::rbindlist(xtable)
        time_minutes <-   round(sum(xtable[ , (seconds_bygroup[1]) / 60, by = testgroup][, V1]) , 1)
        cat(paste0('\n', time_minutes[1], ' minutes total for all groups\n\n'))

        xtable[ , flagged_byfile := sum(flagged), by = "file"]
        xtable[ , failed_byfile  := sum(failed),  by = "file"]
        xtable[ , flagged_bygroup := sum(flagged), by = "testgroup"]
        xtable[ , failed_bygroup  := sum(failed),  by = "testgroup"]
        setorder(xtable, -failed_bygroup, -flagged_bygroup, testgroup, -failed, -flagged, file)
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
        if (!missing(tname)) {y_runsome <- TRUE}
        if ( missing(tname)) {y_runsome = askYesNo("Run ONLY SOME OF THE tests ?", default = FALSE)}
      }
      if (is.na(y_runsome))  {stop("cancelled")}
      if (y_runsome) {y_runall =  FALSE}
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
      if (missing(y_stopif)) {
        y_stopif = askYesNo("Halt when a test fails?")}
      if (is.na(y_stopif)) {stop("cancelled")}

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

    if (!missing(tname)) {y_runsome <- TRUE} # you specified some tests to run, so assume you meant to ignore the default y_runsome xxx
    if (y_runsome) {y_runall =  FALSE}
    if (y_runsome) {
      tname <- unlist(strsplit(gsub(" ", "", tname), ","))
      tname = paste0("test_", tname)
      #    test_file("./tests/testthat/test-MAP_FUNCTIONS.R" )
      partial_testlist <-  testlist[names(testlist) %in% tname]
    }
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

  # if did not ask and still have not defined mydir
  if (missing(mydir) && (!exists('mydir') || is.null(mydir))) {
    if (y_tempdir) {
      mydir <- tempdir()
    } else {
      mydir = '.'
    }
  }
  mydir <- normalizePath(mydir)
  logfilename = (  file.path(mydir, logfilename_only) )

  cat("Saving in ", logfilename, ' etc. \n')
# ~ ####
  ########################### #  ########################################## #
  # load_all() or library(EJAM) ####
  cat('\n')
  if (useloadall) {
    devtools::load_all()
  } else {
    suppressPackageStartupMessages({   library(EJAM)   })
  }
  cat("Downloading all large datasets that might be needed...\n")
  dataload_dynamic("all")
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
      junk = ejam2excel(x, save_now = F, launchexcel = T)

      ejam2report(x, analysis_title = "2 point latlon example")
      ejam2report(x, analysis_title = "2 point latlon example but selecting 1 site", sitenumber = 2)

      ejam2map(x) # no sitenumber param available
      # convert to shapefile of circles at points
      fname = ejam2shapefile(x, folder = tempdir())
      shpin = shapefile_from_any(fname)
      ejam2map(x, shp = shpin) # if shp is provided
      map_shapes_leaflet(shpin) # does not use nice EJAM popups
      cat("\n\n DONE WITH latlon CHECKS \n\n")
      x1 = x
    }

    if (y_shp) {
      # shapefile

      shp <- shape_buffered_from_shapefile( shapefile_from_sitepoints(testpoints_5[1:2,]), radius.miles = 1)
      # or use test data  shp <- shapefile_from_any()
      shp <- shapefile_from_any(
        system.file("testdata/shapes/portland_folder_shp/Neighborhoods_regions.shp", package = "EJAM")
      )[1:3, ]
      x3 <- ejamit( shapefile = shp, radius = 0 )
      names(x3)
      ejam2table_tall(x3)
      ejam2barplot(x3)
      ejam2barplot_sites(x3)
      ejam2tableviewer(x3 , fname = file.path(tempdir(), "ejam2tableviewer_3polygon_test.html")) # should be able to pick name
      junk = ejam2excel(x3, save_now = F, launchexcel = T)

      ejam2report(x3, analysis_title = "3 polygon portland example", shp = shp)
      ejam2report(x3, analysis_title = "3 polygon portland example, 1 site", shp = shp, sitenumber = 2)

      ejam2map(x3) # no latlon or geometry is in output of ejamit() here so just shows a point at each poly!!
      ejam2map(x3, shp = shp)  # if shp is provided, map works!

      # map_ejam_plus_shp(out = x3, shp = shp) # also works
      # tfile = ejam2shapefile(x3, folder = tempdir()) # no latlon or geometry is in output of ejamit() here
      shp3 = ejam2shapefile(x3, save = FALSE, shp = shp) # this also merges them but there are better ways above
      map_shapes_leaflet(shp3) # ugly popup but works
      cat("\n\n DONE WITH shp CHECKS \n\n")
      x1 = x3
    }

    if (y_fips) {
      # fips
      fipstest = fips_bgs_in_fips(fips_counties_from_state_abbrev("DE")[1])[1:2]
      x2 <- ejamit(fips = fipstest) # just 2 blockgroups
      names(x2)
      ejam2table_tall(x2)
      ejam2barplot(x2)
      ejam2barplot_sites(x2)
      ejam2tableviewer(x2)
      junk = ejam2excel(x2, save_now = F, launchexcel = T)

      ejam2report(x2)
      ejam2report(x2, sitenumber = 2)

      ejam2map(x2) # no latlon or geometry is in output of ejamit() but this does work!
      # ejam2map(x2, shp = shapes_from_fips(fipstest)) # not needed and replaces fips with id 1:N

      ejam2shapefile(x2, folder = tempdir()) # no latlon or geometry is in output of ejamit() here so this is not working for FIPS or shapefile analysis cases yet, except see  mapfastej_counties()
      x3b <- ejamit(fips = fips_counties_from_state_abbrev("DE"))  #   3 Counties
      mapfastej_counties(x3b$results_bysite) # not (x)
      cat("\n\n DONE WITH fips CHECKS \n\n")
      x1 = x3b
    }

    cat("Done with basic checks. Not doing any other testing. \n\n")
    invisible(x1)
  } # halts if this gets done - just y_basic done.
  ########################### #  ########################################## #
  ########################### #  ########################################## #


  ########################### #  ########################################## #

  # try to do this once here and not in setup.R
  # out_api ####
  if (exists("out_api" , envir = globalenv() )) {
    cat("Using the copy of out_api that already is in globalenv() so if that is outdated you should halt and do rm(out_api) now\n")
  } else {
    if (!ejscreenapi_online()) {
      cat("API URL does not seem to be accessible \n")
    } else {
      cat("Creating out_api in the globalenv(), using ejscreenapi()\n\n")
      test2lat <- c(33.943883,    39.297209)
      test2lon <- c(-118.241073, -76.641674)
      pts <- data.frame(lat = test2lat, lon = test2lon)
      testradius = 1
      out_api       <- ejscreenapi(lon = test2lon, lat = test2lat, radius = testradius, on_server_so_dont_save_files = TRUE, save_when_report = FALSE)
      assign(x = "out_api", out_api, envir = globalenv())
    }
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
    if (y_runsome) {y_runall =  FALSE}
    shownlist = partial_testlist
    shownlist = cbind(testgroup = rep(names(shownlist), sapply(shownlist, length)), file = unlist(shownlist))
    rownames(shownlist) = NULL

    cat("\n USING THESE TEST FILES: \n\n")

    print(shownlist); cat('\n\n')

    secs1 = sum(timebygroup$seconds_bygroup[timebygroup$testgroup %in% shownlist[, 'testgroup']])
    mins1 = round(secs1 / 60, 1)

    cat("Predicted time to run tests is roughly", mins1, "minutes. Very rough estimate of ETA: ")

    print(Sys.time() + secs1)
    cat("\n\n")
    #
    # fnames = as.vector(unlist(shownlist))
    # secs2 = 1.3 * sum(timebyfile$seconds_byfile[timebyfile$file %in% fnames])
    # mins2 = round(secs2 / 60, 1)
    # cat("Predicted time to run tests is roughly", mins2, "minutes. Very rough estimate of ETA: ")
    # print(Sys.time() + secs2)
    # cat("\n\n")

    x <- testbygroup(testlist = partial_testlist, stop_on_failure = y_stopif)
    bytest <- x

    junk = loggable(file = logfilename, x = {

      cat("-------------------------------------------------- \n")
      cat("\n")
      cat("           TEST RESULTS AS OF "); cat(as.character(Sys.Date()), '\n')

      # cat("\n                            RESULTS THAT FAILED/ WARNED/ CANT RUN     \n\n")

      if (any(x$flagged  > 0)) {
        # print(as.data.frame(x)[x$flagged  > 0, !grepl("byfile|bygroup", names(x))])
      } else {
        cat("All selected tests ran and passed.")
      }
      cat("\n")
    })
    ########################### #
    ## save results of some testing ####
    if (y_seeresults) {
      # will do save of everything after summarizing results
    } else {
      if (y_save) {
        fname <- paste0("results_of_some_unit_testing_", as.character(Sys.Date()), ".rda")
        fname = (  file.path(mydir, fname) )
        save(bytest, file = fname)
        junk = loggable(file = logfilename, x = {

          cat('\n  See', fname, ' for results of some unit testing.\n\n')
        })
      } # end if - save
    }
  }
  ########################### #  ########################################## #
  ########################### #  ########################################## #

  # RUN ALL TESTS (slow)  ####

  if (y_runall) {

    z <- system.time({

      shownlist = testlist
      shownlist = cbind(testgroup = rep(names(shownlist), sapply(shownlist, length)), file = unlist(shownlist))
      rownames(shownlist) = NULL
      cat("\n USING THESE TEST FILES: \n\n")
      print(shownlist); cat('\n\n')

      secs1 = sum(timebygroup$seconds_bygroup[timebygroup$testgroup %in% shownlist[, 'testgroup']])
      mins1 = round(secs1 / 60, 1)
      cat("Predicted time to run tests is roughly", mins1, "minutes. Very rough estimate of ETA: ")
      print(Sys.time() + secs1)
      cat("\n\n")
      #
      # fnames = as.vector(unlist(shownlist))
      # secs2 = 1.3 * sum(timebyfile$seconds_byfile[timebyfile$file %in% fnames])
      # mins2 = round(secs2 / 60, 1)
      # cat("Predicted time to run tests is roughly", mins2, "minutes. Very rough estimate of ETA: ")
      # print(Sys.time() + secs2)
      # cat("\n\n")

      rm(shownlist)

      x <- testbygroup(testlist = testlist, stop_on_failure = y_stopif)
      bytest <- x

    })
    junk = loggable(file = logfilename, x = {

      cat("-------------------------------------------------- \n")
      cat("\n")
      cat("           TEST RESULTS AS OF "); cat(as.character(Sys.Date()), '\n')

      # cat("\n                            RESULTS THAT FAILED/ WARNED/ CANT RUN     \n\n")

      if (any(x$flagged > 0)) {
        # print(as.data.frame(x)[x$flagged > 0, !grepl("byfile|bygroup", names(x))])
      } else {
        cat("All selected tests ran and passed.\n")
      }
      cat("\n")
    })
    ########################### #
    ## save results of all testing ####
    if (y_seeresults) {
      # will do save of everything after summarizing results
    } else {
      # y_save = askYesNo("Save results of unit testing?")
      if (is.na(y_save)) {stop("cancelled")}
      if (y_save) {
        fname <- paste0("results_of_unit_testing_", as.character(Sys.Date()), ".rda")
        fname = (  file.path(mydir, fname) )
        save(bytest, file = fname)
        junk = loggable(file = logfilename, x = {
          cat('\n  See', fname, ' for full results of unit testing.\n\n')
        })
      } # end if - save
    }
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

      cat("\n")

      cat("COUNT PASS / FAIL \n\n")

      passcount = colSums(x[, .(total, passed, flagged,   untested_cant, untested_skipped, warned, failed)])
      print(passcount)
      cat("\n")

      cat("PERCENT PASS / FAIL ")

      cat("\n\n")
      passpercent = round(100 * colSums(x[, .( total, passed, flagged,   untested_cant, untested_skipped, warned, failed )])
                          / sum(x$total), 1)
      print(passpercent)
      ########################### #  ########################### #

      ## KEY GROUPS - WHICH TEST GROUPS or FILES HAVE THE MOST FAILING TESTS?

      bygroup <- x[ , .(total = sum(total), passed = sum(passed), flagged = sum(flagged),
                        untested_cant = sum(untested_cant), untested_skipped = sum(untested_skipped), warned = sum(warned), failed = sum(failed),
                        seconds_bygroup = seconds_bygroup[1]),
                    by = "testgroup"]
      cat("\n")

      cat("GROUPS OF FILES")

      cat("\n\n")
      print(bygroup)  # show even if no issues arose
      ########################### #  ########################### #

      ## WHICH FILES HAVE THE MOST FAILING TESTS?

      byfile <- x[ , .(
        flagged_byfile = flagged_byfile[1],    #    total, passed, flagged,   untested_cant, untested_skipped, warned, failed
        flagged_bygroup = flagged_bygroup[1],
        failed_byfile = failed_byfile[1],
        failed_bygroup = failed_bygroup[1],
        testgroup = testgroup[1]
      ),
      by = "file"]
      setorder(byfile, -failed_bygroup, -flagged_bygroup, testgroup, failed_byfile, -flagged_byfile, file)
      setcolorder(byfile, neworder = c("testgroup", "failed_bygroup", "flagged_bygroup", "file", "failed_byfile", "flagged_byfile"))
      byfile_key <- byfile[flagged_byfile > 0, ]
      cat("\n\n")
      if (NROW(byfile_key) == 0) {
        cat("No files had any tests with issues\n\n")
      } else {

        cat("KEY FILES")

        cat("\n\n")
        print(as.data.frame(byfile_key)[ , !grepl("_bygroup", names(byfile_key))])
      }
      ########################### #

      # WHICH TESTS?

      bytest_key = x[order(-x$flagged, -x$failed), ]
      these = bytest_key$flagged > 0
      if (any(these)) {
        bytest_key <- bytest_key[these, ]
        cat("\n\n")

        cat("KEY TESTS")

        cat("\n\n")
        print(as.data.frame(bytest_key)[ , !grepl("_byfile|_bygroup", names(bytest_key))])
        cat("\n")
      } else {
        cat("\n")
        cat("No tests had issues\n\n")
        bytest_key = NA
      }
      ########################### #

      # show how to open key files

      if (NROW(byfile_key) != 0) {
        topfilenames <- as.data.frame(byfile_key)
        topfilenames = topfilenames[order(topfilenames$failed_byfile, topfilenames$flagged_byfile, decreasing = TRUE), ]
        topfilenames = topfilenames$file[topfilenames$flagged_byfile > 0]
        if (length(topfilenames) > 0) {
          topfilenames <- topfilenames[1:min(5, length(topfilenames))]

          cat("\nTO OPEN SOME KEY TEST FILES FOR EDITING, FOR EXAMPLE:\n" ,

              paste0("rstudioapi::navigateToFile('./tests/testthat/", topfilenames, "')", collapse = "\n "),
              "\n\n")
          # rstudioapi::navigateToFile("./tests/testthat/test-doaggregate.R")
          # rstudioapi::navigateToFile("./tests/testthat/test-ejamit.R")
          # rstudioapi::navigateToFile("./tests/testthat/test-latlon_df_clean.R")
        }
      }

    }) # end loggable
  } # end of big if - viewing results
  ########################### #  ########################################## #

  # COMPILE ALL RESULTS IN A LIST

  if (!exists("bytest")) {bytest <- NA}

  totalseconds = sum(x[ , seconds_bygroup[1], by = "testgroup"][,V1])
  totalminutes = round(totalseconds / 60, 1)

  biglist <- list(
    minutes = totalminutes,
    passcount = passcount,    #      total, passed, flagged,   untested_cant, untested_skipped, warned, failed
    passpercent = passpercent,
    bygroup = bygroup,
    byfile = byfile,
    bytest_key = bytest_key,
    bytest_all = bytest,
    folder = mydir,
    count_available_files_bygroup = count_available_files_bygroup,
    params = params
  )
  # SAVE results + summary ####
  if (y_save) {
    fname <- paste0("results_SUMMARY_of_unit_testing_", as.character(Sys.Date()), ".rda")
    fname = (file.path(mydir, fname))
    save(biglist, file = fname)

    cat(paste0('\nSaved results here: \n  "', fname, '" \n\n'))
  }
  loggable(file = logfilename, x = {
    cat(paste0(
      totalminutes, ' minutes elapsed running these tests',
      ' (finished at ', as.character(Sys.time()), ')\n'))
  })

  if (interactive()) {
    browseURL(mydir) # open folder in file explorer / finder
    if (rstudioapi::isAvailable()) {
      # view the file
      rstudioapi::navigateToFile(logfilename)
    }
  }
  if (interactive()) {beepr::beep()} # utils::alarm() may not work

  invisible(
    biglist
  )
} # end of function
################################### #  ################################### #  ################################### #
# ~ ####
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
cat('

# examples of using it ####

x <- test_interactively(F)  # no questions, just defaults


x <- test_interactively()   # it asks questions


x <- test_interactively(F, mydir = rstudioapi::selectDirectory())

')
################################### #  ################################### #  ################################### #
