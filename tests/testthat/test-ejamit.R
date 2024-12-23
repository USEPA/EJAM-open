
# test_file("./tests/testthat/test-ejamit.R")

########################################################## #
# see test-ejamit_compare_distances.R  for test of "donuts ok in ejamit(radius_donut_lower_edge=3)"

########################################################## #

test_that('ejamit() returns a list with no error, for very simple example', {
  # no crash for basic example
  expect_no_error({
    suppressWarnings({
      
      suppressMessages({
        v10 <- ejamit(testpoints_10, radius = 1, quiet = T, silentinteractive = TRUE) # same as  ejamoutnow <- ejamit(testpoints_10, radius = 1) done in  setup.R, but tested here. - takes roughly 5-10 seconds
      })
    })
  })
  expect_true('list' %in% class(v10))
})


test_that("ejamit() returns no distances greater than radius - even if maxradius parameter not specified", {
  max_specified <- 3
  suppressWarnings(
    suppressMessages({
      v10 <- ejamit(sitepoints = testpoints_10, radius = max_specified, quiet = T, silentinteractive = TRUE)
    })
  )
  max_found <- max(v10$results_bysite$radius.miles)
  expect_lte(
    max_found,
    max_specified
  )
  
})


# expect_identical(NROW(val), NROW(EJAM::testpoints_10))
### only if update ejamit to return blank rows where latlon invalid or no blocks so no results for that point



########################################################## #

################# #

test_that('ejamit() output has names the same as it used to return, i.e. names(testoutput_ejamit_10pts_1miles)', {
  suppressWarnings(suppressMessages({
    v10 <- ejamit(sitepoints = testpoints_10, radius = 1, quiet = T, silentinteractive = TRUE)
  }))
  expect_identical(
    names(v10),
    names(testoutput_ejamit_10pts_1miles)
  )
  expect_equal(
    c("results_overall", "results_bysite", "results_bybg_people", "longnames",
      "count_of_blocks_near_multiple_sites", "results_summarized", "formatted", "sitetype"),
    names(v10))
})
################# #

test_that("ejamit() still returns results_overall identical to what it used to return
          (saved as testoutput_ejamit_10pts_1miles$results_overall)", {
            suppressWarnings({
              suppressMessages({
                ejamoutnow <- ejamit(testpoints_10, radius = 1, quiet = T, silentinteractive = TRUE)  #  - takes roughly 5-10 seconds
                
                expect_identical(
                  ejamoutnow$results_overall,
                  testoutput_ejamit_10pts_1miles$results_overall
                )
              } )
            })
            # all.equal(ejamoutnow$results_overall,
            #           testoutput_ejamit_10pts_1miles$results_overall)
          })
################# #

test_that("ejamit() still returns results_bysite identical to expected numbers it used to return
          (saved as testoutput_ejamit_10pts_1miles$results_bysite)", {
            suppressWarnings({
              suppressMessages({
                ejamoutnow <- ejamit(testpoints_10, radius = 1, quiet = T, silentinteractive = TRUE) # see setup.R - takes roughly 5-10 seconds
                expect_identical(
                  ejamoutnow$results_bysite,
                  testoutput_ejamit_10pts_1miles$results_bysite,
                  ignore_attr = T
                )
                # all.equal(    ejamoutnow$results_bysite,
                #               testoutput_ejamit_10pts_1miles$results_bysite)
              } )
            })
          })
################# #

test_that("ejamit() returns same exact colnames() in both results_bysite and results_overall", {
  ejamoutnow <- ejamit(testpoints_10, radius = 1, quiet = T, silentinteractive = TRUE) # see setup.R - takes roughly 5-10 seconds
  expect_identical(
    colnames(ejamoutnow$results_bysite),
    colnames(ejamoutnow$results_overall)
  )
})
########################################################## #




# more tests for ejamit go here

testthat::test_that("ejamit can use fips=fips_counties_from_statename()", {
  testthat::expect_no_error({
    suppressWarnings(
      suppressWarnings({
        y <- ejamit(fips = fips_counties_from_statename("Delaware"), quiet = TRUE, silentinteractive = TRUE, in_shiny = F)
      })
    )
  })
})

# ***

############################### # ############################### # ############################### # ############################### #
############################### # ############################### # ############################### # ############################### #


# more tests for ejscreenit go here ? 


# # ***
#
# maybe
# create a test data set that is
# testoutput_ejscreenit_10pts_1miles  so
# testoutput_ejscreenit_10pts_1miles$table  can be compared to
# testoutput_ejamit_10pts_1miles$results_bysite[, 4:ncol(testoutput_ejamit_10pts_1miles$results_bysite)]
#


################################ #




