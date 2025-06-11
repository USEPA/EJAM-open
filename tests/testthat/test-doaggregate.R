############################################################################## #

## unit tests for EJAM::doaggregate
## Author: MC
## Initial set of tests checks function works with good inputs, and
## correctly handles bad table input or bad radius input.
##
## More tests needed, to check handling of other input parameters.


if (!exists('blockwts')) {
  stop('tests cannot run without blockwts dataset being loaded')
}
################# #  ################# #  ################# #


################# #
# WHAT IF INPUTS ARE ALL VALID - DOES FUNCTION WORK AT ALL? ####
################# #

# no crash when aggregate basic example of sites2blocks
test_that('doaggregate() returns a correctly named list, with no error if key params provided', {
  expect_no_error({
    suppressWarnings({
      val <- doaggregate(sites2blocks = testoutput_getblocksnearby_10pts_1miles,
                         sites2states_or_latlon = testpoints_10, 
                         radius = max(testoutput_getblocksnearby_10pts_1miles$distance), include_ejindexes = TRUE)
    })
  })
  expect_true('list' %in% class(val))
  expect_identical(
    names(val),
    c("results_overall", "results_bysite", "results_bybg_people",
      "longnames", "count_of_blocks_near_multiple_sites")
  )
})
################# #  ################# #  ################# #

################# #
# DOES IT STILL RETURN WHAT IT USED TO, OR HAS FUNCTION CHANGED SO THAT OUTPUTS NO LONGER MATCH ARCHIVED OUTPUTS? ####
################# #
test_that("still same exact results_overall as previously saved", {
  
  # # data created/saved was this:
  # out_data_doagg <- doaggregate(out_data_getblocks, sites2states_or_latlon = testpoints_data, radius = myrad, include_ejindexes = TRUE) # not the default but want to test this way
  
  suppressWarnings({
    # WHAT IT RETURNS NOW:
    x <- doaggregate(testoutput_getblocksnearby_10pts_1miles, 
                     sites2states_or_latlon = testpoints_10, 
                     radius = 1, include_ejindexes = TRUE)
    overall_has_changed <- !isTRUE(all.equal(
      testoutput_doaggregate_10pts_1miles$results_overall,
      x$results_overall))
  })
  expect_equal(
    testoutput_doaggregate_10pts_1miles$results_overall,
    x$results_overall # use defaults
  )
  skip_if(overall_has_changed, "not testing all outputs of doaggregate against archived since results_overall test failed")
  # overall_has_changed
  
  expect_equal(
    testoutput_doaggregate_10pts_1miles$results_bysite,
    x$results_bysite # use defaults
  )
  expect_equal(
    testoutput_doaggregate_10pts_1miles$results_bybg_people,
    x$results_bybg_people # use defaults
  )
  expect_equal(
    testoutput_doaggregate_10pts_1miles$longnames,
    x$longnames # use defaults
  )
  rm(x)
})


################# #
# WHAT IF TABLE INPUT IS CLEARLY BAD ####
################# #

test_that('error if in inputs are null, empty, NA, or blank',{
  expect_warning(doaggregate(NULL, silentinteractive = TRUE))
  expect_warning(doaggregate(NA, silentinteractive = TRUE))
  expect_error(doaggregate())
  expect_warning({
    x <- doaggregate('', silentinteractive = TRUE)
    })
  expect_true(is.null(x))
})

test_that('warn but no error if input is data.frame but not data.table (?)', {
  df <- data.table::setDF(  data.table::copy(testoutput_getblocksnearby_10pts_1miles) )
  suppressWarnings(
    expect_no_error(doaggregate(df))
  )
  suppressWarnings(
    expect_warning({
      x <- doaggregate(df)
      })
  )
  expect_true("results_overall" %in% names(x))
})

test_that('error if input has column not named distance', {
  wrongnames <- data.table::copy(testoutput_getblocksnearby_10pts_1miles)
  data.table::setnames(wrongnames, 'distance', 'radius')
  suppressWarnings({
    expect_warning({
      suppressMessages({
        junk = capture_output({
      x <- doaggregate(sites2blocks = wrongnames)
      })
    })
  })
  })
  expect_true("results_overall" %in% names(x))
})

###############################################  ##
# TESTS TO ADD, FOR HANDLING OF MISSING or various values for  param  sites2states_or_latlon
#
# This case never arises if using shiny app  or ejamit
# 
# testthat::test_that("doaggregate() handles missing sites2states_or_latlon", {
#   expect_error({
#     x = doaggregate(sites2blocks = testoutput_getblocksnearby_10pts_1miles,
#                     radius = 1) 
#     })
# })
# doaggregate(testpoints_10[1:2,], radius = 1)




# *** WHAT IF OTHER BAD FORMATS FOR TABLE? SEE bad_numbers examples from setup.R, as used in radius tests below.

cat('still need to test cases where input table is some other invalid format\n')





# *** WHAT IF TABLE INPUT EXCEEDS SOME SIZE LIMIT? TOO MANY ROWS; TOO MANY COLUMNS; TOO MANY MEGABYTES?

cat('still need to test cases where input table is valid class, type, but too many rows or columns\n')





################# #  ################# #  ################# #

################# #
# WHAT IF RADIUS INPUT IS IN CORRECT FORMAT BUT UNUSUAL VALUES FOR RADIUS ####
################# #

# note that SOME OF THESE TESTS ARE A BIT REDUNDANT AND MAYBE CAN GET CLEANED UP- IT IS COMPLICATED HOW RADIUS CAN BE INFERRED OR IS SUPPLIED BUT DOES NOT SEEM TO MATCH WHAT MUST HAVE BEEN USED IN getblocksnearby()

test_that('warning if ask for radius < 0', {
  junk = capture_output({
    expect_no_warning(
      suppressMessages({
        doaggregate(sites2blocks = testoutput_getblocksnearby_10pts_1miles , radius = 0)
      })
    )
    expect_warning({
      suppressMessages({
        x <- doaggregate(sites2blocks = testoutput_getblocksnearby_10pts_1miles , radius = -0.001)
      })
    })
    expect_true("results_overall" %in% names(x))
  })  
})

test_that('warning if ask for radius > 32, and just uses 32 instead', {
  # if (radius > 32) {radius <- 32; warning("Cannot use radius above 32 miles (almost 51 km) here - Returning results for 32 miles!")}
  junk = capture_output({
    suppressWarnings(
      expect_warning({
        x <- doaggregate(sites2blocks = testoutput_getblocksnearby_10pts_1miles , radius = 32.01, silentinteractive = TRUE)
      }))
  })
  expect_true("results_overall" %in% names(x))
})

testthat::test_that("same result if radius requested is 32 or 50, since >32 gets treated as if 32", {
  junk = capture_output({
    suppressMessages(suppressWarnings(x <- doaggregate(sites2blocks = testoutput_getblocksnearby_10pts_1miles, radius = 50, silentinteractive = TRUE) ))
    suppressMessages(suppressWarnings(y <- doaggregate(sites2blocks = testoutput_getblocksnearby_10pts_1miles, radius = 32, silentinteractive = TRUE) ))
  })
  expect_equal(x, y, ignore_attr = TRUE)
})

test_that("no warning if radius = 32 exactly IF original analysis was for AT LEAST 1/1.5x that radius", {
  x = getblocksnearby(testpoints_10[1,], radius = 1.01 * (32 / 1.5), quiet = TRUE) # 1.5x is where it starts to warn now in doag
  testthat::expect_no_warning({
    junk = capture_output({
      suppressMessages({
        y <- doaggregate(sites2blocks = x, radius = 32, silentinteractive = TRUE)
      })
    })
  })
  expect_true("results_overall" %in% names(y))
})

test_that("warning if radius = 32 exactly and original analysis was LESS THAN 1/1.5x that radius", {
  x = getblocksnearby(testpoints_10[1,], radius = 0.99 * (32 / 1.5), quiet = TRUE)
  testthat::expect_warning({
    junk = capture_output({
      suppressMessages({
        y <- doaggregate(sites2blocks = x, radius = 32, silentinteractive = TRUE)
      })
    })
  })
  expect_true("results_overall" %in% names(y))
})

test_that("radius param to doag that is very small relative to radius seen from getblocks get reported and used to filter distances", {
  junk = capture_output({
    suppressMessages({
      x <- doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = 0.25)$results_bysite$radius.miles[1]
    })
  })
  expect_equal(
    x,
    0.25
  )
})

test_that("radius param to doag that is 1.5x as big as radius seen from getblocks gets reported anyway as radius instead of inferring!?!? - do we want that???", {
  suppressWarnings(
    suppressMessages({
      junk = capture_output({
        x <- doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = 1.5)$results_bysite$radius.miles[1]
      })
    })
  )
  expect_false(isTRUE(all.equal(x, 1.5)))
})

test_that("radius param to doagg that is MUCH larger than seen from getblocks is ignored and doag uses inferred radius instead", {
  suppressWarnings({
    suppressMessages({
      y <- doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = 1.6)$results_bysite$radius.miles[1]
    })
    expect_equal(
      y, 1
    )
  })
})
################# #  ################# #  ################# #


################# #
# WHAT IF RADIUS INPUT IS BAD  ####
################# #

#     WHAT IF CHARACTER STRING PROVIDED AS RADIUS

#    may want to change this radius behavior ? ***

test_that('confusingly, warning (but not error) if radius = character string that can be coerced to a single number - does not actually coerce it but uses max seen!', {
  expect_warning({
    x <- doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = "1")
  })
  expect_true(is.list(x))
})

test_that("radius param to doagg that is string/text like '0.25' is not interpreted as the number 0.25 but use radius inferred from output of getblocks", {
  suppressWarnings(   
    expect_equal(
    doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = "0.25")$results_bysite$radius.miles[1],
    1) # inferred based on sites2blocks
  )})

## Run several test cases for inputs error checking, using list of test cases from setup.R
##
## to look at the test objects created earlier in setup.R,
# nix <- sapply(1:length(bad_numbers), function(z) {cat( "\n\n\n------------------------\n\n  ", names(bad_numbers)[z], "\n\n\n" ); print( bad_numbers[z][[1]] )}); rm(nix)

test_that(paste0("doaggregate radius with the input below should not warn or err!"), {
  cause_no_warn_no_err <- list(normalnumber = 1.3)
  cat('\n  Trying radius that is', names(cause_no_warn_no_err)[1], '- Testing to ensure it works... ')
  try({
    expect_no_condition({
      x <- doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = cause_no_warn_no_err[[1]])
    })
  })
  expect_true(
      NROW(x$results_bysite) == 10
    )
})

test_that(paste0("doaggregate radius like with the input below should warn!"), {
  
  cause_warn <- bad_numbers[c('TRUE1', 'text1', 'list1', "NA1", "NULL1")]
  
  cat('\n  Trying radius that is', names(cause_warn)[1], '- Testing to ensure it warns... ')
  try({
    expect_warning(
      x <- doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = cause_warn[[1]]), 
      info = paste0("doaggregate radius like ", names(cause_warn)[1], " should warn!")
    )
    expect_true(is.list(x))
  })
  
  cat('\n  Trying radius that is', names(cause_warn)[2], '- Testing to ensure it warns... ')
  try({
    expect_warning(
      x <- doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = cause_warn[[2]]), 
      info = paste0("doaggregate radius like ", names(cause_warn)[2], " should warn!")
    )
  })
  
  cat('\n  Trying radius that is', names(cause_warn)[3], '- Testing to ensure it warns... ')
  try({
    expect_warning(
      x <- doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = cause_warn[[3]]), 
      info = paste0("doaggregate radius like ", names(cause_warn)[3], " should warn!")
    )
    expect_true(is.list(x))
  })
  
  cat('\n  Trying radius that is', names(cause_warn)[4], '- Testing to ensure it warns... ')
  try({
    expect_warning(
      x <- doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = cause_warn[[4]]),
      info = paste0("doaggregate radius like ", names(cause_warn)[4], " should warn!")
    )
  })
  
  cat('\n  Trying radius that is', names(cause_warn)[5], '- Testing to ensure it warns... ')
  try({
    expect_warning(
      x <- doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = cause_warn[[5]]), 
      info = paste0("doaggregate radius like ", names(cause_warn)[5], " should warn!")
    )
  })
})

test_that(paste0("doaggregate radius like with the input below should report error!"), {
  cause_err <- bad_numbers[c("vector2", "array2","matrix_1row_4col", "matrix_4row_1col", "matrix_2x2" )]
  
  cat('\n  Trying radius that is', names(cause_err)[1], '- Testing to ensure it reports error... ')
  try({
    expect_error(
      x <- doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = cause_err[[1]]), 
      info = paste0("doaggregate radius like ", names(cause_err)[1], " should report error!")
    )
  })
  
  cat('\n  Trying radius that is', names(cause_err)[2], '- Testing to ensure it reports error... ')
  try({
    expect_error(
      x <- doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = cause_err[[2]]),
      info = paste0("doaggregate radius like ", names(cause_err)[2], " should report error!")
    )
  })
  
  cat('\n  Trying radius that is', names(cause_err)[3], '- Testing to ensure it reports error... ')
  try({
    expect_error(
      x <- doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = cause_err[[3]]), 
      info = paste0("doaggregate radius like ", names(cause_err)[3], " should report error!")
    )
  })
  
  cat('\n  Trying radius that is', names(cause_err)[4], '- Testing to ensure it reports error... ')
  try({
    expect_error(
      x <- doaggregate(sites2blocks =  testoutput_getblocksnearby_10pts_1miles, radius = cause_err[[4]]),
      info = paste0("doaggregate radius like ", names(cause_err)[4], " should report error!")
    )
  })
  
  cat('\n  Trying radius that is', names(cause_err)[5], '- Testing to ensure it reports error... ')
  try({
    expect_error(
      x <- doaggregate(sites2blocks =  testoutput_getlocksnearby_10pts_1miles, radius = cause_err[[5]]),
      info = paste0("doaggregate radius like ", names(cause_err)[5], " should report error!")
    )
  })
  
  expect_true(TRUE) # just to avoid report of empty test
})


## print(setdiff(names(bad_numbers), names(c(cause_no_warn_no_err, cause_warn, cause_err))))
## c("matrix_1x1", "array1", "character1", "df1")
#
# cause_something_else <- bad_numbers[c("matrix_1x1", "array1", "character1", "df1")]  # ????

rm(cause_no_warn_no_err)


################# #  ################# #  ################# #

################# #
# WHAT IF OTHER INPUTS ARE BAD ?? ####
################# #

cat('still need to test cases where inputs other than table or radius are invalid\n')
