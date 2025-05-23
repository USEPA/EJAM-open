## unit tests for EJAM::state_from_latlon
## Author: Sara Sokolinski


# does it work?
test_that('lookup works correctly',{
  suppressWarnings({
    
  expect_no_warning({val <- state_from_latlon("18", "-66")})
  expect_equal(val$statename, "Puerto Rico")
  expect_no_warning(val <- state_from_latlon(18, -66))
  expect_equal(val$statename, "Puerto Rico")
  })
})

# add warning that lat long are not in the states (empty dataframe)
# could turn into an error
test_that('warns for invalid latlon',{
  expect_warning(
    expect_warning(
      expect_warning({val <- state_from_latlon("45", "-7")})
    )
  )
  expect_true(is.na(val$statename))
  expect_warning(
    expect_warning(
      expect_warning({val <- state_from_latlon(45, -7)})
    )
  )
  expect_true(is.na(val$statename))

})

test_that('warn for invalid strings',{
  # hitting error from st_as_sf.data.frame()
  # added error message for invalid strings to give simple error messages
suppressWarnings({
  expect_warning(
   state_from_latlon("blue", "fox")
   )
})
})

test_that('error for invalid syntax',{
  # just no default for "lon" error, ok
  expect_error({val <- state_from_latlon(c("18", "-66"))})
})

test_that('latlon as string vectors are ok', {
  # can't handle it, generic condition length error
  expect_no_condition({
    x = state_from_latlon(lat = c("33", "40"), lon = c("-90", "-110"))
    })
  expect_equal( x$ST , c("MS", "UT") )
})
