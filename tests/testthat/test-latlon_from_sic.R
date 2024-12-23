## unit tests for EJAM::latlon_from_sic
## Author: Sara Sokolinski

#
# library data.table
#

# does it work?

test_that('lookup works correctly',{
  expect_no_warning({val <- latlon_from_sic('0780')})
  expect_true(!is.na(val$lat[1]))
  expect_no_warning({val <- latlon_from_sic("0780")})
  expect_true(!is.na(val$lat[1]))
  # Leading zeros can't be handled like with fips codes, fine
  expect_warning({val <- latlon_from_sic(0780)})
  expect_false(!is.na(val$lat[1]))
  expect_no_warning({val <- latlon_from_sic(3229)})
  expect_true(!is.na(val$lat[1]))
})



test_that('warning and error for invalid SIC',{
  suppressWarnings(expect_warning({
    val <- latlon_from_sic("glass")
    }))
  suppressWarnings(expect_warning({
    val <- latlon_from_sic("blue")
    }))
})



test_that('warning for no sites',{
  expect_warning({val <- latlon_from_sic("7")})
  expect_true(is.na(val$lat[1]))
  expect_warning({val <- latlon_from_sic("70")})
  expect_true(is.na(val$lat[1]))
})



test_that('id_only works',{
  expect_no_warning({val <- latlon_from_sic("0780", id_only = TRUE)})
  expect_true(!is.na(val[1]))
})
