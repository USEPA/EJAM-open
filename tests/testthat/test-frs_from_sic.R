## unit tests for EJAM::frs_from_sic
## Author: Sara Sokolinski


# does it work?
test_that('lookup works correctly',{
  expect_no_warning(val <- frs_from_sic('0780'))
  expect_true(!is.na(val$lat[1]))
  expect_no_warning(val <- frs_from_sic("0780"))
  expect_true(!is.na(val$lat[1]))
  # Leading zeros can't be handled like with fips codes, fine
  suppressWarnings({
    expect_warning(val <- frs_from_sic(0780))
    
  })
  
  expect_false(!is.na(val$lat[1]))
  # 780 is not in frs data while 7800 is
  # so sub codes aren't being looked up unless the code itself returns results
  # fixed within underlying sic_from_any
  expect_no_warning(val <- frs_from_sic(0780, children = TRUE))
  expect_true(!is.na(val$lat[1]))
  expect_no_warning(val <- frs_from_sic(3229))
  expect_true(!is.na(val$lat[1]))

})

# add warnings to the sic_from_any function inside not here

test_that('warning for empty df / invalid SIC',{
  expect_no_error({
    suppressWarnings({
      val <- frs_from_sic("blue")
    })
  })
  expect_true(is.na(val$lat[1]))
  suppressWarnings({
    expect_warning({val <- frs_from_sic("7")})
  })
  expect_true(is.na(val$lat[1]))
  suppressWarnings({
  expect_warning({val <- frs_from_sic("70")})
})
  expect_true(is.na(val$lat[1]))

})
