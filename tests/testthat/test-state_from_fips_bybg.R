## unit tests for EJAM::state_from_fips_bybg
## Author: Sara Sokolinski


# does it work?
test_that('lookup works correctly',{
  expect_no_warning({val <- state_from_fips_bybg("45")})
  expect_equal(val[1], "SC")
})

# it uses fips_bgs_in_fips to check if it's valid.
# any warnings should be added there not to this function
test_that('actually does not warn but does not crash, for invalid fips',{
  suppressWarnings({
    
  expect_no_error({
    val <- state_from_fips_bybg("452")
    })
  expect_true(
    is.na(val[1])
    )

  expect_no_error({
    val <- state_from_fips_bybg("blue")
  })
  expect_true(is.na(val[1]))
})
})



# I added this optional new parameter since it might be useful to somebody
#
#    but note the latest main branch version lacks such a parameter... not sure where that may have been created
#
# test_that('abbrev works',{
#   expect_no_warning({
#     val <- state_from_fips_bybg("45", abbrev = FALSE)
#                     })
#   expect_equal(val[1], "South Carolina")
# })
