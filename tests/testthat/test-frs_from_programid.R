## unit tests for frs_from_programid
## Author: Sara Sokolinski

# function is in the script frs_from_xyz.R

# not much to test here

# does it work with a propoer program id
test_that('lookup works correctly',{
  expect_no_warning({
    val <- frs_from_programid("State", "XJW000012435")
    })
  expect_true("lat" %in% names(val) & "lon" %in% names(val) &   "data.table" %in% class(val))
})


# does it give an error when id doesnt exist?
# no it doesn't, just returns an empty data frame, or actually 1 row all NA values
test_that('lookup works correctly',{
  expect_no_error({
    val <- frs_from_programid("State","fakeid")
    })
  expect_true(all(is.na(val)))
  # expect_equal(NROW(val), 0)
})
