## unit tests for frs_is_valid
## Author: Sara Sokolinski

# may not be exported



# first I downloaded an FRS EPA query CSV file
# tested using it in the app
# Invalid Filetype error when I use a CSV even though it lists CSV as a valid type
# Convert to xlsx
# Reads it in now

# works with standard REGISTRY_ID column
test_that('REGISTRY_ID column is recognized',{
  expect_true(suppressWarnings(
    frs_is_valid(data.frame("REGISTRY_ID" = 110000307695,
                            "PRIMARY_NAME" = "testname"))))
})

# works with regid column --------------?
test_that('regid column is recognized',{
  expect_true(suppressWarnings(
    frs_is_valid(data.frame("regid" = 110000307695,
                            "PRIMARY_NAME" = "testname"))))
})

# works with RegistryID column
test_that('RegistryID column is recognized',{
  expect_true(suppressWarnings(
    frs_is_valid(data.frame("RegistryID" = 110000307695,
                            "PRIMARY_NAME" = "testname"))))
})

# works with siteid column
test_that('siteid column is recognized',{
  expect_true(suppressWarnings(
    frs_is_valid(data.frame("siteid" = 110000307695,
                            "PRIMARY_NAME" = "testname"))))
})

# those are the four column names in frs_is_valid

# fails if colname is something like "id"
test_that("colname like 'id' fails to be recognized as regid, since too ambiguous", {
  testthat::expect_false(suppressWarnings(
    frs_is_valid(data.frame(id = 110000307695, "PRIMARY_NAME" = "testname")))
  )
})
########################### #

# gives FALSE if colname is valid other than REGISTRY_ID but no valid numbers in alias col used
test_that("colname not REGISTRY_ID but seems to be ok alias, so check done for invalid numbers, so returns FALSE if invalid number", {
  testthat::expect_false(suppressWarnings(
    frs_is_valid(data.frame(regid = NA, "PRIMARY_NAME" = "testname")))
  )
})

# Oddly, does not fail if colname is REGISTRY_ID but no valid numbers provided
test_that("colname REGISTRY_ID ok, so no check done for invalid numbers, so returns TRUE despite all bad numbers", {
  testthat::expect_true(suppressWarnings(
    frs_is_valid(data.frame(REGISTRY_ID = 1, "PRIMARY_NAME" = "testname")))
  )
})

test_that("providing a data.table not just data.frame is handled/ fixed", {
  testthat::expect_no_error(suppressWarnings(
    frs_is_valid(data.table(id = 110000307695, "PRIMARY_NAME" = "testname")))
  )
})



# could test...
# if above tests but for 2+ rows
# if 0 rows
# if not a data.frame or data.table
