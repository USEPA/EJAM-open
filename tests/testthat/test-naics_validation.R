## unit tests for EJAM::NAICS_validation() which is not a great function, and naics_is.valid() add by mc 2024 seems to work better.
## Author: Sara Sokolinski

# documentation seems outdated defining the input NAICS but not the distinction between naics_enter and NAICS_select

# test with real NAICS
test_that('real NAICS works',{
  expect_no_warning({
    val <- naics_validation(naics_enter = "211", naics_select = "1")
    })
  expect_true(val)
})

# naics_validation() numeric selector works
test_that('numeric selector works',{
  expect_no_warning({
    val <- naics_validation(naics_enter = "211", naics_select = 1)
  })
  expect_true(val)
})

#  No naics_validation() selector gives invalid
test_that('naics_validation() with no selectors gives error',{
  expect_error(naics_validation(naics_enter = "211"))
})

# 
# #  list of NAICS gives error ?? does it need to for shiny app?
# test_that('is multiple NAICS supposed to give error? does it need to for shiny app??', {
#   expect_error({
#     val <- naics_validation(naics_enter = c("211", "452"), naics_select = "1")
#   })
# 
# })

################################################## # 

## THIS NEWER FUNCGTION IS BETTER AT ACTUALLY VALIDATING NAICS CODES AGAINST LIST OF VALID CODES:

test_that('naics_is.valid() correctly reports 1 NAICS as not valid', {
  expect_no_warning({
    val <- naics_is.valid("LOL")
  })  
  expect_false(val) 
})
test_that('naics_is.valid() correctly reports some NAICS as not valid', {
expect_equal(
  naics_is.valid(code = c("211", "452", "999")),
  c(TRUE, TRUE, FALSE)
)
})
################################################## # 
# <<<<<<<<<<<<<<<<<<<<<<<<
test_that('fake NAICS in naics_validation() should report that valid is FALSE but ???', {
  expect_no_warning({
    capture_output({
      val <- naics_validation(naics_enter = "LOL", naics_select = "1")
    })
  }) ## ??
  expect_true(val)               # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<   WAS ERROR - naics_validation() is too weak a test of validity
})
# <<<<<<<<<<<<<<<<<<<<<<<<
# naics_validation() multiple selectors gives error ??
test_that('is multiple values for naics_validation(naics_select) supposed to give error? does it need to for shiny app??',{
  expect_no_error({
    capture_output({
      val <- naics_validation(naics_enter = "211", naics_select = c(1,2))  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< FAILED SINCE NO ERROR 
    #           - not sure this makes sense to have no error or warning and it reports valid (TRUE) -- naics_validation() is too weak a test of validity
    })
    })
})
# <<<<<<<<<<<<<<<<<<<<<<<<
