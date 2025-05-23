## unit tests for EJAM::naics_subcodes_from_code
## Author: Sara Sokolinski
#
# library(dplyr)
# library(EJAM)
# library(testthat)
# function is in naics_from_any.R

# test some different syntax
test_that('no ERROR for standard code lookup', {
  # these do warn and that warning is about the function really, so changed test
  suppressWarnings({
    expect_no_error(naics_subcodes_from_code(21112))
    expect_no_error(naics_subcodes_from_code("21112"))
    expect_no_error(naics_subcodes_from_code(c(21112)))
    expect_no_error(naics_subcodes_from_code(c("21112")))
  })
})

# warn if passed text string or invalid number string
# no errors but returns empty dataframe
test_that('error for query string', {
  suppressWarnings({
    
    expect_warning({val <- naics_subcodes_from_code("gold ore")})
    expect_warning({val <- naics_subcodes_from_code("$100,0")})
  })
  
})


test_that('list of queries returns joined results', {
  # these do warn and that warning is about the function really, so changed test
  suppressWarnings({
    expect_no_error({x <- naics_subcodes_from_code(c("211",  "452"))})
    expect_no_error({ y <- naics_subcodes_from_code("211")})
    expect_no_error({ z <- naics_subcodes_from_code("452")})
    expect_equal(x %>% dplyr::arrange(code), full_join(y,z) %>% dplyr::arrange(code))
    
  #   expect_no_warning({x <- naics_subcodes_from_code(c("211",  "452"))})
  # expect_no_warning({ y <- naics_subcodes_from_code("211")})
  # expect_no_warning({ z <- naics_subcodes_from_code("452")})
  # expect_equal(x %>% dplyr::arrange(code), full_join(y,z) %>% dplyr::arrange(code))
  })
})


# error if passed text string or invalid number string
# no errors but returns empty dataframe
test_that('1 digit gives error', {
  suppressWarnings({
    expect_warning(  naics_subcodes_from_code(c("1",  "4"))  )
    
  })
  
})
