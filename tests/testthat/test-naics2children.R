## unit tests for EJAM::naics2children
## Author: Sara Sokolinski

# library dplyr
# library EJAM

# there are more 2 digit naics now then when the script was written
# table(nchar(as.character(NAICS)))
# There were 17 and are now 24

# test some different syntax
test_that('no warning for standard code lookup', {
  capture_output({
    expect_no_warning(naics2children(21112))
    expect_no_warning(naics2children("21112"))
  })
})

test_that("warn if text instead of code provided", {
  expect_warning(naics2children(c("cement")))
}) 

test_that('list of queries returns joined results', {
  capture_output({
    expect_no_warning({
      x <- naics2children(c("211",  "452"))
    })
    expect_no_warning({y <- naics2children("211")})
    expect_no_warning({z <- naics2children("452")})
    expect_equal(x, c(y,z))
  })
})

test_that('order doesnt matter', {
  capture_output({
    expect_no_warning({x <- naics2children(c("211",  "452"))})
    expect_no_warning({ y <- naics2children(c("452", "211"))})
    expect_equal(x, y)
  })
})
