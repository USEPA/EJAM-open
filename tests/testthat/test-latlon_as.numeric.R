## unit tests for EJAM::latlon_as.numeric
## Author: Sara Sokolinski

# latlon_as.numeric() is not exported

# Remove all characters other than minus signs, decimal points, and numeric digits
# Useful if latitude or longitude vector has spaces, tabs, etc.
#   CAUTION - Assumes stripping those out and making it numeric will fix whatever problem there was
#   and end result is a valid set of numbers. Inf etc. are turned into NA values.
#   Empty zero length string is turned into NA without warning. NA is left as NA.
#   If anything other than empty or NA could not be interpreted as a number, it
#   returns NA for those and offers a warning.
## updated: 
# now if NOT running shiny, it stops with error if bad input. 
# if it is running shiny, it only warns and returns NA if bad input.

test_that('NA returns NA without warning', {
  expect_no_warning({
    val <- latlon_as.numeric(NA)
    })
  expect_true(is.na(val))
  })

test_that('empty string returns NA without warning (maybe unexpectedly)', {
  expect_no_warning({
    val <- latlon_as.numeric("")
    })
  expect_true(is.na(val))
})

# anything other than empty or NA that is not a number, should return NA
# with a warning (whether or not running shiny)
test_that('logical returns NA', {
  expect_warning({
    val <- latlon_as.numeric(TRUE)
    })
  expect_true(is.na(val))
})
############################################# #


test_that('empty vector STOPS WITH ERROR (unless in shiny) because length is 1', {
  expect_error({
    val <- latlon_as.numeric(c())
    })
  # expect_true(is.na(val))
})

test_that('list or data.frame, instead of just a vector, STOPS WITH ERROR (unless in shiny) ', {
  expect_error(
    latlon_as.numeric(list("a" = c(1:3), "b" = LETTERS[1:10]))
    )
  expect_error(
    latlon_as.numeric(data.frame(aaaa = 1:3, bbbb = c("x", 'y', 'z')))
  )
})
############################################# #


# test the removals work as expected
test_that('minus signs, decimal points and numeric digits are not removed while other symbols like dollar signs, commas, or percents are removed', {
  expect_no_warning({
    val <- latlon_as.numeric(c("-11.9345", "$30,165%"))
    })
  expect_true(all(val == c(-11.9345, 30165)))
})

test_that('spaces are removed', {
  expect_no_warning({
    val <- latlon_as.numeric(" - 11.9345 ")
    })
  expect_true(val == -11.9345)
})

test_that('when all characters are removed, it returns NA with warning', {
  expect_warning({
    val <- latlon_as.numeric("#_@!^&*()=+~[]{}<>?/|")
    })
  expect_true(is.na(val))
})
