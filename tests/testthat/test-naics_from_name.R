## unit tests for EJAM::naics_from_name
## Author: Sara Sokolinski

# library dplyr
# library EJAM

# warn if expect name but passed numeric
# no errors but returns empty dataframe
test_that('warn for query string', {
  expect_warning({val <- naics_from_name(11)})
  expect_no_warning({val <- naics_from_name("11")}) # it does not and probably cannot be expected to warn over this
  expect_equal(NROW(val), 0)
})



# test some different syntax

test_that('should give no warning for standard NAICS industry name lookup ??? ', {

  expect_no_warning(naics_from_name("gold ore"))

  expect_no_warning(naics_from_name(c("gold ore")))

  })


test_that('list of queries returns joined results', {

  expect_no_warning({
    x <- naics_from_name(c("gold ore",  "silver ore"))
  })

  expect_no_warning({
    y <- naics_from_name("gold ore")
    })

  expect_no_warning({
    z <- naics_from_name("silver ore")
  })

  expect_equal(
    x %>% arrange(name),
    full_join(y,z) %>% arrange(name)
    )

})





#  when a NAICS has subcategories that are not being output
# a lack of warning would confuse some users who don't know to enter children = TRUE
# perhaps default children = TRUE or provide some type of message
# the output are joined properly

test_that('results of subcategories only output when children = TRUE', {

  suppressWarnings(expect_no_error({
    x <- naics_from_name("gold ore and silver ore")
    })) # "silver and gold mining"
  expect_true(NROW(x) == 1)
  suppressWarnings(expect_no_error({
    x <- naics_from_name("gold ore and silver ore", children = TRUE)
    }))  # "silver and gold mining"
  expect_true(NROW(x) > 1)
  suppressWarnings(expect_no_error({
      y <- naics_from_name("gold ore")
      })) # "gold mining"
  expect_no_error(suppressWarnings({
      z <- naics_from_name("silver ore")
      })) # "silver mining"
  expect_true(NROW(z) > 1)

  #expect_equal(length(which(grepl(212221,x$n6))), nrow(naics_from_name("gold ore")[])) # 1 gold mining naics
  #expect_equal(length(which(grepl(212222,x$n6))), nrow(naics_from_name(212222))) # 1 silver mining naics
  expect_equal(
    nrow(naics_from_name("gold ore and silver ore")),
    sum(!grepl(212221,x$n6 ) & !grepl(212222, x$n6))) # 1 gold and silver mining
  expect_equal(
    nrow(naics_from_name("gold ore and silver ore", children = TRUE)),
    nrow(full_join(x,y) %>% full_join(z)) )# 1 gold and silver mining

})


