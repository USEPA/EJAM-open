## unit tests for EJAM::naics_from_any
## Author: Sara Sokolinski

# library dplyr
# library EJAM
# library

# gives an error that the function naics_url_of_query does not exist
# after renaming this to naics_url_of_code in the naics_from_any function (& sourcing it)
# it worked successfully (naics_url_of_code is in script naics_url_of_code.R)

test_that('naics_from_any() -- URL and scrape lookup works', {

  expect_equal(naics_from_any("crude petroleum")$code, c(21112, 211120))
  expect_equal(naics_from_any(21112, website_url = TRUE), "https://www.naics.com/six-digit-naics/?v=2017&code=21112")
  expect_equal(naics_from_any(21112, website_scrape = TRUE),
               data.frame("code" = "211120", "name" = "Crude Petroleum Extraction"))

  })


# there is no warning when a NAICS has subcategories that are not being output
# while it works as detailed, this lack of warning could confuse some users who don't know to enter children = TRUE
# perhaps default children = TRUE or provide some type of message

test_that('naics_from_any() -- results of subcategories only output when children = TRUE', {
  expect_no_warning(naics_from_any(21222)) # "silver and gold mining"
  # expect_no_warning(naics_from_any(21222, children = TRUE)) # "silver and gold mining"
  expect_no_warning(naics_from_any(212221)) # "gold mining"
  expect_no_warning(naics_from_any(212222)) # "silver mining"
suppressWarnings({
  x <- naics_from_any(21222, children = TRUE) # 3 (gold, silver, & silver+gold)
  
})

  expect_equal(length(which(grepl(212221,x$n6))), nrow(naics_from_any(212221))) # 1 gold mining naics
  expect_equal(length(which(grepl(212222,x$n6))), nrow(naics_from_any(212222))) # 1 silver mining naics
  expect_equal(nrow(naics_from_any(21222)), sum(!grepl(212221,x$n6 ) & !grepl(212222, x$n6))) # 1 gold and silver mining
})


# string queries I believe are based on longest common string. For example "gold ore"
# finds NAICS 21222 & 212221 "gold ore mining" but "gold mining" returns empty
test_that('naics_from_any() -- string queries function', {
  expect_no_warning({val <- naics_from_any("gold ore")})
  expect_true(nrow(val) > 0)
  expect_no_warning({val <- naics_from_any("gold mining")})
  expect_true(nrow(val) == 0)
})


test_that('naics_from_any() -- list of queries returns joined results', {
  expect_no_warning({x <- naics_from_any(c("gold ore",  "silver ore"))})
  expect_no_warning({ y <- naics_from_any("gold ore")})
  expect_no_warning({ z <- naics_from_any("silver ore")})
  expect_equal(x %>% arrange(code), full_join(y,z) %>% arrange(code))
})


test_that('naics_from_any() -- list of queries can mix numbers and strings', {
  expect_no_warning({x <- naics_from_any(c("212221",  "silver ore"))})
  expect_no_warning({y <- naics_from_any(c(212221,  "silver ore"))})
  expect_equal(x,y)
})

# test input ignore.case; default = TRUE
# passed with query text, children and  fixed to naics_from_name
# default in naics_from_name is also TRUE
# by default the grepl function looking for the query text is not case sensitive

test_that('naics_from_any() -- case of query text only matters, if ignore.case = FALSE', {
  expect_equal(nrow(naics_from_any(c( "silver ore"))),
               nrow(naics_from_any(c( "Silver Ore"), ignore.case = FALSE)))
  expect_false(nrow(naics_from_any(c("silver ore"), ignore.case = FALSE)) ==
                 nrow(naics_from_any(c("Silver Ore"), ignore.case = FALSE)))
})

# test input fixed; default = FALSE
# passed with query text, children and ignore.case to naics_from_name
# default in naics_from_name is also false
# by default the grepl function allows the search string to be modifed by other parameters (like ignore.case)
# by setting fixed = TRUE and using capital letters in the query, ignore.case = TRUE should be ignored and the string will be searched for exactly
# this gives a warning
test_that('naics_from_any() -- fixed = TRUE makes case matter, even if ignore.case = TRUE',{
  expect_warning({x <- naics_from_any(c("Silver Ore"), fixed = TRUE)})
  expect_warning({y <- naics_from_any(c("silver ore"), fixed = TRUE)})

  expect_equal(nrow(y), 0)
  expect_equal(nrow(x),
               nrow(naics_from_any(c("silver ore"))))

})

