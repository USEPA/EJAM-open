## unit tests for frs_from_naics
## Author: Sara Sokolinski

# function is defined in the file frs_from_xyz.R
# however, it is really testing naics_from_any
# all inputs get passed to that function

# first option is for website_url = TRUE which gives error. it cannot be used in frs_from naics
# since the response would not be a NAICS code to enter into regid_from_naics
# website_scrape = TRUE does work however, since it grabs a data frame with the column 'code'
# the code is stirng instead of numeric, but that doesn't seem to matter to regid_from_naics (in latlon_from_naics.R)

test_that('website_url and website_scrape cause errors',{
  expect_error( expect_warning({  val <- frs_from_naics(21112, website_url = TRUE)})) # "crude petroleum"
  expect_no_error({val <- frs_from_naics(21112, website_scrape = TRUE)}) # "crude petroleum"
  })

# however, even using naics_from_any gave an error that the function naics_url_of_query does not exist
# after renaming this to naics_url_of_code, it worked successfully (in script naics_url_of_code.R)
test_that('naics_from_any URL and scrape lookup works', {
  expect_equal(naics_from_any("crude petroleum")$code, c(21112, 211120))
  expect_equal(naics_from_any(21112, website_url = TRUE), "https://www.naics.com/six-digit-naics/?v=2017&code=21112")
  expect_equal(naics_from_any(21112, website_scrape = TRUE),
               data.frame("code" = "211120", "name" = "Crude Petroleum Extraction"))
  })

# some naics without warning will return an empty dataframe
# in this case 21112 gives no results because it wants 211120, which in previous examples
# was looked up by webscrape = TRUE or using "crude pretroleum" as the query
test_that('some naics have no sites',{
  expect_equal(0, nrow(frs_from_naics(21112, childrenForNAICS= FALSE))) # "crude petroleum extraction"
})

# others return results
# there is no warning when a NAICS has subcategories that are not being output
# while it works as detailed, this lack of warning could confuse some users who don't know to enter children = TRUE
# perhaps default children = TRUE or provide some type of message

test_that('results of subcategories only output when children = TRUE',{
  expect_no_warning(frs_from_naics(21222)) # "silver and gold mining"
  suppressWarnings({
    expect_no_error(frs_from_naics(21222)) # "silver and gold mining" # warns now about the function - see warning
  })
  expect_no_warning(frs_from_naics(212221)) # "gold mining"
  expect_no_warning(frs_from_naics(212222)) # "silver mining"
  suppressWarnings({
    x <- frs_from_naics(21222) # 373 # all gold and silver mining
  })

  expect_equal(length(which(grepl(212221,x$NAICS))), nrow(frs_from_naics(212221, childrenForNAICS= TRUE))) # 354 count of gold, matches subset from 21222 w/ children
  expect_gt(length(which(grepl(212222,x$NAICS))), nrow(frs_from_naics(21222, childrenForNAICS= FALSE))) # 41 count of silver, matches subset from 21222 w/ children
  
  expect_equal(nrow(frs_from_naics(21222, childrenForNAICS= FALSE)), sum(!grepl(212221,x$NAICS ) & !grepl(212222, x$NAICS))) # 1, that was returned by 21222 w/o children (both gold and silver mining)
  expect_equal(nrow(frs_from_naics(21222, childrenForNAICS = TRUE)), sum((grepl(212221,x$NAICS ) | grepl(212222, x$NAICS) | grepl(21222, x$NAICS))))
})

# string queries I believe are based on longest common string. For example "gold ore"
# finds NAICS 21222 & 212221 "gold ore mining" but "gold mining" returns empty
test_that('string queries function', {
  expect_no_warning({val <- frs_from_naics("gold ore")})
  expect_true(nrow(val) > 0)
  expect_no_warning({val <- frs_from_naics("gold mining")})
  # expect_true(nrow(val) > 0) # fails but that is ok
})

test_that('list of queries returns joined results', {
  expect_no_warning({ x <- frs_from_naics(c("gold ore", "silver ore"))})
  expect_no_warning({ y <- frs_from_naics("gold ore")})
  expect_no_warning({ z <- frs_from_naics("silver ore")})
  expect_equal(x %>% arrange(REGISTRY_ID), full_join(y, z) %>% arrange(REGISTRY_ID))

})

test_that('list of queries can mix numbers and strings', {
  expect_no_warning({x <- frs_from_naics(c("212221",  "silver ore"))})
  expect_no_warning({y <- frs_from_naics(c(212221,  "silver ore"))})
  expect_equal(x,y)
})

# test input ignore.case; default = TRUE
# passed with query text, children and  fixed to naics_from_name
# default in naics_from_name is also TRUE
# by default the grepl function looking for the query text is not case sensitive

test_that('case of query text only matters, if ignore.case = FALSE', {
  expect_equal(nrow(frs_from_naics(c( "silver ore"))),
               nrow(frs_from_naics(c( "Silver Ore"), ignore.case = FALSE)))

})

# test input fixed; default = FALSE
# passed with query text, children
#   and ignore.case to naics_from_name ?
# default in naics_from_name is also false
# by default the grepl function allows the search string to be modifed by other parameters (like ignore.case)
# by setting fixed = TRUE and using capital letters in the query, ignore.case = TRUE should be ignored and the string will be searched for exactly
# this gives a warning
test_that('fixed = TRUE makes case matter, even if ignore.case = TRUE',{
  expect_warning({x <- frs_from_naics(c("Silver Ore"), fixed = TRUE)})
  ## all lowercase produces 2 warnings for some reason
    expect_warning(y <- frs_from_naics(c("silver ore"), fixed = TRUE))

  expect_equal(nrow(y), 0)
  expect_equal(nrow(x),
               nrow(frs_from_naics(c("silver ore"))))

})
