## unit tests for EJAM unexported function naics_webscrape
## Author: Sara Sokolinski


test_that('the function works for naics codes (numeric or string) and query text',{
  expect_no_warning({
    val <- naics_findwebscrape("gold ore")
    })
  expect_no_warning({
    val <- naics_findwebscrape("212221")
    })
  expect_no_warning({
    val <- naics_findwebscrape(212221)
    })
})


# latlon_df_clean had issues web scraping gold ore mining from gold mining
# this does not, but it returns many potential matches.
# These appear to be any mention of mining

# perhaps add an input like 'contained' to filter to just names containing every word in the string
# for this example the order matters, as in,
# it looks for gold mining and not mining gold

test_that('you can filter the results of webscrape',{
  query = "gold mining"
  expect_no_warning({val <- naics_findwebscrape(query)})

  query_split = strsplit(query, " ")
  query_mod = paste0(unlist(query_split), collapse = ".*")

  longperl <- grepl(query_mod, val$name, ignore.case = TRUE)
  val <- val[longperl,]
  expect_equal(nrow(val), 1) # there should only be one naics with both gold and mining
})
