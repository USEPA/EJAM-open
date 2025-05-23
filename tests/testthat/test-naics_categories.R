## unit tests for EJAM::naics_categories
## Author: Sara Sokolinski




# should digits = 1 give a warning? since it's empty
test_that('the function works for all digits',{
  table(nchar(NAICS))
  expect_no_warning({val <- naics_categories(digits = 1)})
  expect_no_warning({val <- naics_categories(digits = 2)})
  expect_no_warning({val <- naics_categories(digits = 3)})
  expect_no_warning({val <- naics_categories(digits = 4)})
  expect_no_warning({val <- naics_categories(digits = 5)})
  expect_no_warning({val <- naics_categories(digits = 6)})
})



# filter EJAM data going in
#
# test_that('entering filtered dataframe works', {
#
#   df <- EJAM::NAICS %>% as.data.frame
#   # names(df) <- "dot"
#
#   # these tests did not make sense and did not work:
#
#   # df <- df %>% filter(startsWith(as.character(df$dot), "11")) # ?? fails
#   # df <- df$.
#   # expect_no_warning({
#   #   val <- naics_categories(digits = 2, dataset = df)
#   #   })
#   # expect_equal(length(val), 1)
#
# }
# )
