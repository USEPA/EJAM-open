test_that("is.numeric.text works", {
  
  ## Just checks the vector as a whole, returning only 1 T/F.
  ## i.e., does not check each element to return a logical vector.
  expect_length(is.numeric.text(c('1', '2')), 1)
  
  ## For Valid numbers stored as numeric not text, func will return FALSE!
  expect_false(is.numeric.text(1:10))

  ## normal cases
  expect_true(is.numeric.text('1'))
  expect_true(is.numeric.text('01'))
  expect_true(is.numeric.text(c('01', '02')))
  

  ## NA value(s) always ignored, so if it is only NA values, the result is FALSE

  ## NA values
  expect_true(  is.na(  is.numeric.text(    NA,  na.rm = F))) #
  expect_true(is.numeric.text(c('1',  NA), na.rm = T))
  expect_true(is.numeric.text(c('01', NA), na.rm = T))
  expect_false(is.numeric.text(c(1,    NA), na.rm = T))       # not is.character
  
  ## invalid text
  expect_false(is.numeric.text(        "text",  na.rm = T))
  expect_false(is.numeric.text(c("a",  "text"), na.rm = T))
  expect_false(is.numeric.text(c(1,    "text"), na.rm = T))
  expect_false(is.numeric.text(c("01", "text"), na.rm = T))

  ## NA and invalid text
  expect_false(is.numeric.text(c(NA, "text"), na.rm = T))
  
})
