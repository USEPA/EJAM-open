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
  expect_false(   (  is.numeric.text(    NA))) # *** perhaps want it to return TRUE ? xxxxxxxxxxxxxxxxx
  expect_true(is.numeric.text(c('1',  NA)))
  expect_true(is.numeric.text(c('01', NA)))
  expect_false(is.numeric.text(c(1,    NA)))       # not is.character
  
  ## invalid text
  expect_false(is.numeric.text(        "text"))
  expect_false(is.numeric.text(c("a",  "text")))
  expect_false(is.numeric.text(c(1,    "text")))
  expect_false(is.numeric.text(c("01", "text")))
  
  ## NA and invalid text
  expect_false(is.numeric.text(c(NA, "text")))
  
})
