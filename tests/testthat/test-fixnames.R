
################################ # fixnames ####

cat('\n testing fixnames() \n')


test_that('ejscreenapi() output colnames get renamed (at least 1 does)', {
  expect_gt(length(setdiff(fixnames(names(out_api)), names(out_api))) , 0)   # at least 1 got renamed
})
test_that('ejscreenapi() outputs work in fixnames(), returning char vector of right length, totalPop renamed to pop', {
  oldvars <- colnames(out_api) # out_api was from ejscreenapi()
  vars <- fixnames(oldvars, oldtype = 'api', newtype = 'r')
  expect_gt(length(setdiff(vars, names(out_api))) , 0) # at least 1 got renamed
  expect_vector(vars)
  expect_type(vars, "character")
  expect_identical(length(vars),  length(oldvars))
  # totalPop is in outputs and gets renamed as pop
  expect_identical(vars[grepl("totalPop", oldvars)] , 'pop')
})
test_that('fixnames() for ejscreenapi() outputs works same if provided colnames or the whole data.frame, matrix, data.table, list', {
  a = fixnames(names(out_api))
  t1 = fixnames(out_api)
  t2 = fixnames(data.table::as.data.table(out_api))
  # dmatrix = matrix(1, nrow = 2, ncol = NCOL(out_api))
  # colnames(dmatrix) <- colnames(out_api)
  dmatrix <- as.matrix(out_api)
  t3 = fixnames(dmatrix)
  t4 = fixnames(list(totalPop = 1001:1002, other = 1:2))
  expect_identical(a,t1)
  expect_identical(a,t2)
  expect_identical(a,t3)
  expect_identical(c('pop','other'), t4)
})

##################### more for fixnames 

test_that(desc = 'fixnames() output is char vector of right length, for a simple test set of 2 names', {
  oldvars <- c('totalPop', 'y')
  vars <- fixnames(oldvars, oldtype = 'api', newtype = 'r')
  expect_vector(vars)
  expect_type(vars, "character")
  expect_identical(length(vars), length(oldvars))
})
test_that(desc = 'fixnames renames totalPop to pop for correct element', {
  oldvars <- c('totalPop', 'y')
  vars <- fixnames(oldvars, oldtype = 'api', newtype = 'r')
  expect_equal(grepl("totalPop", oldvars), grepl("pop", vars))
})
test_that('fixnames() returns 1 for 1, NA for NA even if all are NA', {
  # renaming works: 1 or more API indicator names including totalPop get renamed as character vector, same length, NA as NA
  expect_identical(fixnames('just_one_item') , 'just_one_item')
  expect_identical(fixnames(c("validword", NA_character_)) , c("validword", NA))
  expect_identical(is.na(fixnames(NA)), TRUE)  
})
