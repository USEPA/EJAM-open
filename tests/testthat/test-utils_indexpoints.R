
# tests for
#
#      indexfrs()
# and
#      indexpoints()

test_that("indexpoints works at all", {
  expect_no_error({
    suppressWarnings({
      indexpoints(testpoints_10[1:2,])
    })
    rm(custom_index, envir = globalenv()) # default name of index
  })
  expect_no_error({
    suppressWarnings({
      indexpoints(testpoints_10[1:2,], indexname = "tempindex")
    })
    rm(tempindex, envir = globalenv())
  })
  expect_no_error({
    tempenv <- new.env()
    suppressWarnings({
      indexpoints(testpoints_10[1:2,], envir = tempenv)
    })
    rm(tempenv) # delete the environment, including anything created in it
  })
})

test_that("indexpoints assigns object to global envt, of specified name", {
  suppressWarnings({
    indexpoints(testpoints_10[1:2,], indexname = "tempindex")
  })
  expect_true(exists("tempindex", envir = globalenv()))
  rm(tempindex, envir = globalenv())
})

test_that("indexpoints can create object in a specified envt, of specified name", {
  tempenv <- new.env()
  suppressWarnings({
    indexpoints(testpoints_10[1:2,], indexname = "tempindex", envir = tempenv)
  })
  expect_true(exists("tempindex", envir = tempenv))
  rm(tempenv)
})

test_that("indexpoints() creates object of right class, but DOES NOT RETURN IT", {
  tempenv <- new.env()
  suppressWarnings({
    x <- indexpoints(testpoints_10[1:2,], envir = tempenv)
  })
  expect_true("custom_index" %in% ls(envir = tempenv))
  expect_true("QuadTree" %in% class(get("custom_index", envir = tempenv)))
  expect_s4_class(get("custom_index", envir = tempenv), "QuadTree")
  rm(tempenv)
})

test_that("indexpoints number of points looks right", {
  tempenv <- new.env()
  n <- 10
  suppressWarnings({
    x <- indexpoints(testpoints_10, envir = tempenv)
  })
  x <- get("custom_index", envir = tempenv)
  expect_equal(x@totalData, n)
  rm(tempenv, x)
})
