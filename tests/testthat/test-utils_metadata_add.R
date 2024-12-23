
test_that("metadata_check default works", {
  expect_no_error({
    x <- metadata_check()
  })
  expect_true(
     any(x$has_metadata == TRUE)
  )
})

test_that("metadata_check format OK and finds some results", {
  expect_no_error({
    x <- metadata_check("EJAM")
  })
  
  expect_true(
    "usastats" %in% x$item
  )
  expect_true(
    "EJAM" %in% x$package
  )
  expect_true(
    "census_version" %in% colnames(x)
  )
  expect_true(
    any(2020 == x$census_version )
  )
})

test_that("metadata_check(packages = 'EJAM', loadifnotloaded = FALSE) works", {
  expect_no_error(
    metadata_check(loadifnotloaded = FALSE)
    )
})

test_that("metadata_check(loadifnotloaded = T) works", {
  expect_no_error(metadata_check(packages = "EJAM", loadifnotloaded = TRUE))
  expect_no_error(metadata_check(loadifnotloaded = TRUE))
})

# test_that("handles non-attached package? objects it cannot get() ? ", {
#   expect_no_error(
#     metadata_check(packages = "leaflet", loadifnotloaded = TRUE)
#     )
#   
# })

test_that("should handles non-existant package", {
  expect_no_error(
    
    metadata_check(packages = "NOPACKAGEEXISTSOFTHISNAME", loadifnotloaded = TRUE)
    
  )
})

test_that("metadata_check(various params) and unfound info works", {
  expect_no_error(
    metadata_check(packages = "EJAM", 
                   loadifnotloaded = FALSE,
                   which = c("notfound", "acs_version")
    )
  )
})
##################################################### # 

test_that("metadata_add default works", {
  
  expect_no_error({
    x <- data.frame(a = 1:10, b = 1:10)
    suppressMessages({
      x <- metadata_add(x)
    })
  })
  
  expect_true(
    all(names(as.list(formals(metadata_add)$metadata))[-1] %in% names(attributes(x)))
    # the -1 gets rid of an empty element that is the first in the list somehow
  )
})

# metadata fields to associate with a dataset should be added to the R/metadata_mapping.R script
test_that("metadata_add assignments work", {
  
  expect_no_error({
    test_metadata_custom <- data.frame(a = 1:10, b = 1:10)
    test_metadata_custom2 <- data.frame(a = 1:10, b = 1:10)
    suppressMessages({
      x <- metadata_add(test_metadata_custom)
      x <- metadata_add(test_metadata_custom2)
    })
  })
  expect_identical(
    attributes(x)$custominfo, 123
  )
  expect_identical( 
    attributes(x)$moreinfo, "abc"
  )
  expect_identical(
    attributes(x)$unchangedinfo, 9
  )
})
##################################################### # 
