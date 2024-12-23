
################################ # fixnames_to_type ####

cat('\n testing fixnames_to_type() \n')

test_that("fixnames_to_type() works at all", {
  expect_no_error({
    fixnames_to_type(namesnow = map_headernames$rname, oldtype = 'rname', newtype = 'longname')
  })
  expect_no_error({
    fixnames_to_type(namesnow = map_headernames$rname, oldtype = 'rname', newtype = 'shortlabel')
  })
  expect_no_error({
    fixnames_to_type(namesnow = map_headernames$apiname, oldtype = 'apiname', newtype = 'longname')
  })
  expect_no_error({
    fixnames_to_type(namesnow = map_headernames$apiname, oldtype = 'apiname', newtype = 'rname')
  })
  expect_no_error({
    fixnames_to_type(namesnow = map_headernames$longname, oldtype = 'longname', newtype = 'rname')
  })
  expect_no_error({
    fixnames_to_type(namesnow = map_headernames$longname, oldtype = 'longname', newtype = 'shortlabel')
  })
})
test_that("no error but warns and fails to rename if alias not actual colname used, unlike fixcolnames() which allows alias", {
  expect_no_error({
    suppressWarnings({
      fixnames_to_type(namesnow = map_headernames$rname, oldtype = 'r', newtype = 'longname')
    })
  })
  expect_warning({
    fixnames_to_type(namesnow = map_headernames$rname, oldtype = 'r', newtype = 'longname')
  })
  # this function does not work except with exact colname, but warns instead of error
  expect_no_error({
    suppressWarnings({
      fixnames_to_type(namesnow = map_headernames$rname, oldtype = 'rname', newtype = 'long')
    })
  })
  expect_warning({
    fixnames_to_type(namesnow = map_headernames$rname, oldtype = 'rname', newtype = 'long')
  })
})

