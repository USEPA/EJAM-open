## unit tests for EJAM::regid_from_naics
## Author: Sara Sokolinski


# does it work?
test_that('lookup works correctly',{
  expect_no_warning({val_just_regid <- regid_from_naics("452")})
  expect_no_warning({val_full_table <- regid_from_naics("452", id_only= FALSE)})
  expect_equal(val_just_regid, val_full_table$REGISTRY_ID)
  expect_equal(val_just_regid, frs_from_naics('452')$REGISTRY_ID)
  expect_equal(nrow(val_full_table), nrow(frs_from_naics('452')))
  expect_no_warning({val_just_regid <- regid_from_naics(452)})
  expect_no_warning({val_full_table <- regid_from_naics(452, id_only= FALSE)})
  expect_equal(val_just_regid, val_full_table$REGISTRY_ID)
  expect_equal(val_just_regid, frs_from_naics(452)$REGISTRY_ID)
  expect_equal(nrow(val_full_table), nrow(frs_from_naics('452')))
})

test_that('no crash (but fails to warn)  for invalid NAICS',{
  # expect_warning({val <- regid_from_naics("4")})
  expect_no_error({val <- regid_from_naics("4")})
  expect_equal(length(val), 0)
  # expect_warning({val <- regid_from_naics("blue")})
  expect_no_error({val <- regid_from_naics("blue")})
  expect_equal(length(val), 0)
})

test_that('id_only works',{
  expect_no_warning({val <- regid_from_naics("452", id_only = TRUE)})
  expect_true(is.vector(val))
})

test_that('works in list',{
  expect_no_warning({val <- regid_from_naics(c("452", "21"))})
  expect_equal(nrow(val), nrow(frs_from_naics((c("452", "21")))$REGISTRY_ID))
  expect_no_warning({val_full_table <- regid_from_naics(c("452", "21"), id_only= FALSE)})
  expect_equal(val, val_full_table$REGISTRY_ID)
})

test_that('no crash (but fails to warn) if not present in dataset',{
  # expect_warning({val <- regid_from_naics(c( "9", "101"))})
  expect_no_error({val <- regid_from_naics(c( "9", "101"))})
  expect_equal(length(val), 0)
})
