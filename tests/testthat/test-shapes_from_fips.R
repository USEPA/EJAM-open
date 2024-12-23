

# shapes_from_fips
junk <- capture.output({
  mystates = c('DE', "RI")
  fipslist = list(
    statefips = name2fips(mystates),
    countyfips = fips_counties_from_state_abbrev(c('DE')),
    cityfips = name2fips(c('Rehoboth Beach,DE', 'Camden,de')),
    tractfips = substr(blockgroupstats$bgfips[300:301], 1, 12),
    bgfips = blockgroupstats$bgfips[300:301]
  )
  shp <- list()
  
})

# for (i in seq_along(fipslist)) {
#   shp[[i]] <- shapes_from_fips(fipslist[[i]])
#   print(shp[[i]])
#   # mapfast(shp[[i]])
# }

testthat::test_that("shapes_from_fips for statefips", {
  
  expect_no_error({
    shp <- shapes_from_fips(fipslist$statefips)
  })
  expect_true({'sf' %in% class(shp)})
  expect_identical(mystates, shp$STUSPS)
})

testthat::test_that("shapes_from_fips for countyfips", {
  junk = capture.output({
  expect_no_error({
    shp <- shapes_from_fips(fipslist$countyfips)
  })
  })
  expect_true({'sf' %in% class(shp)})
  expect_identical(fipslist$countyfips, shp$FIPS)
})

testthat::test_that("shapes_from_fips for cityfips", {
  
  expect_no_error({
    shp <- shapes_from_fips(fipslist$cityfips)
  })
  expect_true({'sf' %in% class(shp)})
  expect_identical(shp$GEOID, fipslist$cityfips)
})

testthat::test_that("shapes_from_fips for tractfips", {
  junk = capture.output({
    
  expect_no_error({
    shp <- shapes_from_fips(fipslist$tractfips)
  })
  })
  expect_true({'sf' %in% class(shp)})
  expect_identical(shp$FIPS, fipslist$tractfips)
  expect_identical(NROW(shp), length(fipslist$tractfips)) 
})

testthat::test_that("shapes_from_fips for bgfips", {
  junk = capture.output({
    
  expect_no_error({
    shp <- shapes_from_fips(fipslist$bgfips)
  })
})
  expect_true({'sf' %in% class(shp)})
})
################ #
# 
# testthat::test_that("shapes_from_fips error cases", {
#   
#   expect_null({
#     shp <- shapes_from_fips("string not fips")
#   })
#   expect_null({
#     shp <- shapes_from_fips(-1)
#   })
#   expect_null({
#     shp <- shapes_from_fips(c(1,0)) # some ok some not - state
#   })
#   expect_null({
#     shp <- shapes_from_fips(c('10001', '99999')) # some ok some not - county
#   })
#   expect_null({
#     shp <- shapes_from_fips(c('2513205','1239999')) # some ok some not - city
#   })
#   
# })
