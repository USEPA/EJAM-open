
testthat::test_that("getblocksnearby_from_fips works at all", {
  
  testthat::capture_output({
    
    testthat::expect_no_error({
      x <- getblocksnearby_from_fips("482011000011") # one blockgroup only
      # y=doaggregate(x)
    }
    ) 
    testthat::expect_no_error({
      x <- getblocksnearby_from_fips(fips_counties_from_state_abbrev("DE"), inshiny = F, need_blockwt = TRUE)
    })
    testthat::expect_setequal(
      names(x),
      c("ejam_uniq_id" ,"blockid"  ,    "distance"  ,   "blockwt"  ,    "bgid")
    )
    # counties_ej <- doaggregate(x)
    #cannot use mapfast(counties_ej$results_bysite) since no lat lon.  mapfastej_counties() should work...
    
  })
  
})

