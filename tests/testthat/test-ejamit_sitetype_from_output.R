

# test-ejamit_sitetype_from_output()

testthat::test_that("ejamit_sitetype_from_output(out_shp) ok",  {
  
  ###  MAKE TEST DATA
  suppressWarnings({
    junk = capture_output({
      # dir( system.file('testdata/shapes', package = "EJAM") )
      fil = 'portland_folder_shp'   # works
      # fil = 'portland.gdb.zip'    # did not work like this
      shapefname = paste0(system.file('testdata/shapes', package = "EJAM"), "/", fil)
      
      out_shp     = ejamit(shapefile = shapefname, radius = 0)
    })
  })
  
  expect_no_error({
    ejamit_sitetype_from_output(out_shp)
  })
})


testthat::test_that("ejamit_sitetype_from_output(out_fips) ok",  {
  
  ###  MAKE TEST DATA
  suppressWarnings({
    junk = capture_output({
      out_fips    = ejamit(fips = fips_counties_from_state_abbrev('DE'))
    })
  })
  
  expect_no_error({
    ejamit_sitetype_from_output(out_fips)
  })
})



testthat::test_that("ejamit_sitetype_from_output(out_latlon) ok",  {
  
  out_latlon  = testoutput_ejamit_10pts_1miles
  
  expect_no_error({
    ejamit_sitetype_from_output(out_latlon)
  })
})





# ejamit_sitetype_from_output(testoutput_ejscreenapi_plus_5) # not designed for that but ok

