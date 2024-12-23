
################# #
# latlon_from_vectorofcsvpairs
# latlon2csv
# latloncsv2nexus
# latlon2nexus
################# #

## test cases

# x = list( x_good = c("30.01,-90.61", "30.26,-90.95", "30.51,-91.23"),
#           x_na1 = NA,
#           x_na2 = c("30,-83","32.5,-86.377325", NA),
#           x_na3 = c(NA,NA),
#           
#           x_null = NULL,
#           x_txt1 = "text,text",
#           x_txt2 = c("30,-83", "a,b"),
#           x_empty1 = c("30,-83", ""),
#           x_empty2 = c("30,-83", ","),
#           x_empty3 = c("30,-83", "NA,"),
#           x_commas = c("1,2,3", "30,-83"),
#           x_commas2 = c("1,2,3", "30,-83,999")
# )
########################################################## #

testthat::test_that("latlon_from_vectorofcsvpairs works for good data", {
  x_good = c("30.01,-90.61", "30.26,-90.95", "30.51,-91.23")
  testthat::expect_no_error({
    out = latlon_from_vectorofcsvpairs(x_good)
  })
  testthat::expect_true(is.data.frame(out))
  testthat::expect_true(NROW(out) == 3)
  testthat::expect_equal(
    out,
    structure(list(
      lat = c(30.01, 30.26, 30.51), 
      lon = c(-90.61, -90.95, -91.23)), 
      class = "data.frame", row.names = c(NA, -3L)))
  
  
  lat_x = testpoints_10$lat
  lon_x = testpoints_10$lon
  latlon_pairs = latlon2csv(lat = lat_x, lon = lon_x)
  latlon_from_vectorofcsvpairs(latlon_pairs)
  testthat::expect_equal(
    testpoints_10[, c("lat", "lon")], 
    latlon_from_vectorofcsvpairs(latlon_pairs)
    )
  
})
################# #

testthat::test_that("can handle NA as a lat,lon pair", {
  x_na1 = NA
  x_na2 = c("30,-83","32.5,-86.377325", NA)
  x_na3 = c(NA,NA)
  testthat::expect_no_error({
    latlon_from_vectorofcsvpairs(x_na1)
  })
  testthat::expect_no_error({
    latlon_from_vectorofcsvpairs(x_na2)
  })
  testthat::expect_no_error({
    latlon_from_vectorofcsvpairs(x_na3)
  })
})

########################################################## #

testthat::test_that("err on null", {
  x_null = NULL
  testthat::expect_error({
    latlon_from_vectorofcsvpairs(x_null)
  })
})
################# #
testthat::test_that("err on text instead of lat,lon", {
  x_txt1 = "text,text"
  x_txt2 = c("30,-83", "a,b")
  testthat::expect_error({
    latlon_from_vectorofcsvpairs(x_txt1)
  })
  testthat::expect_error({
    latlon_from_vectorofcsvpairs(x_txt2)
  })  
})
################# #
testthat::test_that("err if lat or lon missing/blank/NA", {
  x_empty1 = c("30,-83", "")
  x_empty2 = c("30,-83", ",")
  x_empty3 = c("30,-83", "NA,")
  testthat::expect_error({
    latlon_from_vectorofcsvpairs(x_empty1)
  })
  testthat::expect_error({
    latlon_from_vectorofcsvpairs(x_empty2)
  })
  testthat::expect_error({
    latlon_from_vectorofcsvpairs(x_empty3)
  })  
})
################# #
testthat::test_that("err if all or some have extra commas", {
  x_commas = c("1,2,3", "30,-83")
  x_commas2 = c("1,2,3", "30,-83,999")
  suppressWarnings({
    testthat::expect_error({
      latlon_from_vectorofcsvpairs(x_commas)
    })
    testthat::expect_error({
      latlon_from_vectorofcsvpairs(x_commas2)
    })
  })
})
########################################################## #

testthat::test_that("latlon2csv ok", {
  lat_example = c(30.01,30.26,30.51)
  lon_example = c(-90.61,-90.95,-91.23)
  latloncsv_example = c("30.01,-90.61", "30.26,-90.95", "30.51,-91.23")
  
  testthat::expect_no_error({
    x <- latlon2csv(lat = lat_example, lon = lon_example) 
  })
  testthat::expect_equal(
    latloncsv_example,
    x
  )
})
########################################################## #

testthat::test_that("latloncsv2nexus ok", {
  latloncsv_example = c("30.01,-90.61", "30.26,-90.95", "30.51,-91.23")
  nexus_example =  "30.01,-90.61;30.26,-90.95;30.51,-91.23"
  testthat::expect_no_error({
    x <- latloncsv2nexus(latloncsv = latloncsv_example)
  })
  testthat::expect_equal(
  nexus_example, 
  x
  )
})
########################################################## #

testthat::test_that("latlon2nexus ok", {
  
  lat_example = c(30.01,30.26,30.51)
  lon_example = c(-90.61,-90.95,-91.23)
  latloncsv_example = c("30.01,-90.61", "30.26,-90.95", "30.51,-91.23")
  nexus_example =  "30.01,-90.61;30.26,-90.95;30.51,-91.23"
  testthat::expect_no_error({
    x <- latlon2nexus(lat = lat_example, lon = lon_example)
  })
  testthat::expect_equal(
    nexus_example,
    x
  )
})
########################################################## #
