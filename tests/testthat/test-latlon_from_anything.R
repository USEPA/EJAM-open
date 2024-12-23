
################################################ #

testthat::test_that("latlon_from_anything works with data.frames", {
  
  expect_no_error({
    x <- latlon_from_anything(testpoints_100[1:6, ])
  })
  expect_equal(
    NROW(x), 6
  )
  expect_true(
    all(c("lat", "lon") %in% names(x))
  )
  expect_no_error({
    x <- latlon_from_anything(testpoints_100[1:6, c('lat','lon')] )
  })
  expect_true(
    all(c("lat", "lon") %in% names(x))
  )
})
################################################ #

testthat::test_that("latlon_from_anything returns lat,lon as colnames even if aliases provided and order differs", {
  suppressWarnings({
    x <- latlon_from_anything(data.frame(n = 1, Longitude = -110, other = 1, latitude = 33))
  })
  expect_true(
     x$lat == 33 & x$lon == -110
  )
})
################################################ #

testthat::test_that("latlon_from_anything prefers lat,lon if other aliases also exist", {
  expect_no_error({
   x <- latlon_from_anything(data.frame(lat = 32, lon = -100, longitude = 0, Latitude = 0)) 
  })
  expect_true({
    all(c("lat", "lon") %in% names(x)) & "longitude" %in% names(x) & x$lat == 32 & x$lon == -100 
  })
})
################################################ #

testthat::test_that("latlon_from_anything warns if lat, lon switched", {
 expect_warning(
   expect_warning(
   expect_warning({
    x <- latlon_from_anything(testpoints_100$lon[1:6], testpoints_100$lat[1:6])
  })
 ))
})

testthat::test_that("latlon_from_anything works with x, y vectors", {
  expect_no_error({
    x <- latlon_from_anything(testpoints_100$lat[1:6], testpoints_100$lon[1:6])
    y <- latlon_from_anything(testpoints_100[1:6, ])
  })
  expect_identical(x[, c("lat", "lon")], y[, c("lat", "lon")])
})
################################################ #

testthat::test_that("latlon_from_anything works with 1-row data.frame", {
  expect_no_error({
    x <- latlon_from_anything(data.frame(lat = testpoints_10$lat[1], lon = testpoints_10$lon[1])) 
    y <- latlon_from_anything(testpoints_10$lat[1], testpoints_10$lon[1])
  })
  expect_equal(x$lat, y$lat)
  expect_equal(x$lon, y$lon)
  
  # expect_equal(x, y)  # NOT true because column order depends on data.frame provided
  ## note that order of columns returned will match data.frame provided but is always lon, lat  if vectors of x,y provided:
  # >  latlon_from_anything(y = testpoints_10$lat[1], x =  testpoints_10$lon[1])
  # lon     lat valid
  # 1 -83.369 30.9774  TRUE
  # >  latlon_from_anything(x =  testpoints_10$lon[1], y = testpoints_10$lat[1])
  # lon     lat valid
  # 1 -83.369 30.9774  TRUE
  # > latlon_from_anything(data.frame(lat = testpoints_10$lat[1], lon = testpoints_10$lon[1])) 
  # lat     lon valid
  # 1 30.9774 -83.369  TRUE
  # > latlon_from_anything(data.frame(lon = testpoints_10$lon[1], lat = testpoints_10$lat[1])) 
  # lon     lat valid
  # 1 -83.369 30.9774  TRUE
})
################################################ #

testthat::test_that("latlon_from_anything accepts data.table or data.frame, ***returns data.table now***", {
  
 expect_no_error({
   # check 1-row dt
   x <- latlon_from_anything(data.table::data.table(lat = testpoints_10$lat, lon = testpoints_10$lon)[1, ])
 })
  expect_no_error({
    x <- latlon_from_anything(data.table::data.table(lat = testpoints_10$lat, lon = testpoints_10$lon))
  })
  expect_true(
    ("data.frame" %in% class(x)) & "data.table" %in% class(x)
  )
  expect_identical(
    latlon_from_anything(data.frame(            lat = testpoints_10$lat, lon = testpoints_10$lon)),
    latlon_from_anything(data.table::data.table(lat = testpoints_10$lat, lon = testpoints_10$lon))
  )
})
################################################ #

testthat::test_that("latlon_from_anything works with tibbles", {
  
  expect_no_error({
    x <- latlon_from_anything(tibble(lat = testpoints_10$lat, lon = testpoints_10$lon))
    y <- latlon_from_anything(data.frame(lat = testpoints_10$lat, lon = testpoints_10$lon))
  })
  expect_equal(
    x,
    y ,
    ignore_attr = TRUE
  )
})
################################################ #

testthat::test_that("latlon_from_anything works with csv", {
  
  tfile <- tempfile("junk", fileext = ".csv")
  write.csv(testpoints_10[1:2, ], file = tfile, row.names = FALSE)
  skip_if(!file.exists(tfile))
  x <- latlon_from_anything(tfile)
  y <- latlon_from_anything(testpoints_10[1:2, ])
  expect_equal(x, y, ignore_attr = TRUE)
})
################################################ #

testthat::test_that("latlon_from_anything works with xlsx", {
  
  fname <- system.file("./testdata/latlon/testpoints_10.xlsx", package = "EJAM")
  #fname <- "./inst/testdata/latlon/testpoints_10.xlsx"
  skip_if(!file.exists(fname))
  
  expect_no_error(
    {x <- latlon_from_anything(fname)}
  )  
  
})
################################################ #

testthat::test_that("latlon_from_anything works with matrix", {
  
  x <- matrix(c(34, 35, -110, -111), nrow = 2, ncol = 2)
  colnames(x) <- c("Latitude", "Longitude")
  expect_no_error({
    suppressWarnings(
      {
        x <- latlon_from_anything(x)
      }
    )
  })
  expect_identical(
    colnames(x),
    c("lat",   "lon",   "valid", "invalid_msg")
  )
})
################################################ #
