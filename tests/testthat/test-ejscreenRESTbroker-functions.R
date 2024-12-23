
# library(testthat)    
#   test_local()  to manually run all tests on local source pkg


testradius = 1
testlat <-  38.8959  # testpoints_50$lat[1]
testlon <- -77.02985 # testpoints_50$lon[1]
test2lat <- c(33.943883,    39.297209)
test2lon <- c(-118.241073, -76.641674)
pts <- data.frame(lat = test2lat, lon = test2lon)

suppressWarnings({
  outrest       <- ejscreenRESTbroker(lon = testlon, lat = testlat, radius = testradius)
  outrest2table <- ejscreenRESTbroker2table(outrest, getstatefromplacename = TRUE)
  out1          <- ejscreenapi1(lon = testlon,  lat = testlat, radius = testradius) # CAN SOMETIMES TAKE 30 SECONDS, SOMETIMES 5 SECONDS
  
})
# out_api       <- ejscreenapi(lon = test2lon, lat = test2lat, radius = testradius, on_server_so_dont_save_files = TRUE, save_when_report = FALSE, verbose = FALSE)
brokerout <- try(ejscreenRESTbroker(lon = testpoints_5$lon[1], lat = testpoints_5$lat[1], radius = testradius))
missing_api_results <- inherits(brokerout, "try-error")


################################ # ########################## # ejscreenRESTbroker ####

cat('\ntesting ejscreenRESTbroker\n')


testthat::test_that("ejscreenRESTbroker() works live", {
  # message("must be able to reach ejscreen server for this test to work")
  expect_no_error({
    x <- ejscreenRESTbroker(lon = testpoints_5$lon[1], lat = testpoints_5$lat[1], radius = testradius)
  })
})
testthat::test_that("ejscreenRESTbroker() returns ok status code 200 and an element called content", {
  expect_equal(outrest$status_code, 200)
  expect(ok =  'content' %in% names(outrest), failure_message = 'names(outrest) fails to contain "content"')
  # same as expect_in()
})
test_that("ejscreenRESTbroker() output class() is still identical to that of saved testoutput_ejscreenRESTbroker_1pts_1miles" , {
  skip_if(missing_api_results, "skipping because ejscreenRESTbroker() failed")
  expect_equal( class(brokerout), class(testoutput_ejscreenRESTbroker_1pts_1miles))
})
test_that("ejscreenRESTbroker() output names() are still identical to those of saved testoutput_ejscreenRESTbroker_1pts_1miles", {
  skip_if(missing_api_results, "skipping because ejscreenRESTbroker() failed")
  expect_equal(names(testoutput_ejscreenRESTbroker_1pts_1miles), names(brokerout))
})
test_that("ejscreenRESTbroker() output $headers$`content-type` is still identical to those of saved testoutput_ejscreenRESTbroker_1pts_1miles", {
  skip_if(missing_api_results, "skipping because ejscreenRESTbroker() failed")
  expect_equal(brokerout$headers$`content-type`, testoutput_ejscreenRESTbroker_1pts_1miles$headers$`content-type`)
})
test_that("ejscreenRESTbroker() output $headers$`content-type` is still identical to those of saved testoutput_ejscreenRESTbroker_1pts_1miles", {
  skip_if(missing_api_results, "skipping because ejscreenRESTbroker() failed")
  expect_equal(class(brokerout$content), class(testoutput_ejscreenRESTbroker_1pts_1miles$content))
})
# expect_equal(brokerout$content, testoutput_ejscreenRESTbroker_1pts_1miles$content)



################################ # ########################## # ejscreenRESTbroker2table ####

cat('\ntesting ejscreenRESTbroker2table\n')

## also could add here a test of the getstatefromplacename parameter now


test_that("ejscreenRESTbroker2table() does not crash on 1 point", {
  # NOTE IT IS NOT MEANT TO HANDLE 2+ POINTS AT A TIME
  cat('  testing 1 point in slow functions ejscreenRESTbroker2table(ejscreenRESTbroker( \n')
  expect_no_error( 
    ejscreenRESTbroker2table(
      ejscreenRESTbroker(lat = testpoints_5$lat[1], lon = testpoints_5$lon[1], radius = testradius)
      ) 
    )
  # CAN SOMETIMES TAKE 30 SECONDS, SOMETIMES 5 SECONDS
})

test_that('ejscreenRESTbroker2table() does not crash on 1 INVALID point (lat/lon swapped)', {
  cat('  testing 1 invalidpoint in slow functions ejscreenRESTbroker2table(ejscreenRESTbroker( \n')
  suppressWarnings({
  expect_no_error( ejscreenRESTbroker2table(ejscreenRESTbroker(lat = -100, lon = 30, radius = testradius)) )
  })  # CAN SOMETIMES TAKE 30 SECONDS, SOMETIMES 5 SECONDS
})
test_that('ejscreenRESTbroker2table(ejscreenRESTbroker()) returns 1-row data.frame', {
  expect_equal(class(outrest2table), 'data.frame')
  expect_equal(NROW(outrest2table), 1)
})
test_that('ejscreenRESTbroker2table(ejscreenRESTbroker()) results are same as ejscreenapi1(), except latter makes it numeric and drops percent signs', {
  expect_equal(names(outrest2table), names(out1))
  outrest2table$timeSeconds  <- NULL; out1$timeSeconds <- NULL  # because this is slightly different each time API used
  outrest2table$stateAbbr_from_api  <- NULL; out1$stateAbbr_from_api <- NULL  # numeric vs logical 
  outrest2table$stateName_from_api  <- NULL; out1$stateName_from_api <- NULL  # numeric vs logical 
  # # remove percent signs and makes numeric, which ejscreenapi1() does but ejscreenRESTbroker2table() does not
  outrest2table <- makenumericdfFORSHINY(outrest2table)
  expect_equal(outrest2table, out1)
})

### ## would be VERY SLOW: ### testoutput_ejscreenapi_plus_500 <- ejscreenapi_plus(testpoints_500, radius = 1, save_when_report = F, on_server_so_dont_save_files = T, verbose = F)

# TRY VARIOUS DISTANCES (RADIUS VALUES)
# 
##  # Example of problems is point 108 if just 1 mile radius
## testpoints_500[108, ]
##     sitenumber sitename       lon      lat
## 108        108 site 108 -104.5794 39.49929  # very low population density so the blocks are large 

##   ........ (more testdata) ####

xneg <- (ejscreenRESTbroker(lon = testpoints_500[108, 'lon'], lat = testpoints_500[108, 'lat'], radius = -1))
x0 <-   (ejscreenRESTbroker(lon = testpoints_500[108, 'lon'], lat = testpoints_500[108, 'lat'], radius = 0))
x1 <-   (ejscreenRESTbroker(lon = testpoints_500[108, 'lon'], lat = testpoints_500[108, 'lat'], radius = 1))
x3 <-   (ejscreenRESTbroker(lon = testpoints_500[108, 'lon'], lat = testpoints_500[108, 'lat'], radius = 3))

test_that("warned if radius small so no block point inside" , {
  suppressWarnings({
  expect_warning({
    xxneg <- ejscreenRESTbroker2table(xneg) # warning since too small circle, short radius
  })
  expect_warning({
    xx0 <- ejscreenRESTbroker2table(x0)     # warning since too small circle, short radius
  })
  expect_warning({
    xx1 <- ejscreenRESTbroker2table(x1)     # warning since too small circle, short radius 
  })
  expect_no_warning({
    xx3 <- ejscreenRESTbroker2table(x3)  # ok - radius large enough that some block point is found
  })
  })
  # expect_equal(names(xx3), names(xxneg))
  # expect_equal(names(xx3), names(xx0))
  # expect_equal(names(xx3), names(xx1))
  expect_equal(xx0$message, xx1$message)
  # expect_false("error" %in% names(xx0)) # "message" is the name there now
  
})




################################ #   ################################ #   ################################ #   
