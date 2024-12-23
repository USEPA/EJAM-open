################################ # ################################ # ejscreenapi1 ####
cat('\ntesting ejscreenapi1()\n')


testradius = 1
# testlat <-  38.8959  # testpoints_50$lat[1]
# testlon <- -77.02985 # testpoints_50$lon[1]
# test2lat <- c(33.943883,    39.297209)
# test2lon <- c(-118.241073, -76.641674)
# pts <- data.frame(lat = test2lat, lon = test2lon)
# 
# outrest       <- ejscreenRESTbroker(lon = testlon, lat = testlat, radius = testradius)
# outrest2table <- ejscreenRESTbroker2table(outrest, getstatefromplacename = TRUE)
# out1          <- ejscreenapi1(lon = testlon,  lat = testlat, radius = testradius) # CAN SOMETIMES TAKE 30 SECONDS, SOMETIMES 5 SECONDS
# # out_api       <- ejscreenapi(lon = test2lon, lat = test2lat, radius = testradius, on_server_so_dont_save_files = TRUE, save_when_report = FALSE, verbose = FALSE)
# brokerout <- try(ejscreenRESTbroker(lon = testpoints_5$lon[1], lat = testpoints_5$lat[1], radius = testradius))
# missing_api_results <- inherits(brokerout, "try-error")
# 
# 

test_that('ejscreenapi1() does not crash on 1 point', {
  cat('testing 1 point in slow functionsejscreenapi1 \n')
  # CAN SOMETIMES TAKE 30 SECONDS, SOMETIMES 5 SECONDS
  expect_no_error( ejscreenapi1(lat = testpoints_5$lat[1], lon = testpoints_5$lon[1], radius = testradius) )
})

test_that('ejscreenapi() does not crash on 2 points', {
  cat('testing 2 points in slow functionsejscreenapi1 \n')
  # CAN SOMETIMES TAKE 30 SECONDS, SOMETIMES 5 SECONDS
  suppressWarnings({
    expect_no_error( ejscreenapi(lat = testpoints_5$lat[1:2 ], lon = testpoints_5$lon[1:2 ], radius = testradius, on_server_so_dont_save_files = TRUE, save_when_report = FALSE))     
  })
})
