################################ # ################################ # ejscreenapi_plus ####
cat('\ntesting ejscreenapi_plus()\n')


testradius = 1
testlat <-  38.8959  # testpoints_50$lat[1]
testlon <- -77.02985 # testpoints_50$lon[1]
test2lat <- c(33.943883,    39.297209)
test2lon <- c(-118.241073, -76.641674)
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

test_that("ejscreenapi_plus() works at all", {
  expect_no_error(
    suppressWarnings({
      junk = capture.output({
        ejscreenapi_plus( testpoints_5[1:2, ], radius = 0.5, 
                          save_when_report = F, on_server_so_dont_save_files = T, 
                          calculate_ratios = F,
                          verbose = FALSE)  
      })
    })
  )
})
test_that('ejscreenapi_plus() does not crash for 1 point', {
  expect_no_error({
    suppressMessages(
      suppressWarnings({
    
          outplus <- ejscreenapi_plus(x = testlon, y = testlat, radius = testradius, 
                                               on_server_so_dont_save_files = T, save_when_report = F, verbose = FALSE)
        })
      )
  })
  expect_equal(NROW(outplus), 1)
})
test_that('ejscreenapi_plus() does not crash for 2 points, outputs list (data.frame) of 2 rows', {
  expect_no_error({
      suppressMessages(
        suppressWarnings({
          outplus2 <- ejscreenapi_plus(x = test2lon, y = test2lat, radius = testradius, 
                                       on_server_so_dont_save_files = T, save_when_report = F, verbose = FALSE) 
        })
      )

    }) 
  expect_type(outplus2, "list")
  expect_identical(class(outplus2), 'data.frame')
  expect_equal(NROW(outplus2), 2)
})

# *** add a test for each of these settings in ejscreenapi_plus(

# format_report_or_json = "report"

# calculate_ratios = T # default

# usewhichnames = 'long'

# unit = "km"

# mapping_for_names = map_headernames

