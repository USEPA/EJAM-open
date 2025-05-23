################################ # ################################ # ejscreenit ####
cat('\ntesting ejscreenit()\n')

apiok = EJAM:::ejscreenapi_online() # for   skip_if_not(apiok, message = "ejscreen API not available")

# pts <- data.frame(lat = test2lat, lon = test2lon)
#
# outrest       <- ejscreenRESTbroker(lon = testlon, lat = testlat, radius = testradius)
# outrest2table <- ejscreenRESTbroker2table(outrest, getstatefromplacename = TRUE)
# out1          <- ejscreenapi1(lon = testlon,  lat = testlat, radius = testradius) # CAN SOMETIMES TAKE 30 SECONDS, SOMETIMES 5 SECONDS
# # out_api       <- ejscreenapi(lon = test2lon, lat = test2lat, radius = testradius, on_server_so_dont_save_files = TRUE, save_when_report = FALSE, verbose = FALSE)
# brokerout <- try(ejscreenRESTbroker(lon = testpoints_5$lon[1], lat = testpoints_5$lat[1], radius = testradius))
# missing_api_results <- inherits(brokerout, "try-error")




test_that("ejscreenit() works at all", {
  skip_if_not(apiok, message = "ejscreen API not available")
  testradius = 1
  testlat <-  38.8959  # testpoints_50$lat[1]
  testlon <- -77.02985 # testpoints_50$lon[1]
  test2lat <- c(33.943883,    39.297209)
  test2lon <- c(-118.241073, -76.641674)
  expect_no_error(  suppressMessages({
    suppressWarnings({
      junk <-  ejscreenit(testpoints_5[1:2, ], radius = 0.5, nosave = T, nosee = T, interactiveprompt = F,
                          calculate_ratios = F)
    })  })
  )
})

if (!apiok) {
  cat("Warned already that API is not available - skipping other tests in this file \n")

} else {

test_that('ejscreenit() does not crash for 2 points x= lons, y = lats', {
  skip_if_not(apiok, message = "ejscreen API not available")
  testradius = 1
  testlat <-  38.8959  # testpoints_50$lat[1]
  testlon <- -77.02985 # testpoints_50$lon[1]
  test2lat <- c(33.943883,    39.297209)
  test2lon <- c(-118.241073, -76.641674)
  expect_no_error({
    ## returns warnings about invalid sites throughout analysis, but no errors
    suppressWarnings({
      out_ejscreenit_separate_lat_lon <- ejscreenit(
        x = test2lon, y = test2lat, radius = testradius,
        nosave = TRUE, nosee = TRUE, interactiveprompt = FALSE,
      )
    })
  } )
})

test_that("ejscreenit() still returns list with names identical to what it used to return (saved as testoutput_ejscreenit_5)", {
  skip_if_not(apiok, message = "ejscreen API not available")
  testradius = 1
  testlat <-  38.8959  # testpoints_50$lat[1]
  testlon <- -77.02985 # testpoints_50$lon[1]
  test2lat <- c(33.943883,    39.297209)
  test2lon <- c(-118.241073, -76.641674)
  apiref$`Seconds elapsed obtaining data` = NULL
  apinow$`Seconds elapsed obtaining data` = NULL
  expect_identical(
    names(apiref_list),   #  "table" "map"   "plot"
    names(apinow_list2)   #  "table" "map"   "plot"
  )
})
test_that("ejscreenit() still returns a list that includes a table with names identical to the one it used to return (saved as testoutput_ejscreenit_5$table)", {
  skip_if_not(apiok, message = "ejscreen API not available")
  # testradius = 1
  # testlat <-  38.8959  # testpoints_50$lat[1]
  # testlon <- -77.02985 # testpoints_50$lon[1]
  # test2lat <- c(33.943883,    39.297209)
  # test2lon <- c(-118.241073, -76.641674)
  apiref$`Seconds elapsed obtaining data` = NULL
  apinow$`Seconds elapsed obtaining data` = NULL
  expect_identical(
    names(apiref),
    names(apinow)
  )
})
test_that("ejscreenit() still returns a list that includes a table with column classes identical to the ones it used to return (saved as testoutput_ejscreenit_5$table)", {
  skip_if_not(apiok, message = "ejscreen API not available")
  apiref$`Seconds elapsed obtaining data` = NULL
  apinow$`Seconds elapsed obtaining data` = NULL
  expect_identical(
    sapply(apiref, class),
    sapply(apinow, class),ignore_attr = TRUE
  )
  # all.equal(apiref, apinow)
})
test_that("ejscreenit() still returns identical table contents to what it used to", {
  skip_if_not(apiok, message = "ejscreen API not available")
  apiref$`Seconds elapsed obtaining data` = NULL
  apinow$`Seconds elapsed obtaining data` = NULL
  expect_identical(
    apiref,
    apinow, ignore_attr = TRUE
  )
})
test_that('ejscreenit() does not crash, for 2 points, x=pts; list of 3 outputs of correct class. table right NROW', {
  skip_if_not(apiok, message = "ejscreen API not available")
  testradius = 1
  testlat <-  38.8959  # testpoints_50$lat[1]
  testlon <- -77.02985 # testpoints_50$lon[1]
  test2lat <- c(33.943883,    39.297209)
  test2lon <- c(-118.241073, -76.641674)
  apiref$`Seconds elapsed obtaining data` = NULL
  apinow$`Seconds elapsed obtaining data` = NULL
  expect_no_error({
    suppressWarnings(
      suppressMessages({
        out_ejscreenit <- ejscreenit(
          x = pts, radius = testradius,
          nosave = TRUE, nosee = TRUE, interactiveprompt = FALSE
        )
      } )
    )

  })
  # should be a data.frame, etc.
  expect_type(out_ejscreenit, 'list')
  expect_identical(names(out_ejscreenit), c('table', 'map', 'plot'))
  expect_identical(class(out_ejscreenit$table), 'data.frame')
  expect_true('leaflet' %in% class(out_ejscreenit$map))
  expect_true('ggplot' %in% class(out_ejscreenit$plot))
  expect_identical(NROW(out_ejscreenit$table) , NROW(pts))

})

rm(
  testradius,
  testlat,
  testlon,
  test2lat,
  test2lon,
  pts
)


# add other test cases here for various input parameters and conditions
}