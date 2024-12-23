################################ # ################################ #  ejscreenapi ####
cat('\ntesting ejscreenapi()\n')

test_that("ejscreenapi() works at all", {
  expect_no_error(
    suppressWarnings(
      ejscreenapi(lat = testpoints_5$lat[1:2 ], lon = testpoints_5$lon[1:2 ], radius = 0.5, save_when_report = F, verbose = F)
    )
  )
})
