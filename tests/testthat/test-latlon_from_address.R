# test_that("latlon_from_address works at all, default params", {
#   testthat::skip_if_not_installed("AOI")
#   a1 <- "1200 Pennsylvania Ave NW, Washington DC"
#   a2 <- "4930 Old Page Road Durham NC 27703"
#   testthat::expect_no_error(latlon_from_address(a1))
#   testthat::expect_no_error(latlon_from_address(c(a1, a2)))
# })

offline_warning()

test_that("latlon_from_address( xy=TRUE) works", {
  
  testthat::skip_if_offline()
  testthat::skip_if_not_installed("AOI")
  
  if (!exists("geocode") || !is.function(geocode)) {
    expect_warning(
      # if pkg installed but not attached, warn and return NULL
      latlon_from_address("1200 Pennsylvania Ave NW, Washington DC")  
    )
    expect_null(
      latlon_from_address("1200 Pennsylvania Ave NW, Washington DC")  
    )
  }
  
  ## test should work if installed as long as we load/attach the package, if it is not already in Imports of DESCRIPTION of EJAM pkg.
  require("AOI")
  
  a1 <- "1200 Pennsylvania Ave NW, Washington DC"
  a2 <- "4930 Old Page Road Durham NC 27703"
  out1old <- structure(list(lon = -77.028948300066, lat = 38.8948262664), class = "data.frame", row.names = c(NA, -1L))
  out2old <- structure(list(lon = c(-77.028948300066, -78.841790425983), lat = c(38.8948262664, 35.887596591323)), class = "data.frame", row.names = c(NA, -2L))
  testthat::expect_no_error({
    out1 <- latlon_from_address(a1, xy = TRUE)
  })
  testthat::expect_no_error({
    out2 <- latlon_from_address(c(a1, a2), xy = TRUE)
  })
  testthat::expect_equal(out1, out1old)
  testthat::expect_equal(out2, out2old)
})

testthat::test_that("latlon_from_address( xy=FALSE) works", {
  
  testthat::skip_if_offline()
  testthat::skip_if_not_installed("AOI")
  require("AOI")
  
  a1 <- "1200 Pennsylvania Ave NW, Washington DC"
  a2 <- "4930 Old Page Road Durham NC 27703"
  out1old <- structure(list(request = "1200 Pennsylvania Ave NW, Washington DC",
                            score = 100L, arcgis_address = "1200 Pennsylvania Ave NW, Washington, District of Columbia, 20004",
                            lon = -77.028948300066, lat = 38.8948262664), row.names = c(NA, -1L), class = "data.frame")
  out2old <- structure(list(request = c("1200 Pennsylvania Ave NW, Washington DC",
                                        "4930 Old Page Road Durham NC 27703"),
                            score = c(100L, 100L),
                            arcgis_address = c("1200 Pennsylvania Ave NW, Washington, District of Columbia, 20004",
                                               "4930 Old Page Road, Durham, North Carolina, 27703"),
                            lon = c(-77.028948300066, -78.841790425983),
                            lat = c(38.8948262664, 35.887596591323)), row.names = c(NA, -2L), class = "data.frame")
  testthat::expect_no_error({
    out1 <- latlon_from_address(a1, xy = FALSE)
  })
  testthat::expect_no_error({
    out2 <- latlon_from_address(c(a1, a2), xy = FALSE)
  })
  testthat::expect_equal(out1, out1old)
  testthat::expect_equal(out2, out2old)
})

testthat::test_that("latlon_from_address( aoimap=T) works", {
  
  testthat::skip_if_offline()
  testthat::skip_if_not_installed("AOI")
  require("AOI")
  
  testthat::expect_no_error({
    x <- latlon_from_address("ames iowa", aoimap = T)
  })
  testthat::expect_true({"geometry" %in% names(x)})
})


