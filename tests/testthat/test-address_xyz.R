
#  FUNCTIONS


# address_from_table_goodnames()
# address_from_table()

# latlon_from_address_table()
# latlon_from_address()


################################ #

# TEST DATA

# test_address_9
# test_address_parts1
# test_addresses2  # "1200 Pennsylvania Ave, NW Washington DC" "Research Triangle Park"
# test_address_table
# test_address_table_withfull
# test_address_table_goodnames

## fname <- system.file("testdata/address/street_address_9.xlsx", package = "EJAM")

offline_warning()

################################ #

testthat::test_that("address_from_table_goodnames works", {

  testthat::skip_if_offline()
  testthat::expect_no_error({
    x <- address_from_table_goodnames(test_address_table_goodnames)
  })
  testthat::expect_identical(x,  c("1200 Pennsylvania Ave Washington DC ", "5 pARK AVE NY NY "))
})
###################### #

testthat::test_that("address_from_table works", {
  
  testthat::skip_if_offline()
  
  ### address_from_table() works with filename??
  ## fname <- system.file(  ..........................)
  ### pts <- address_from_table(fname)
  
  testthat::expect_no_error({
    x <- address_from_table(test_address_table)
  })
  testthat::expect_identical(x, c("1200 Pennsylvania Ave Washington DC ", "5 pARK AVE NY NY "))
  
  testthat::expect_no_error({
    x <- address_from_table(test_address_table_goodnames)
  })
  testthat::expect_identical(x, c("1200 Pennsylvania Ave Washington DC ", "5 pARK AVE NY NY "))
})

# testthat::test_that("address_from_table works in odd case (Address colname has different FULL address than STREET etc do)", {
# 
# testthat::skip_if_offline()
# suppressWarnings({
#     testthat::expect_no_error({
#       
#       x <- address_from_table(test_address_table_withfull)
#       
#     })
#     testthat::expect_identical(
#       x,
#       c("1200 Pennsylvania Ave, NW Washington DC", "Research Triangle Park"))
#   })
# })
###################### #

testthat::test_that("latlon_from_address works", {
  
  testthat::skip_if_offline()
  skip_if_not_installed("AOI")
  if (!exists("geocode")) {
    library(AOI)
    cat("MUST LOAD AOI PKG FOR THIS geocode to work \n\n")
  }
  addresses_example_temp = c("1200 Pennsylvania Ave NW, Washington, District of Columbia, 20004",
                             "5 Park Ave, New York, New York, 10016")
  testthat::expect_no_error({
    x <- latlon_from_address(addresses_example_temp)
  })
  
  testthat::expect_identical(
    x,
    structure(list(
      request = c("1200 Pennsylvania Ave NW, Washington, District of Columbia, 20004", 
                  "5 Park Ave, New York, New York, 10016"), 
      score = c(100L, 100L), 
      arcgis_address = c("1200 Pennsylvania Ave NW, Washington, District of Columbia, 20004", 
                         "5 Park Ave, New York, New York, 10016"), 
      lon = c(-77.028948300066, -73.980999465092), 
      lat = c(38.8948262664, 40.747143677784)
    ), row.names = c(NA, -2L), class = "data.frame")
  )
})
###################### #

testthat::test_that("latlon_from_address err if too many addresses", {
  
  testthat::skip_if_offline()
  testthat::expect_error(latlon_from_address(rep("a", 1001)))
  })
###################### #

testthat::test_that("latlon_from_address_table works on test_address_table", {
  
  testthat::skip_if_offline()
  skip_if_not_installed("AOI")
  testthat::expect_no_error({
    x <- latlon_from_address_table(test_address_table)
  })
  testthat::expect_identical(x, structure(list(
    request = c("1200 Pennsylvania Ave Washington DC ",
                "5 pARK AVE NY NY "),
    score = c(99.48, 100),
    arcgis_address = c("1200 Pennsylvania Ave NW, Washington, District of Columbia, 20004",
                       "5 Park Ave, New York, New York, 10016"),
    lon = c(-77.028948300066, -73.980999465092),
    lat = c(38.8948262664, 40.747143677784)
  ), row.names = c(NA, -2L), class = "data.frame"))
  
  testthat::expect_no_error({
    x <- latlon_from_address_table(test_address_table_withfull)
  })
})
###################### #

testthat::test_that("odd case- latlon_from_address_table works on test_address_table_withfull", {
  
  testthat::skip_if_offline()
  skip_if_not_installed("AOI")
  
  x <- latlon_from_address_table(test_address_table_withfull)
  
  testthat::expect_identical(
    x[,c("arcgis_address", "lon", "lat")],
    structure(list(
      arcgis_address = c(
        "1200 Pennsylvania Ave NW, Washington, District of Columbia, 20004",
        "5 Park Ave, New York, New York, 10016"), 
      lon = c(-77.028948300066, -73.980999465092), 
      lat = c(38.8948262664, 40.747143677784)
    ), 
    class = "data.frame", row.names = c(NA, -2L))
    
    # structure(list(
    #   request = c("1200 Pennsylvania Ave, NW Washington DC", "Research Triangle Park"),
    #   score = c(100L, 100L),
    #   arcgis_address = c("1200 Pennsylvania Ave NW, Washington, District of Columbia, 20004",
    #                      "Research Triangle Park, North Carolina"),
    #   lon = c(-77.028948300066, -78.86228),
    #   lat = c(38.8948262664, 35.90831)
    # ), row.names = c(NA, -2L), class = "data.frame")
  )
})

## *** NOTE IT FAILS or has trouble IF A COLUMN WITH STREET NAME ONLY IS CALLED "address" instead of that storing the full address.

