
#   This is a helper func to simulate ejamit() relying on ejamit_sitetype_from_input()
#   where inputs to ejamit() get checked by ejamit_sitetype_from_input() 
# 
## This confirmed  ejamit() could successfully use the function ejamit_sitetype_from_input() 
## to examine the params passed to ejamit()
## and it would work as expected instead of needing the code to be inside ejamit()
## The params passed to the outer func ejamit() 
## when they are missing or NULL inputs to ejamit() or similar functions
## do in fact get handled as missing() or NULL 
## inputs to the inner ejamit_sitetype_from_input() func when passed through like this.

ejamit_test = function(sitepoints, fips=NULL, shapefile=NULL) {
  
  sitetype <- ejamit_sitetype_from_input(sitepoints = sitepoints, fips = fips, shapefile = shapefile)

  cat("          is sitepoints reported as missing?   ")
  cat(missing(sitepoints), ' ...  ')
  cat('sitetype: \n')
  return(sitetype)
}
################################################ #

test_that(" if 0 of 3 is provided - assume latlon", {
  # it would normally try to interactively select file of latlon but this ejamit_test() avoids that
  testthat::skip_if_not(interactive())
  expect_no_error({
    sitetype <- ejamit_test( )
  })
  expect_equal(sitetype, "latlon")
})
############# # 

test_that("if 1 is provided - note which", {
  expect_no_error({
    s1 <- ejamit_test(1)
    s2 <- ejamit_test(sitepoints = 1)
    s3 <- ejamit_test(fips = 2)
    s4 <- ejamit_test(shapefile = 3)
  })
  expect_equal(s1, "latlon")
  expect_equal(s2, "latlon")
  expect_equal(s3, "fips")
  expect_equal(s4, "shp")
})
############# # 

test_that("if 2 are provided - stop or warn", {
  expect_error({
    ejamit_test(sitepoints = 1, fips = 2) 
  })
  expect_error({
    ejamit_test(sitepoints = 1, shapefile = 3)
  })
  expect_error({
    ejamit_test(fips = 2, shapefile = 3)
  })
})
############# # 

test_that("if 3 are provided - stop or warn", {
  expect_error({
    ejamit_test(sitepoints = 1, fips = 2, shapefile = 3)
  })
})
############# # 

rm(ejamit_test)

################################################ #

###  ALT VERSION WHERE WE COULD JUST WARN NOT STOP WHEN 2 ARE SPECIFIED BY ACCIDENT:
# 
# ejamit_sitetype_from_input_warn <- function(sitepoints, fips=NULL, shapefile=NULL) { 
#   
#   if (!is.null(shapefile)) {
#     sitetype <- "shp"
#   } else if (!is.null(fips)) {
#     sitetype <- "fips"
#   } else {
#     sitetype <- "latlon" # if none of 3 is specified, tries to interactively select file of latlon
#   }
#   if (sum(!missing(sitepoints), !is.null(shapefile), !is.null(fips)) > 1) {
#     # stop("2 or more of the 3 parameters 'sitepoints', 'shapefile', 'fips' were provided, but must specify only 1 of the 3. ")
#     ## or, if we want to warn instead of stop, 
#     ## could use only latlon when avail even if fips and/or shp was also erroneously specified, & use shp if only shp&fips specified.
#     if (!missing(sitepoints)) {sitetype <- "latlon"}
#     warning("2 or more of the 3 parameters 'sitepoints', 'shapefile', 'fips' were provided, but must specify only 1 of the 3. Using sitepoints if provided. If not, ignoring fips and using shapefile.")
#   }
#   
#   if (sitetype == "latlon" & missing(sitepoints)) {message("ejamit() will try to help select a latlon file")}
#   return(sitetype)
# }
# ############################ ############################# #

# # ejamit_sitetype_from_input_warn() test cases
# for 3 site types: sitepoints, fips, shapefile 
# 
# # if 0 of 3 is provided - try to interactively select file of latlon
# 
# # if 1 is provided - note which
# ejamit_sitetype_from_input_warn(1)
# ejamit_sitetype_from_input_warn(sitepoints=1)
# ejamit_sitetype_from_input_warn(fips=2)
# ejamit_sitetype_from_input_warn(shapefile = 3)
# ejamit_sitetype_from_input_warn(pts<-1)
# 
# # if 2 are provided - stop or warn
# try({  ejamit_sitetype_from_input_warn(sitepoints=1, fips=2)  })
# try({  ejamit_sitetype_from_input_warn(sitepoints=1, shapefile = 3)  })
# try({  ejamit_sitetype_from_input_warn(fips=2, shapefile = 3)  })
# 
# # if 3 are provided - stop or warn
# # stops (not warns) and assumes 
# try({   ejamit_sitetype_from_input_warn(sitepoints=1, fips=2, shapefile=3)  })
