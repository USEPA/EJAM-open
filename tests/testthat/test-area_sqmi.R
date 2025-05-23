
############################################# #
## To read the test files and see number of polygons in each: ####

junk1 <- function() {
  
  fnames = testdata('shape', quiet = TRUE)
  fnames = fnames[fs::is_file(fnames)]
  # print(fnames)
  cat("\n")
  sh = list()
  for (i in seq_along(fnames)) {
    cat("Trying to read #", i, ":", fnames[i], "\n")
    trash <- capture.output({
      sh[[i]] <- try({shapefile_from_any(fnames[i])})
    }) 
    if (inherits(sh[[i]], "try-error")) {
      warning("failed to read ", fnames[i])
      sh[[i]] <- NA
    }
  }
  names(sh) <- basename(fnames)
  cat("-------------------------------------------------------------------------\n")
  x = data.frame(file = names(sh), rows = sapply(sh, NROW))
  rownames(x) <- NULL
  cat("Numbers of polygons:\n ")
  print(x)
  cat("-------------------------------------------------------------------------\n")
  invisible(sh)
}
## To read the test files and see number of polygons in each:
# consoleclear()
# shps <- junk1()
############################################# #


# ejamit() sitetype if analyzing points, shapes, and buffered shapes

junk2 <- function() {
  print("x1 <- mydf_ejam_analyzed_pts <- ejamit(testpoints_10, radius = 2) ")
  trash <- capture.output({
    x1 <- mydf_ejam_analyzed_pts <- ejamit(testpoints_10, radius = 2) 
  }) 
  cat('sitetype: ', x1$sitetype, '\n')
  #  latlon
  x1$results_bysite[,c("lat", "lon", "radius.miles")]
  cat("-------------------------------------------------------------------------\n")
  
  print("x2 <- mydf_ejam_analyzed_polygons <- ejamit(shapefile = testshapes_2, radius = 0) ")
  trash <- capture.output({
    x2 <- mydf_ejam_analyzed_polygons <- ejamit(shapefile = testshapes_2, radius = 0) 
  }) 
  cat('sitetype: ', x2$sitetype, '\n')
  # shp
  x2$results_bysite[,c("lat", "lon", "radius.miles")]
  cat("-------------------------------------------------------------------------\n")
  
  print("x3 <- mydf_ejam_analyzed_polygons_PLUS_BUFFER <- ejamit(shapefile = testshapes_2, radius = 0.5) ")
  trash <- capture.output({
    x3 <- mydf_ejam_analyzed_polygons_PLUS_BUFFER <- ejamit(shapefile = testshapes_2, radius = 0.5) 
  }) 
  cat('sitetype: ', x3$sitetype, '\n')
  #  shp
  x3$results_bysite[,c("lat", "lon", "radius.miles")]
  cat("-------------------------------------------------------------------------\n")
  
  print('x4 <- other <- ejamit(fips = fips_counties_from_state_abbrev("DE"))')
  trash <- capture.output({
    x4 <- other <- ejamit(fips = fips_counties_from_state_abbrev("DE")) 
  }) 
  cat('sitetype: ', x4$sitetype, '\n')
  #  shp
  x4$results_bysite[,c("lat", "lon", "radius.miles")]
  cat("-------------------------------------------------------------------------\n")
  
  ############################################# #
}
# 
# consoleclear()
# junk2()
rm(junk1, junk2)
############################################# #  ############################################# #

## to be finished possibly... ***

# test_that("area_sqmi ok within ejamit", {
#   expect_no_error({
#     
#     # to test area_sq() usage in ejamit() (but also see how server uses it)
#     # 
#     x1 = ejamit(shapefile = testshapes_2)
#     x1b = ejamit(shapefile = testshapes_2, radius = 0.25)
#     
#     x2 = ejamit(shapefile = shapes_from_fips(fips_counties_from_state_abbrev("DE")))
#     
#     x3 = ejamit(fips = fips_counties_from_state_abbrev("DE"), download_fips_bounds_to_calc_areas = T)
#     x3b = ejamit(fips = fips_counties_from_state_abbrev("DE"), download_fips_bounds_to_calc_areas = F)
#     
#     x4 = ejamit(testpoints_100, radius = 10)
#   })
#   
#   
#   # for (this in list(x1,x1b, x2, x3, x3b, x4)) {
#   #   
#   #   cat(this$sitetype, " - ")
#   #   cat(this$results_overall$area_sqmi, '\n\n')
#   #   
#   # }
#   
# })
############################################# #

# area_sqmi_from_pts ####

test_that("area_sqmi_from_pts no error", {
  expect_no_error({
    area_sqmi_from_pts(3)
    area_sqmi_from_pts(rep(3, 10))
    expect_equal(
      area_sqmi_from_pts(1:3),
      pi * (1:3)^2
    )
    area_sqmi_from_pts(0)
    expect_true(
      is.na(area_sqmi_from_pts(NA))
    )
  })
})

test_that("area_sqmi_from_pts errors handled", {
  expect_error({
    area_sqmi_from_pts(testpoints_10)
    area_sqmi_from_pts("TEXT")
  })
})

test_that("area_sqmi_from_pts calculation ok", {
  expect_equal(
    area_sqmi_from_pts(1:3), pi * (1:3)^2
  )
})
############################################# #

# area_sqmi_from_shp ####

test_that("area_sqmi_from_shp ok", {
  expect_no_error({
    expect_equal(
      area_sqmi_from_shp(testshapes_2),
      c(3.6879370, 0.1769883)
    )
  })
})
############################################# #

# area_sqmi_from_fips ####

test_that("area_sqmi_from_fips() ok", {
  
  expect_no_error({
    junk <- capture_output({
      x <- area_sqmi_from_fips(fips = blockgroupstats$bgfips[40000])
    })
  })
  expect_true(x > 2.4 & x < 2.5)
})
############################################# #

# area_sqmi_from_table ####

test_that("area_sqmi_from_table() ok", {
  expect_no_error({
    expect_equal(
      pi * c(3,3)^2,
      area_sqmi_from_table(df = data.frame(radius = c(3,3), lat = c(1,2), lon = c(3,4)))
    )  })
})
########################## #

# area_sqmi() ####

test_that("area_sqmi(df) has colname radius", {
  expect_no_error({
    expect_true(
      2 == length(area_sqmi(df = data.frame(radius = c(3,3), lat = c(1,2), lon = c(3,4))))
    )
  })
})
test_that("area_sqmi(df) has colname radius.miles", {
  expect_no_error({
    expect_true(
      2 == length(area_sqmi(df = data.frame(radius.miles = c(3,3), lat = c(1,2), lon = c(3,4))))
    )  })
})
test_that("area_sqmi(df) has colname Radius", {
  expect_no_error({
    expect_equal(
      pi * c(3,3)^2,
      area_sqmi(df = data.frame(Radius = c(3,3), lat = c(1,2), lon = c(3,4)))
    )  })
})
######## # 

test_that("area_sqmi(shp)", {
  expect_no_error({
    expect_equal(
      area_sqmi(shp = testshapes_2),
      c(3.6879370, 0.1769883)
    )
  })
})
######## # 
test_that("area_sqmi(fips)", {
  expect_no_error({
    junk <- capture_output({
      x <- area_sqmi(fips = blockgroupstats$bgfips[40000])
    })
  })
  expect_true(x > 2.4 & x < 2.5)
})
######## # 
test_that("area_sqmi handles error >1 param", {
  expect_error({
    area_sqmi(df = testpoints_10, radius.miles = 3)
  })
})
test_that("area_sqmi handles table without usable colnames", {
  expect_warning({
    expect_equal(
      area_sqmi(testpoints_10),
      rep(NA, 10)
    )
  })
})
########################## #
