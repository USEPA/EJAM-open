# Tests for shapefile_ functions
################################################################ #

# shapefile_from_json(testfilename_json)
# shapefile_from_zip(testfilename_zipdir)    # shapefile_from_zip(testfilename_zipdir2)  #  shapefile_from_zip(testfilename_zipshp)
# shapefile_from_gdb(testfilename_gdb)
# shapefile_from_gdbzip(testfilename_gdbzip)
# shapefile_from_folder(testfilename_dirshp)
# shapefile_from_filepaths(testfilenameset_4)  # shapefile_from_filepaths(testfilename_shp_alone)
# shapefile_from_sitepoints(testpoints_10) 
# 
# shapefile_filepaths_from_folder(testfilename_dirshp)
# shapefile_filepaths_valid(testfilenameset_4)
# shapefile_filepaths_validize(testfilename_shp_alone)

# shapefile_clean(testshapes_2)
# shape_buffered_from_shapefile(testshapes_2)

# shape_buffered_from_shapefile_points(testshape_points)
# shapefile2latlon(testshape_points)
# latlon_from_shapefile(testshape_points)

# shapefile_from_any(   )

################################################################ #
testthat::test_that("test data files are available", {
  
  expect_no_error({
    testfilename_dirshp    <- system.file("testdata/shapes/portland_folder_shp",     package = "EJAM")
    testfilename_gdb       <- system.file("testdata/shapes/portland.gdb",            package = "EJAM")
    testfilename_gdbzip    <- system.file("testdata/shapes/portland.gdb.zip",        package = "EJAM")
    testfilename_zipdir    <- system.file("testdata/shapes/portland_folder_shp.zip", package = "EJAM")
    testfilename_zipdir2   <- system.file("testdata/shapes/portland_shp.zip",        package = "EJAM") # .shp etc basenames are NOT same as  .zip file basename
    testfilename_zipshp    <- system.file("testdata/shapes/stations.zip",            package = "EJAM") # .shp etc basenames ARE IDENTICAL TO .zip file basename
    testfilename_json      <- system.file("testdata/shapes/portland.json",           package = "EJAM")
    testfilename_shp_alone <- system.file("testdata/shapes/portland_folder_shp/Neighborhoods_regions.shp", package = "EJAM") # Neighborhoods_regions.shp
    testfilenameset_4 <- shapefile_filepaths_validize(testfilename_shp_alone)
    junk <- capture.output({
    testshape        <- shapefile_from_folder(testfilename_dirshp)
    # could also use  testshapes_2
    })
    testshape_points <- shapefile_from_sitepoints(testpoints_10) 
  })
  
  expect_true(file.exists(testfilename_dirshp))
  expect_true(file.exists(testfilename_gdb))
  expect_true(file.exists(testfilename_gdbzip))
  expect_true(file.exists(testfilename_zipdir))
  expect_true(file.exists(testfilename_zipdir2))
  expect_true(file.exists(testfilename_zipshp))
  expect_true(file.exists(testfilename_json))
  expect_true(file.exists(testfilename_shp_alone))
  expect_true(all(file.exists(testfilenameset_4)))
  
  #   list.files(system.file("testdata/shapes/", package = "EJAM"))
  #  ##"portland.gdb"  "portland.gdb.zip"  "portland.json"  
  #  ##"portland_folder_shp"  "portland_folder_shp.zip"
  #  ##"portland_shp.zip"  ### "stations_shp.zip" "stations.zip"
})
################################################################ #

################################################################ #
testthat::test_that("shapefile_from_json(testfilename_json) not crash", {
  testfilename_json      <- system.file("testdata/shapes/portland.json", package = "EJAM")
  expect_no_error({suppressWarnings({
    junk <- capture.output({
      JUNK <- shapefile_from_json(testfilename_json)
    })
  })})
  expect_true(
    "sf" %in% class(JUNK)
  )
  rm(JUNK)
})
######################################################### #        works
testthat::test_that("shapefile_from_zip(testfilename_zipdir)  not crash", {
  testfilename_zipdir    <- system.file("testdata/shapes/portland_folder_shp.zip", package = "EJAM")
  expect_no_error({suppressWarnings({
    junk <- capture.output({
      JUNK <- shapefile_from_zip(testfilename_zipdir)
    })
  })})
  expect_true(
    "sf" %in% class(JUNK)
  )
  rm(JUNK)
})
######################################################### #                     FAILS NOW ?    
testthat::test_that("shapefile_from_zip(testfilename_zipdir2) not crash", {
  testfilename_zipdir2   <- system.file("testdata/shapes/portland_shp.zip", package = "EJAM") # .shp etc basenames are NOT same as  .zip file basename
  expect_no_error({suppressWarnings({
    junk <- capture.output({
      JUNK <- shapefile_from_zip(testfilename_zipdir2)
    })
  })})
  expect_true(
    "sf" %in% class(JUNK)
  )
  rm(JUNK)
})
######################################################### #                     FAILS NOW ?  
testthat::test_that("shapefile_from_zip(testfilename_zipshp) not crash", {
  testfilename_zipshp    <- system.file("testdata/shapes/stations.zip", package = "EJAM") # .shp etc basenames ARE IDENTICAL TO .zip file basename
  expect_no_error({suppressWarnings({
    junk <- capture.output({
      JUNK <- shapefile_from_zip(testfilename_zipshp)
    })
  })})
  expect_true(
    "sf" %in% class(JUNK)
  )
  rm(JUNK)
})
######################################################### #
testthat::test_that("shapefile_from_gdb(testfilename_gdb) not crash", {
  testfilename_gdb       <- system.file("testdata/shapes/portland.gdb", package = "EJAM")
  expect_no_error({suppressWarnings({
    junk <- capture.output({
      JUNK <- shapefile_from_gdb(testfilename_gdb)
    })
  })})
  expect_true(
    "sf" %in% class(JUNK)
  )
  rm(JUNK, junk)
})
######################################################### # 
testthat::test_that("shapefile_from_gdbzip(testfilename_gdbzip) not crash", {
  testfilename_gdbzip    <- system.file("testdata/shapes/portland.gdb.zip", package = "EJAM")
  expect_no_error({suppressWarnings({
    junk <- capture.output({
      JUNK <- shapefile_from_gdbzip(testfilename_gdbzip)
    })
  })})
  expect_true(
    "sf" %in% class(JUNK)
  )
  rm(JUNK, junk)
})
######################################################### # 
testthat::test_that("shapefile_from_folder(testfilename_dirshp) not crash", {
  testfilename_dirshp    <- system.file("testdata/shapes/portland_folder_shp", package = "EJAM")
  expect_no_error({suppressWarnings({
    junk <- capture.output({
      JUNK <- shapefile_from_folder(testfilename_dirshp)
    })
  })})
  expect_true(
    "sf" %in% class(JUNK)
  )
  expect_equal(
    dim(JUNK),
    c(98,12)
  )
  rm(JUNK, junk)
})
######################################################### #
testthat::test_that("shapefile_from_filepaths(testfilenameset_4) not crash", {
  testfilename_shp_alone <- system.file("testdata/shapes/portland_folder_shp/Neighborhoods_regions.shp", package = "EJAM")
  testfilenameset_4 <- shapefile_filepaths_validize(testfilename_shp_alone)
  expect_no_error({suppressWarnings({
    junk <- capture.output({
      JUNK <- shapefile_from_filepaths(testfilenameset_4)
    })
  })})
  expect_true(
    "sf" %in% class(JUNK)
  )
  rm(JUNK)
})
######################################################### # 
testthat::test_that("shapefile_from_filepaths(testfilename_shp_alone) not crash", {
  testfilename_shp_alone <- system.file("testdata/shapes/portland_folder_shp/Neighborhoods_regions.shp", package = "EJAM")
  expect_no_error({suppressWarnings({
    junk <- capture.output({
      JUNK <- shapefile_from_filepaths(testfilename_shp_alone)
    })
  })})
  expect_true(
    "sf" %in% class(JUNK)
  )
  rm(JUNK, junk)
})
######################################################### #
testthat::test_that("shapefile_from_sitepoints(testpoints_10)  not crash", {
  expect_no_error({suppressWarnings({
    junk <- capture.output({
      JUNK <- shapefile_from_sitepoints(testpoints_10)
    })
    # testshape_points <- shapefile_from_sitepoints(testpoints_10) 
  })})
  expect_true(
    "sf" %in% class(JUNK)
  )
  rm(JUNK)
})
######################################################### #
testthat::test_that("shapefile_filepaths_from_folder(testfilename_dirshp) not crash", {
  testfilename_dirshp    <- system.file("testdata/shapes/portland_folder_shp",     package = "EJAM")
  expect_no_error({suppressWarnings({
    junk <- capture.output({
      JUNK <- shapefile_filepaths_from_folder(testfilename_dirshp)
    })
  })})
  expect_true({
    length(JUNK) > 1 & all(is.character(JUNK)) & all(file.exists(JUNK))
  })
  rm(JUNK)
})
testthat::test_that("shapefile_filepaths_from_folder() returns NULL and warns on empty folder", {
  emptyfolder <- file.path(tempdir(), "emptysubdir")
  if (!dir.exists(emptyfolder)) dir.create(emptyfolder)
  expect_no_error({
    junk <- capture.output({
      shapefile_filepaths_from_folder(emptyfolder)
    })
      })  # character(0) ??
  expect_equal(0, length(shapefile_filepaths_from_folder(emptyfolder))  )
  
  suppressWarnings(expect_warning({nullresults <- shapefile_from_folder(emptyfolder)}))
  suppressWarnings(expect_equal(NULL, nullresults))
})
######################################################### #  
testthat::test_that("shapefile_filepaths_valid(testfilenameset_4) not crash", {
  testfilename_shp_alone <- system.file("testdata/shapes/portland_folder_shp/Neighborhoods_regions.shp", package = "EJAM") # Neighborhoods_regions.shp
  testfilenameset_4 <- shapefile_filepaths_validize(testfilename_shp_alone)
  expect_no_error({suppressWarnings({
    junk <- capture.output({
    JUNK <- shapefile_filepaths_valid(testfilenameset_4)
    })
  })})
  expect_true({
    JUNK
  })
  rm(JUNK)
})
######################################################### #                                 
testthat::test_that("shapefile_filepaths_validize(testfilename_shp_alone) not crash", {
  testfilename_shp_alone <- system.file("testdata/shapes/portland_folder_shp/Neighborhoods_regions.shp", package = "EJAM") # Neighborhoods_regions.shp
  expect_no_error({suppressWarnings({
    JUNK <- shapefile_filepaths_validize(testfilename_shp_alone)
  })})
  expect_true(
    length(JUNK) > 1 & all(is.character(JUNK)) ### & all(file.exists(JUNK))
  )
  rm(JUNK)
})
######################################################### #
testthat::test_that("shapefile_clean(testshape) not crash", {
  testfilename_dirshp    <- system.file("testdata/shapes/portland_folder_shp", package = "EJAM")
  junk <- capture.output({
  testshape <- testshapes_2 # shapefile_from_folder(testfilename_dirshp)
  })
  expect_no_error({suppressWarnings({
    JUNK <- shapefile_clean(testshape)
  })})
  expect_true(
    "sf" %in% class(JUNK)
  )
  rm(JUNK)
})
######################################################### #                               
testthat::test_that("shape_buffered_from_shapefile(testshape) not crash", {
  testfilename_dirshp    <- system.file("testdata/shapes/portland_folder_shp", package = "EJAM")
  junk <- capture.output({
  testshape <- testshapes_2 # shapefile_from_folder(testfilename_dirshp)[c(1,3), ]
  })
  expect_no_error({suppressWarnings({
    JUNK <- shape_buffered_from_shapefile(testshape, radius.miles = 0.5)
  })})
  # mapview(testshape, col.regions = "darkgreen") + mapview(JUNK, col.regions = "lightgray")
  expect_true({
    "sf" %in% class(JUNK) & "sf" %in% class(testshape)
  })
  expect_true(
    all((as.numeric((sf::st_area(JUNK)) / (sf::st_area(testshape)))) > 1)
  )
  rm(JUNK)
})
######################################################### #                                 
testthat::test_that("shape_buffered_from_shapefile_points(testshape_points) not crash", {
  junk <- capture.output({
  testshape_points <- shapefile_from_sitepoints(testpoints_10[1:3, ])
  })
  expect_no_error({suppressWarnings({
    JUNK <- shape_buffered_from_shapefile_points(testshape_points, radius.miles = 1)
  })})
  # mapview::mapview(JUNK[1, ])
  # mapfast(testpoints_10[1, ], radius = 1)
  expect_true(
    "sf" %in% class(JUNK)
  )
  rm(JUNK)
})
######################################################### #
testthat::test_that("shapefile2latlon(testshape_points) aka latlon_from_shapefile(testshape) not crash", {
  junk <- capture.output({
  testshape_points <- shapefile_from_sitepoints(testpoints_10)
  })
  expect_no_error({suppressWarnings({
    junk <- capture.output({
    JUNK <- shapefile2latlon(testshape_points)
    JUNK <- latlon_from_shapefile(testshape_points)
    })
  })})
  expect_true(
    is.data.frame(JUNK) & data.table::is.data.table(JUNK)
  )
  rm(JUNK)
})
######################################################### # 
testthat::test_that("latlon_from_shapefile(testshape_points) not crash", {
  junk <- capture.output({
    testshape_points <- shapefile_from_sitepoints(testpoints_10)
  })
  expect_no_error({suppressWarnings({ suppressMessages({
    junk <- capture.output({
    JUNK <- latlon_from_shapefile(testshape_points)
    })
  })  })})
  expect_true(
    is.data.frame(JUNK) & data.table::is.data.table(JUNK)
  )
  rm(JUNK)
})
######################################################### # 

################################################################ #

# shapefile_from_any(   various  inputs  allowed   )

######################################################### # 
testthat::test_that("shapefile_from_any(testfilename_dirshp) works", {
  testfilename_dirshp    <- system.file("testdata/shapes/portland_folder_shp",     package = "EJAM")
  expect_no_error({junk <- capture.output({
    JUNK <- shapefile_from_any(testfilename_dirshp)
  })})
  expect_true("sf" %in% class(JUNK))
})
######################################################### # 
testthat::test_that("shapefile_from_any(testfilename_gdb) works", {
  testfilename_gdb       <- system.file("testdata/shapes/portland.gdb",            package = "EJAM")
  expect_no_error({junk <- capture.output({
    JUNK <- shapefile_from_any(testfilename_gdb)
  })})
  expect_true("sf" %in% class(JUNK))
})
################################################################ #
testthat::test_that("shapefile_from_any(testfilename_gdbzip) works", {
  testfilename_gdbzip    <- system.file("testdata/shapes/portland.gdb.zip",        package = "EJAM")
  expect_no_error({junk <- capture.output({
    JUNK <- shapefile_from_any(testfilename_gdbzip)
  })})
  expect_true("sf" %in% class(JUNK))
})
######################################################### # 
testthat::test_that("shapefile_from_any(testfilename_zipdir) works", {
  testfilename_zipdir    <- system.file("testdata/shapes/portland_folder_shp.zip", package = "EJAM")
  expect_no_error({junk <- capture.output({
    JUNK <- shapefile_from_any(testfilename_zipdir)
  })})
  expect_true("sf" %in% class(JUNK))
})
######################################################### # 
testthat::test_that("shapefile_from_any(testfilename_zipdir2) works", {
  testfilename_zipdir2   <- system.file("testdata/shapes/portland_shp.zip",        package = "EJAM") # .shp etc basenames are NOT same as  .zip file basename
  expect_no_error({junk <- capture.output({
    JUNK <- shapefile_from_any(testfilename_zipdir2)
  })})
  expect_true("sf" %in% class(JUNK))
})
######################################################### # 
testthat::test_that("shapefile_from_any(testfilename_zipshp) works", {
  testfilename_zipshp    <- system.file("testdata/shapes/stations.zip",            package = "EJAM") # .shp etc basenames ARE IDENTICAL TO .zip file basename
  expect_no_error({junk <- capture.output({
    JUNK <- shapefile_from_any(testfilename_zipshp)
  })})
  expect_true("sf" %in% class(JUNK))
})
######################################################### # 
testthat::test_that("shapefile_from_any(testfilename_json) works", {
  testfilename_json      <- system.file("testdata/shapes/portland.json",           package = "EJAM")
  expect_no_error({junk <- capture.output({
    JUNK <- shapefile_from_any(testfilename_json)
  })})
  expect_true("sf" %in% class(JUNK))
})
######################################################### # 
testthat::test_that("shapefile_from_any(testfilename_shp_alone) works", {
  testfilename_shp_alone <- system.file("testdata/shapes/portland_folder_shp/Neighborhoods_regions.shp", package = "EJAM") # Neighborhoods_regions.shp
  expect_no_error({junk <- capture.output({
    JUNK <- shapefile_from_any(testfilename_shp_alone)
  })})
  expect_true("sf" %in% class(JUNK))
})
######################################################### # 
testthat::test_that("shapefile_from_any(testfilenameset_4) works", {
  testfilename_shp_alone <- system.file("testdata/shapes/portland_folder_shp/Neighborhoods_regions.shp", package = "EJAM")
  testfilenameset_4 <- shapefile_filepaths_validize(testfilename_shp_alone)
  expect_no_error({junk <- capture.output({
    JUNK <- shapefile_from_any(testfilenameset_4)
  })})
  expect_true("sf" %in% class(JUNK))
})
######################################################### # 


