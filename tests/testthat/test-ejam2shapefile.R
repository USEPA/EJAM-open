
cat("\n") # because can't suppress some info being printed to console.

# test cases/ examples
########################################################################## # 
# IF FILE PROVIDED, DONT ASK USER TO CONFIRM
# IF FOLDER PROVIDED, DONT ASK USER TO CONFIRM
# IF FILE NOT PROVIDED, DOES NOT ASK TO CONFIRM THE DEFAULT FILENAME IS OK 
# IF FOLDER NOT PROVIDED... IT ASKS TO CONFIRM THE DEFAULT FOLDER IS OK IF INTERACTIVE, SO SKIP THAT case IN THESE TESTS.

########################################################################## # 

# no prompt if folder is specified, or if save=F    



test_that("ejam2shapefile ok if save=F", {
  expect_no_error( 
    expect_warning( # some specified varnames not found
      # save FALSE
      suppressMessages({
        junk <- testthat::capture_output({
          
          # make the test data smaller to try to speed it up
          tout = list(results_bysite = testoutput_ejamit_10pts_1miles$results_bysite[1:2, ])
          shp <- ejam2shapefile(tout, save = FALSE)
          # shp <- ejam2shapefile(testoutput_ejamit_10pts_1miles, save = FALSE)
        })
      })
    )
    # map_shapes_leaflet(shp)
  )
  expect_true("sf" %in% class(shp))
  # expect_equal(NROW(shp), 10)
  expect_equal(NROW(shp), 2)
})
################################# # 

## note: cannot find a way to suppress the text output about 4 files being created - sink and capture output and suppressMessages dont help

test_that("ejam2shapefile ok if folder=tempdir()", {
  # provide folder
  
  expect_no_error({
    suppressWarnings({
      suppressMessages({
        junk = capture_output({
   
          # make the test data smaller to try to speed it up
          tout = list(results_bysite = testoutput_ejamit_10pts_1miles$results_bysite[1:2, ])
          x = ejam2shapefile(tout, folder = tempdir())
          # x = ejam2shapefile(testoutput_ejamit_10pts_1miles, folder = tempdir())

          # zip::zip_list(x)  # not required by EJAM pkg
          # browseURL(dirname(x))
          # dir(dirname(x), pattern = "zip")
        })
        
        junk = capture_output({
          
          shp <- shapefile_from_any(x)
          # shp[1:3,4:8] 
          
        })
      })
    })
  })
  expect_true(file.exists(x))
  expect_true("sf" %in% class(shp))
  # expect_equal(NROW(shp), 10)
  expect_equal(NROW(shp), 2)
})
################################# # 

## note: cannot find a way to suppress the text output about 4 files being created - sink and capture output and suppressMessages dont help

test_that("ejam2shapefile ok if folder and fname both specified", {
  expect_no_error({
    suppressWarnings({
      suppressMessages({
        junk = capture_output({
          
          # make the test data smaller to try to speed it up
          tout = list(results_bysite = testoutput_ejamit_10pts_1miles$results_bysite[1:2, ])
          
          # both
          x = ejam2shapefile(tout, fname = 'test.shp', folder = tempdir())
          # x = ejam2shapefile(testoutput_ejamit_10pts_1miles, fname = 'test.shp', folder = tempdir())
          # zip::zip_list(x) # not required by EJAM pkg
          shp <- shapefile_from_any(x)
          
        })
      })
    })
  })
  expect_true(file.exists(x))
  expect_true("sf" %in% class(shp))
  # expect_equal(NROW(shp), 10)
  expect_equal(NROW(shp), 2)
})

########################################################################## # 
 

# *** if interactive it normally tries to prompt for shapefile folder in some cases  ####

## not really using these tests of interactive mode so just comment them out.


# if (interactive() & !exists("noquestions")) {
#   if ( askYesNo("run tests where you have to interactively specify a folder for shapefiles?")) {
#     noquestions = FALSE
#   }  else {
#     noquestions <- TRUE
#   }
# } else {
#   noquestions = TRUE
# }

# noquestions = TRUE

# 
# test_that("ejam2shapefile ok if save=T", {
#   if (!exists('noquestions')) {noquestions <- TRUE}
#   testthat::skip_if(noquestions) 
#   
#   expect_no_error({
#     suppressWarnings({
#       suppressMessages({
#         junk = capture_output({
#           
#           # save TRUE - note if interactive it tries to prompt for folder
#           x <- ejam2shapefile(testoutput_ejamit_10pts_1miles, save = TRUE)
#           shp = shapefile_from_any(x)
#           # map_shapes_leaflet(shp)
#           
#         })
#       })
#     })
#   })
#   expect_true(file.exists(x))
#   expect_true("sf" %in% class(shp))
#   expect_equal(NROW(shp), 10)
# })
# ################################# # 
# 
# test_that("ejam2shapefile ok if use defaults", {
#   if (!exists('noquestions')) {noquestions <- TRUE}
#   testthat::skip_if(noquestions) 
#   
#   expect_no_error({
#     suppressWarnings({
#       suppressMessages({
#         junk = capture_output({
#           
#           # defaults - note if interactive it tries to prompt for folder
#           x = ejam2shapefile(testoutput_ejamit_10pts_1miles)
#           # zip::zip_list(x) # not required by EJAM pkg
#           shp <- shapefile_from_any(x)
#         })
#       })
#     })
#   })
#   expect_true(file.exists(x))
#   expect_true("sf" %in% class(shp))
#   expect_equal(NROW(shp), 10)
# })
# ################################# # 
# 
# test_that("ejam2shapefile ok if use defaults + fname", {
#   if (!exists('noquestions')) {noquestions <- TRUE}
#   testthat::skip_if(noquestions) 
#   
#   expect_no_error({
#     suppressWarnings({
#       suppressMessages({
#         junk = capture_output({
#           
#           # provide fname - note if interactive it tries to prompt for folder
#           x = ejam2shapefile(testoutput_ejamit_10pts_1miles, fname = "test.shp")
#           # zip::zip_list(x) # not required by EJAM pkg
#           shp <- shapefile_from_any(x)
#           
#         })
#       })
#     })
#   })
#   expect_true(file.exists(x))
#   expect_true("sf" %in% class(shp))
#   expect_equal(NROW(shp), 10)
# })
# ########################################################################## # 
   
