# popup_from_ejscreen()
# popup_from_any()
# popup_from_df() - will deprecate once confirm popup_from_any() works in its place
# popup_from_uploadedpoints()

# mapfast()
# mapfastej()

# ejam2map()
# map2browser()

# map_facilities()
# map_facilities_proxy()
# mapfastej_counties()
# map_blockgroups_over_blocks()
# map_shapes_plot()
# map_shapes_leaflet_proxy()
# map_shapes_mapview  # If mapview pkg available
# shapes_counties_from_countyfips() #Get Counties boundaries via API, to map them
# shapes_blockgroups_from_bgfips()
# mapfast_gg()
############################################## # 

test_that("popup_from_ejscreen() works even if 1 row or 1 indicator", {
  expect_no_error({
    suppressWarnings({
      x = popup_from_ejscreen(testoutput_ejscreenapi_plus_5)
      
      x = popup_from_ejscreen(testoutput_ejamit_10pts_1miles$results_bysite[1:2,])
      
      # only one place (one row)
      x = popup_from_ejscreen(testoutput_ejamit_10pts_1miles$results_bysite[1,])
    })
  })
  expect_no_error({
    suppressWarnings({
      # what if only some indicators available??
      x = popup_from_ejscreen(testoutput_ejamit_10pts_1miles$results_bysite[,  1:20])
      # what if try to use for other table than supposed to
      x = popup_from_ejscreen(testpoints_10[1:2,]) 
    })
  })
})
############################################## # 

test_that("popup_from_any() works even if 1 row or 1 indicator", {
  expect_no_error({
    suppressMessages({
      
      x1 = popup_from_any(testpoints_10[1:2,])
      
      x2 = popup_from_any(testoutput_ejamit_10pts_1miles$results_bysite[1:2,], column_names = names_d, labels = fixcolnames(names_d, "r", "short"))
      x3 = popup_from_any(testoutput_ejamit_10pts_1miles$results_bysite[1:2,],  n = 7) # only uses the first 7 columns so NA reported for all others which seems not ideal
      length(x3)
      
      # only one place (one row)
      x4 = popup_from_any(testpoints_10[1,])
      
      # only one indicator
      x5 = popup_from_any(testoutput_ejamit_10pts_1miles$results_bysite[1:2,],  column_names = "pop")
      x5b = popup_from_any(testoutput_ejamit_10pts_1miles$results_bysite[1,],  n = 1) #  # one row, one indicator
      x5c = popup_from_any(testoutput_ejamit_10pts_1miles$results_bysite[1,],  column_names = "pctlowinc") #  # one row, one indicator 
      
      # if data.table format
      x6 = popup_from_any(data.table(testpoints_10[1:2,]))
      x7 = popup_from_any(data.table(testpoints_10[1,]))
      x8 = popup_from_any(data.table(testoutput_ejamit_10pts_1miles$results_bysite)[1:2, ],  column_names = "pop")
      x9 = popup_from_any(data.table(testoutput_ejamit_10pts_1miles$results_bysite)[1, ],  column_names = "pop") # one row, one indicator
      
    })
  })
  
  suppressWarnings({
    expect_warning({
      x0 = popup_from_any(testpoints_10,  column_names = "pop is not a column in that dataset")
    })
  })
})
############################################## # 
if (exists("popup_from_df")) { # will likely deprecate
  test_that("popup_from_df() works but popup_from_any() may replace it", {
    expect_no_error({
      suppressMessages({
        popup_from_df(testpoints_10[1:2,])
        popup_from_df(testoutput_ejamit_10pts_1miles$results_bysite[1:2,],  n = 3)
        popup_from_df(testoutput_ejamit_10pts_1miles$results_bysite[1:2,], column_names = names_d, labels = fixcolnames(names_d, "r", "short"))
        # not testing 1 row or 1 indicator cases
      })
    })
  })
}
############################################## # 

test_that("popup_from_uploadedpoints() works", {
  expect_no_error({
    suppressMessages({
      popup_from_uploadedpoints(testpoints_10[1:2,])
      # just one location
      popup_from_uploadedpoints(testpoints_10[1,])
    })
  })
})
############################################## # 



############################################## # 
############################################## # 
test_that("mapfast() works", {
  expect_no_error({
    suppressMessages({
      mapfast(testpoints_10)
      mapfast(testoutput_ejamit_10pts_1miles$results_bysite, radius = 0.2, column_names = names_d, browse = FALSE)
      mapfast(testoutput_ejamit_10pts_1miles$results_bysite, radius = 0.2, column_names = names_d, labels = fixcolnames(names_d, "r", "short"))
      # but note 0-1 not 0-100 shown for demog percentages this way
    })
  })
})
test_that("mapfast warns if forget specify table", {
  suppressWarnings({
    expect_warning({
      mapfast(testoutput_ejamit_10pts_1miles) # if forgot to specify table $results_bysite
    })
  })
})

test_that("mapfast should handle just 1 indicator!", {
  errmsgjunk = capture.output(
    expect_no_error(
      mapfast(testoutput_ejamit_10pts_1miles$results_bysite, radius = 0.2, column_names = "Demog.Index", labels = "Demographic Score", browse = FALSE)
    )
  )
})
############################################## # 

test_that("mapfastej() works", {
  expect_no_error({
    suppressWarnings({
      mapfastej(testoutput_ejamit_10pts_1miles$results_bysite)
      mapfastej(testoutput_ejamit_10pts_1miles$results_bysite, radius = 3)
    })
  })
})
############################################## # 
############################################## # 

test_that("ejam2map() works", {
  expect_no_error({
    suppressWarnings({
      x = ejam2map(testoutput_ejamit_10pts_1miles)
    })
  })
  expect_true("leaflet" %in% class(x))
})
############################################## # 

test_that("map2browser() works", {
  testthat::skip_if_not(interactive())
  expect_no_error({
    suppressWarnings(
    capture.output(
      x = map2browser(ejam2map(testoutput_ejamit_10pts_1miles))
    )
    )
    # file.exists(x) # tricky to test for that
  })
})
############################################## # 
############################################## # 

test_that("map_facilities() works", {
  expect_no_error({
    suppressMessages({
      map_facilities(mypoints = testpoints_10[1,], )
      map_facilities(mypoints = testpoints_10[1:2,], rad = 1)
      map_facilities(mypoints = testpoints_10[1:3,], rad = 1, highlight = TRUE, clustered = c(TRUE,TRUE,FALSE))
    })
  })
  expect_error({
    map_facilities(mypoints = testpoints_10[1:2,], rad = 1, 
                   highlight = TRUE) # requires clustered param if highlight=T
  })
})
test_that("map_facilities() works with non-data.table data.frame", {
  expect_no_error({
    suppressMessages({
      map_facilities(mypoints = data.frame(testpoints_10[1,]))
      map_facilities(mypoints = data.frame(testpoints_10[1:2,]), rad = 1)
    })
  })
})
############################################## # 
test_that("map_facilities_proxy() works", {
  expect_no_error({
    suppressMessages({
      map_facilities_proxy(
        mapfast(testpoints_10[1,]), # only 1 point
        rad = 4, 
        popup_vec = popup_from_any(data.frame(
          newinfo = "text", 
          other = 1
        ))
      ) 
    })
  })
  expect_no_error({
    suppressMessages({
      map_facilities_proxy(
        mapfast(testpoints_10[1:2,]), 
        rad = 4, 
        popup_vec = popup_from_any(data.frame(
          newinfo = c("xyz", "zzz"), 
          other = 1:2
        ))
      ) 
    })
  })
})

############################################## # 

test_that("mapfastej_counties() works", {
  
  # getblocksnearby_from_fips() has warnings here 
  suppressMessages({
    
    expect_no_error({
      capture.output({
        mydat = ejamit(fips = fips_counties_from_statename("Rhode Island")[1], radius = 0, silentinteractive = TRUE)$results_bysite
        suppressWarnings({
        x = mapfastej_counties(mydat)
        })
      })
    })
    expect_true("leaflet" %in% class(x))
  })
})
############################################## # 
test_that("map_blockgroups_over_blocks() works", {
  expect_no_error({
    capture.output({
    y <- plotblocksnearby(testpoints_10[5,],
                          radius = 0.5,
                          returnmap = TRUE)
    z = map_blockgroups_over_blocks(y)
    })
  })
  expect_true(
    "leaflet" %in% class(z)
  )
})
############################################## # 
test_that("shapes_counties_from_countyfips() works", {
  # Get Counties boundaries via API, to map them
  expect_no_error({
    myshapes = shapes_counties_from_countyfips(fips_counties_from_state_abbrev("DE")[1])
  })
})
############################################## # ############################################## # 

myshapes = shapes_counties_from_countyfips(fips_counties_from_state_abbrev("DE")[1])  # kind of slow so just done once here for tests

############################################## # 
test_that("map_shapes_plot() works", {
  
  # myshapes = shapes_counties_from_countyfips(fips_counties_from_state_abbrev("DE")[1])
  
  expect_no_error({
    map_shapes_plot(myshapes)   
  })
})

# test_that("map_shapes_leaflet_proxy() works", {
# 
#   myshapes = shapes_counties_from_countyfips(fips_counties_from_state_abbrev("DE")[1])
# 
#   expect_no_error({
#     map_shapes_leaflet_proxy(     )
#   })
# })
############################################## # 
test_that("map_shapes_mapview() if mapview pkg available works", {
  
  # myshapes = shapes_counties_from_countyfips(fips_counties_from_state_abbrev("DE")[1])
  skip_if_not_installed("mapview")
  # requires mapview pkg be attached by setup.R in tests folders
  expect_no_error({
    # warns now but no error if package mapview not installed or not attached
    map_shapes_mapview(myshapes)
  })
})
############################################## # 
test_that("shapes_blockgroups_from_bgfips() works", {
  expect_no_error({
    x = shapes_blockgroups_from_bgfips()
  })
  expect_true("sf" %in% class(x))
})
############################################## # 
test_that("mapfast_gg() works", {
  expect_no_error({
    mapfast_gg(testpoints_10)
  })
})
############################################## # ############################################## # 
