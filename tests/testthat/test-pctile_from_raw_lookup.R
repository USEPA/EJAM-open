## unit tests for EJAM::pctile_from_raw_lookup
## Author: Sara Sokolinski


# use default usastats table and test with random column
test_that('default lookup works',{
  expect_no_warning({
    val <- pctile_from_raw_lookup(
      myvector = c(20, 25, 30), 
      varname.in.lookup.table = "pm"
    )
  })
  # expect_equal(val, c(34,59,83)) # changed
})

# use custom lookup table
# it has to have REGION = USA
test_that('custom lookup works',{
  tab <- data.frame("PCTILE" = seq(0,100,1), "traffic.score" = seq(10,210, 2), "REGION" = "USA")
  expect_no_warning({
    val <- pctile_from_raw_lookup(
      myvector = c(20, 25, 30), 
      varname.in.lookup.table = "traffic.score",
      lookup = tab
    )
  })
  expect_equal(val, c(5,7,10))
})

# lets test zone = NY
test_that('pass states table as lookup and select a zone',{
  expect_no_warning({val <- pctile_from_raw_lookup(myvector = c(20, 25, 30), 
                                                   varname.in.lookup.table = "traffic.score",
                                                   lookup = statestats,
                                                   zone = "NY")})
  # expect_equal(val, c(43,55,67))
})

# use custom lookup table
# expect error if missing PCTILE column
test_that('custom lookup works',{
  tab <- data.frame("PerCenTILE" = seq(0,100,1), "traffic.score" = seq(10,210, 2), "REGION" = "USA")
  expect_error({val <- pctile_from_raw_lookup(myvector = c(20, 25, 30), 
                                              varname.in.lookup.table = "traffic.score",
                                              lookup = tab)})
})

#   If the value is between the cutpoints listed as
#   percentiles 89 and 90, it returns 89, for example.
#
test_that('rounds down',{
  expect_no_warning({
    val <- pctile_from_raw_lookup(myvector = c(80, 80.25, 80.5, 80.75), 
                                  varname.in.lookup.table = "traffic.score", 
                                  lookup = data.frame(
                                    PCTILE = 0:100, 
                                    traffic.score = 0:100, 
                                    REGION = "USA"))
  })
  expect_equal(val, c(80,80,80,80))
})

#   If the value is exactly equal to the cutpoint listed as percentile 90,
#   it returns percentile 90.
#   
#   This works when passed the vector but not the exact value?
test_that('equal to cutpoint rounds up',{
  expect_no_warning({
    val <- pctile_from_raw_lookup(myvector = c(80, 80.25, 85, 90), 
                                  varname.in.lookup.table = "traffic.score", 
                                  lookup = data.frame(
                                    PCTILE = 0:100, 
                                    traffic.score = 0:100, 
                                    REGION = "USA"))
 } )
  expect_equal(val, c(80, 80, 85, 90))
})

#   If the value is exactly the same as the minimum in the lookup table and multiple percentiles 
#   in that lookup are listed as tied for having the same threshold value defining the percentile
#    (i.e., a large % of places have the same score and it is the minimum score), 
#    then the percentile gets reported as 0, not the percent of places tied for that minimum score.

test_that('multiple zero minimums return zero',{
  expect_no_warning({
    
    mylookup = data.frame(PCTILE = 0:100, 
                              pctlingiso = c(rep(0,57), rep(0.02,5),  (0.06 + (1:36)/100), 
                                             0.45, 0.58, 1), 
                              REGION = "USA")
    # max(as.numeric((mylookup$PCTILE[mylookup$pctlingiso == 0])))
    # [1] 56
    
    val <- pctile_from_raw_lookup(myvector = c(0, 0.01, 0.05, 0.31, 0.45), 
                                  varname.in.lookup.table = "pctlingiso",
                                  lookup = mylookup 
                                  )
  })
  expect_equal(val, c(  0, 56, 61 ,86, 98))
})
 
# #   If the value is less than the cutpoint listed as percentile 0,
# #   which should be the minimum value in the dataset,
# #   it still returns 0 as the percentile, but with a warning that
# #   the value checked was less than the minimum in the dataset.

test_that('below min returns zero with warning??',{
  
  expect_warning({
    tab <- data.frame("PCTILE" = seq(0,100,1), "traffic.score" = c(1,1,1,10, 10, seq(15,206, 2)), "REGION" = "USA")
    val <- pctile_from_raw_lookup(myvector = c(0, 10, 11, 15),
                                               varname.in.lookup.table = "traffic.score",
                                               lookup = tab)
    })
  
  expect_equal(val, c(0, 4, 4, 5))
})



test_that('order does not affect pctiles',{
  
     bysite <-  testoutput_ejamit_10pts_1miles$results_bysite
     bysite <- bysite[c(2:10, 1),]
    val <- pctile_from_raw_lookup(myvector = bysite$pctnhaiana,
                                  varname.in.lookup.table = "pctnhaiana",
                                  lookup = statestats, zone = bysite$ST)
  
  expect_equal(val, testoutput_ejamit_10pts_1miles$results_bysite$state.pctile.pctnhaiana[c(2:10, 1)])
})
