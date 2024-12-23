# # exported functions here:
# 
# ejamit_compare_distances() - to see indicators by distance, for sites as a whole (overall)
# ejamit_compare_distances2plot - to plot that
# ejamit_compare_distances_fulloutput - to get full set of ejamit() results at each distance

# # internal:
# 
# distance_trends - which indicator has strongest trend with distance?
# out_bydistance2results_bydistance - USED  BY ejamit_compare_distances() to see indicators by distance, for sites as a whole (overall)
# out_bydistance2results_bysite_bydistance - unused - to check one distance, all sites
# out_bydistance2results_bydistance_bysite - unused - to check one site, all distances
################################################################### #

test_that(
  "ejamit_compare_distances_fulloutput() -slow", 
  {
    
    junk <- capture_output({
      expect_no_error({
        suppressWarnings({
          
          fout = ejamit_compare_distances_fulloutput(
            sitepoints = testpoints_10[1:3, ],
            radii = c(1, 10),
            quiet = F, silentinteractive = F
          )
          
        })
      })
    })
    
    expect_equal(
      length(fout), 2  # list length is 1 per radius, not 1 per site
    )
    expect_equal( # each distance provides results format exactly like ejamit() provides
      names(fout[[1]]),
      names(testoutput_ejamit_10pts_1miles)
    )
    
    expect_equal(   # 3 sites in a given results_bysite table
      NROW(fout[[1]]$results_bysite),
      3
    )
    expect_equal(   # 3 sites in a given results_bysite table
      NROW(fout[[2]]$results_bysite),
      3
    )
    expect_equal(  # 3 sites used same radius in given results_bysite table
      fout[[2]]$results_bysite$radius.miles,
      c(10, 10, 10)
    )
    expect_equal(  # 3 sites have population counts exactly like ejamit() would, for distance #1
      fout[[1]]$results_bysite$pop,
      testoutput_ejamit_10pts_1miles$results_bysite$pop[1:3]
    )
    
  })
######################################## # 

test_that(
  "ejamit_compare_distances() works", 
  {
    junk <- capture_output({
      
      expect_no_error({
        suppressWarnings({
          suppressMessages({
            
            out <- ejamit_compare_distances(
              sitepoints = testpoints_10[1:3, ],
              radii = 1:2, 
              # donuts_not_cumulative = FALSE, 
              # quiet = TRUE, silentinteractive = TRUE, 
              plot = FALSE,
              # # myvars = names_d_subgroups_ratio_to_state_avg,
              # ylab = "Ratio to avg in state or nation", 
              n = 1,
              include_ejindexes = F,
              calculate_ratios = T
            )
            
          })
        })
      })
    })
    
    expect_equal(
      NROW(out), 2
    )
    expect_equal(
      out$radius.miles, 1:2
    )
    expect_true(
      out$pop[2] > out$pop[1]
    )
    suppressMessages({
      out2 = ejamit(testpoints_10[1:3,], radius = 2, silentinteractive = TRUE)
    })
    expect_equal(
      out2$results_overall$pop, out$pop[2]
    )
  })
######################################## #


######################################## #

test_that("donuts ok in ejamit(radius_donut_lower_edge=3)", {
  
  junk <- capture_output({
    
    expect_no_error({
      suppressWarnings({
        suppressMessages({
          out3    <- ejamit(testpoints_10, radius = 3, silentinteractive = TRUE)   # 0 to 3 miles
          out10   <- ejamit(testpoints_10, radius = 10, silentinteractive = TRUE)  # 0 to 10 miles
          outring <- ejamit(testpoints_10, radius = 10, silentinteractive = TRUE, 
                            radius_donut_lower_edge = 3) # 3 to 10 miles
        })
      })
    })
    expect_true(
      round(outring$results_overall$pop, 0) == round(out10$results_overall$pop - out3$results_overall$pop, 0)
    )
    expect_equal(
      outring$results_bysite$pop,
      out10$results_bysite$pop - out3$results_bysite$pop,
      tolerance = 2  # pop count same to within 2 residents since rounding issues etc.
    )
  })
})
######################################## # 

test_that("donuts ok in ejamit_compare_distances(donuts_not_cumulative = T)", {
  
  junk <- capture_output({
    junk <- capture_output({
      expect_no_error({
        suppressWarnings({
          suppressMessages({
            
            out <- ejamit_compare_distances(
              sitepoints = testpoints_10[5:6, ],
              radii = c(3, 10), 
              donuts_not_cumulative          = FALSE, 
              plot = FALSE,
              n = 1,
              include_ejindexes = F,
              calculate_ratios = FALSE
            )
            
            outring <- ejamit_compare_distances(
              sitepoints = testpoints_10[5:6, ],
              radii = c(3, 10), 
              donuts_not_cumulative         = TRUE, 
              plot = FALSE,
              n = 1,
              include_ejindexes = F,
              calculate_ratios = FALSE
            )
            
            # outring[, .( pop, blockcount_near_site, radius.miles, distance_min, distance_min_avgperson)]
            # out[, .( pop, blockcount_near_site, radius.miles, distance_min, distance_min_avgperson)]
          })
        })
      })
      expect_true(   # results are same for the first radius either way
        out$blockcount_near_site[1] == outring$blockcount_near_site[1]
      )
      expect_equal(     
        out$blockcount_near_site[2],
        sum(outring$blockcount_near_site) # normal results are same as cumulative sums of rings
      )
      expect_equal(
        out$pop,
        cumsum(outring$pop), # normal results are same as cumulative sums of rings
        tolerance = 2  # pop count same to within 2 residents since rounding issues etc.
      )
    })
  })
})
################################################################### #


test_that(
  "ejamit_compare_distances2plot() works", 
  {
    junk <- capture_output({
      
      expect_no_error({
        suppressWarnings({
          suppressMessages({
            out <- ejamit_compare_distances(
              sitepoints = testpoints_10[1:2, ],
              radii = 1:2, 
              # donuts_not_cumulative = FALSE, 
              # quiet = TRUE, silentinteractive = TRUE, 
              plot = TRUE,
              # # myvars = names_d_subgroups_ratio_to_state_avg,
              # ylab = "Ratio to avg in state or nation", 
              n = 1,
              include_ejindexes = F,
              calculate_ratios = T
            )
            
          })
        })
      })
      
    })
    
    
    expect_no_error({
      suppressWarnings({
        x <- ejamit_compare_distances2plot(out, 
                                           myvars = names_d[1:4],
                                           ylab = "test", 
                                           ylim = c(0, 1)
        )
      })
    })
    expect_equal(
      class(x), "character"
    )
    
  })
######################################## #


test_that(
  "distance_trends() works", 
  {
    junk <- capture_output({
      
      suppressWarnings({
        rbd = ejamit_compare_distances(
          sitepoints = testpoints_10[1:2, ],
          radii = 1:2)
        
        expect_no_error({
          
          x = distance_trends(rbd)
          
        })
      })
      
    })
    expect_true(
      x %in% fixcolnames(names(rbd), "r", "long")
    )
    
  })
######################################## #

test_that(
  "out_bydistance2results_bydistance(), out_bydistance2results_bysite_bydistance() no error", 
  {
    # expect_no_error({
    suppressWarnings({
      junk <- capture_output({
        
        # takes maybe 5 seconds to re create this each time
        fout = ejamit_compare_distances_fulloutput(
          sitepoints = testpoints_10[1:2, ],
          radii = c(1,10),
          donuts_not_cumulative = F,
          quiet = F, silentinteractive = F
        )
        
      })
    })
    
    expect_no_error({
      suppressWarnings({
        x = out_bydistance2results_bydistance(fout)
      })
    })
    expect_equal(
      NROW(x), 2
    )
    ########################## #
    # out_bydistance2results_bysite_bydistance() no crash
    
    expect_no_error({
      suppressWarnings({
        x = out_bydistance2results_bysite_bydistance(fout)
      })
    })
    ########################## #
    #  out_bydistance2results_bydistance_bysite  ok
    expect_no_error({
      x = out_bydistance2results_bydistance_bysite(fout)
    })
    ########################## #
  })
######################################## #

test_that(
  "out_bydistance2results_bydistance() etc get same numbers as before", 
  {
    suppressWarnings({
      junk <- capture_output({
        
        # takes maybe 5 seconds to re create this each time
        fout = ejamit_compare_distances_fulloutput(
          sitepoints = testpoints_10[1:2, ],
          radii = c(1,10),
          donuts_not_cumulative = F,
          quiet = F, silentinteractive = F
        )
        
      })
    })
    
    suppressWarnings({
      x = out_bydistance2results_bydistance(fout)
    })
    
    ######## out_bydistance2results_bydistance() outputs are still formatted as before
    
    expect_identical(
      names(x), 
      names(testoutput_ejamit_10pts_1miles$results_bysite)
    )
    ########################## #
    # out_bydistance2results_bysite_bydistance() give same results as before
 
      suppressWarnings({
        x = out_bydistance2results_bysite_bydistance(fout)
      }) 
    expect_equal(
      x[[1]],
      testoutput_ejamit_10pts_1miles$results_bysite[1:2,], ignore_attr = T      ########  outputs are same numbers as before
    )
    ########################## #
    #  out_bydistance2results_bydistance_bysite give same results as before

    x = out_bydistance2results_bydistance_bysite(fout)
    expect_identical(
      x[[2]]$ejam_uniq_id,
      as.integer(c(2, 2)) # site number 2 is in this table once per distance
    )
    
    
    # expect_equal(
    #   x[[1]][1, ], # 1st distance,  1st site
    #   testoutput_ejamit_10pts_1miles$results_bysite[1, ]
    # )
    ########################## #
  })
