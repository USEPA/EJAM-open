

# could add tests here to check
# validstats, 
# ratiostats,
# results_bytype$valid, 
# results_bytype$sitecount,
# results_overall$valid, 
# results_overall$sitecount, 
# etc. ***



################################################################# #
test_that("ejamit_compare_types_of_places works", {
  
  expect_no_error({
    
    # slow
    suppressWarnings({
      suppressMessages({
        cat("running ejamit_compare_types_of_places() to test it...\n")
        
        junk <- capture_output({
          
          out <- ejamit_compare_types_of_places(
            testpoints_10[1:3, ], radius = 1,
            typeofsite = c("A", "B", "B")
          )
          
        })
      })
    })
    
  })
  
  expect_equal(
    names(out),
    c("types", "sitecount_bytype", "results_bytype", "results_overall", 
      "ejam_uniq_id", "typeofsite", "results_bysite", "longnames",
      'validstats', 'ratiostats')
  )
  expect_equal(
    out$types,
    c("A", "B")
  )
  expect_identical(
    out$sitecount_bytype,
    1:2
  )
  # confirms fixed state assignments for ejamit_compare_types_of_places()
  expect_equal(out$results_bysite$statename, c("Georgia",  "Alabama",  "Illinois"))
})
############################################################ #
test_that("works if only 1 point", {
  pts1 <- data.frame(testpoints_10[1,])
  
  expect_no_error({
  junk <- capture_output({
    out <- ejamit_compare_types_of_places(pts1, typeofsite = 'A')   
  })
  })
  
  
})
############################################################ #
test_that("ejamit_compare_types_of_places error if type of place not provided", {
  expect_error({ # need to provide type for each place
    junk <- capture_output({
      ejamit_compare_types_of_places(testpoints_10)
    })
  })
})
############################################################ #






############################################################ #
