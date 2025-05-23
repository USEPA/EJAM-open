
test_that("ejam2barplot_sites works", {
  
  out <- copy(testoutput_ejamit_10pts_1miles)
  sites <- copy(out$results_bysite)
  topsites <- tail(sites[order(sites$pop), ], 3)

  expect_no_error({
    ejam2barplot_sites(out, "pop")
  })
  expect_no_error({
    ejam2barplot_sites(out, "pop", main = "TEST", xlab = "OTHER", ylab = "YLAB", topn = 17, sortby = FALSE, 
                       col = "red", cex.names = 0.5)
  })
  expect_no_error({
    x = ejam2barplot_sites(out, "pcthisp", sortby = -1 * out$results_bysite$ejam_uniq_id)
    x
  })
  expect_true('matrix' %in% class(x))
})
########################################################### #

test_that("plot_barplot_sites works", {
  
  out <- copy(testoutput_ejamit_10pts_1miles)
  sites <- copy(out$results_bysite)
  topsites <- tail(sites[order(sites$pop), ], 3)
  
  expect_no_error({
    x = plot_barplot_sites(topsites, "pop")
    x
  })
  expect_true('matrix' %in% class(x))
}) 
########################################################### #
########################################################### # 


test_that("plot_barplot_sitegroups works", {
  
  suppressWarnings({
    suppressMessages({
      junk <- capture_output({
        
        out <- ejamit_compare_types_of_places(
          testpoints_10[1:4, ],
          typeofsite = c("A", "B", "B", "C")
        )
        
      })
    })
  })
  
  expect_no_error({
    x = plot_barplot_sitegroups(out$results_bytype, names_d[1], topn = 3)
    x
    # ejam2barplot_sitegroups(out, "sitecount_unique", topn = 3, sortby = F)
    # ejam2barplot_sitegroups(out, "pop", topn = 3, sortby = F)
  })
  expect_true('matrix' %in% class(x))
})

########################################################### #

test_that("ejam2barplot_sitegroups works", {
  
  suppressWarnings({
    suppressMessages({
      junk <- capture_output({
        
        out <- ejamit_compare_types_of_places(
          testpoints_10[1:4, ],
          typeofsite = c("A", "B", "B", "C")
        )
        
      })
    })
  })
  
  expect_no_error( {
    ejam2barplot_sitegroups(out, names_these_ratio_to_avg[1], topn = 3)
    ejam2barplot_sitegroups(out, "sitecount_unique", topn = 3, sortby = F)
    x = ejam2barplot_sitegroups(out, "pop", topn = 3, sortby = F)
    x
  })
  expect_true('matrix' %in% class(x))
  
})
########################################################### #
