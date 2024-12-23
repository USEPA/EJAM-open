test_that("getblocks_summarize_blocks_per_site does not crash", {
  expect_no_error({
    junk <- capture_output({
      
      getblocks_summarize_blocks_per_site(testoutput_getblocksnearby_1000pts_1miles)
      getblocks_summarize_blocks_per_site(testoutput_getblocksnearby_1000pts_1miles, "blockid" )
      
    })
  })
})

test_that("getblocks_summarize_sites_per_block does not crash", {
  skip_if_not(exists("getblocks_summarize_sites_per_block"), "getblocks_summarize_sites_per_block() is not exported and not loaded")
  expect_no_error({
    # EJAM  ... ::: getblocks_summarize_sites_per_block(testoutput_getblocksnearby_1000pts_1miles) # would use the installed version not a sourced version
    z <- getblocks_summarize_sites_per_block(testoutput_getblocksnearby_1000pts_1miles)
    getblocks_summarize_sites_per_block(testoutput_getblocksnearby_1000pts_1miles, "ejam_uniq_id")
  })
  expect_true({ "table" %in% class(z)})
  expect_error({
    getblocks_summarize_sites_per_block(testoutput_getblocksnearby_1000pts_1miles, "invalidcolname")
  })
})

test_that("getblocks_diagnostics does not crash", {
  expect_no_error({
    junk <- capture.output({ 
      
      getblocks_diagnostics(testoutput_getblocksnearby_1000pts_1miles, detailed = TRUE, see_pctiles = TRUE)
      getblocks_diagnostics(testoutput_getblocksnearby_1000pts_1miles, detailed = TRUE, see_pctiles = F) 
      getblocks_diagnostics(testoutput_getblocksnearby_1000pts_1miles, detailed = F,    see_pctiles = TRUE) 
      getblocks_diagnostics(testoutput_getblocksnearby_1000pts_1miles, detailed = F,    see_pctiles = F) 
    })
  })
})

