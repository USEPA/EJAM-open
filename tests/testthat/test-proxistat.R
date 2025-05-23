
## use a few random sites in one state
##
## ST = state_from_sitetable(testpoints_1000)
## delaware_testpoints = testpoints_1000[(ST$ST == "DE"), ]
## dput(delaware_testpoints)
delaware_testpoints = structure(list(
  lat = c(39.7397, 38.56097, 39.061297, 39.83495, 39.159),
  lon = c(-75.72528, -75.2003, -75.397816, -75.6045, -75.52477
  ),
  sitenumber = c(49L, 174L, 626L, 646L, 824L),
  sitename = c("Example Site 49", "Example Site 174", "Example Site 626", "Example Site 646", "Example Site 824")),
  date_saved_in_package = "2025-02-14",
  row.names = c(49L, 174L, 626L, 646L, 824L), class = "data.frame")
delaware_testpoints$sitenumber = 1:nrow(delaware_testpoints)
delaware_testpoints$sitename = NULL

## use all 20,198 blocks in the state?
##
bst = state_from_blockid(blockpoints$blockid)
delaware_blocks = blockpoints[which(bst == 'DE'), ]
# or use just the ones in one county:
delaware_1county_bgfips = fips_bgs_in_fips(fips_counties_from_state_abbrev('DE')[1])
delaware_1county_bgids = blockgroupstats$bgid[match(delaware_1county_bgfips, blockgroupstats$bgfips)]
bgids_statewide = bgid_from_blockid(delaware_blocks$blockid)
de1county_blocks = delaware_blocks[bgids_statewide %in% delaware_1county_bgids, ]
# > dim(de1county_blocks)
# [1] 4133    3
### GET SCORES FOR JUST 1 COUNTY'S BLOCKS AS TEST
# proxistat(topoints = delaware_testpoints,
#           bpoints = de1county_blocks)

testthat::test_that("proxistat works at all", {


  skip(" proxistat() NOT YET WORKING AND CAN CRASH R SO WILL NOT TEST YET")
  expect_no_error({
    proxistat(topoints = delaware_testpoints,
              bpoints = de1county_blocks)


  })

})

testthat::test_that("proxistat_via_getblocks() works at all", {
  skip("skipping proxistat_via_getblocks testing until ready")
  expect_no_error({
    proxistat_via_getblocks(pts = testpoints_10[1:2, ])
  })

})
