
# ## All states, full table
# # newvars <- acs_bybg(table = "B01001")


# ## Format new data to match rows of blockgroupstats
#
# setnames(newvars, "GEOID", "bgfips")
# dim(newvars)
# newvars <- newvars[blockgroupstats[,.(bgfips, ST)], ,  on = "bgfips"]
# dim(blockgroupstats)
# dim(newvars)
# newvars
# newvars[ST == "DC", ]


test_that("one state", {

  # this requires having set up a census api key - see ?tidycensus::census_api_key
  skip_if(nchar(Sys.getenv("CENSUS_API_KEY")) == 0, message = "missing census API key so cannot test where tidycensus package is used")
  testthat::skip_if_not_installed("tidycensus") # also maybe has to be attached?

  expect_no_error(
    newvars <- acs_bybg(c(pop = "B01001_001", y = "B01001_002"), state = "DC")
  )
  expect_true(
    is.data.table(newvars)
  )
  expect_true(
    NROW(newvars) > 0 & NCOL(newvars) > 0
  )
})



test_that("Calculate a new indicator for each block group, using ACS data", {

  # this requires having set up a census api key - see ?tidycensus::census_api_key
  skip_if(nchar(Sys.getenv("CENSUS_API_KEY")) == 0, message = "missing census API key so cannot test where tidycensus package is used")
  testthat::skip_if_not_installed("tidycensus") # also maybe has to be attached?

  expect_no_error({

    mystates = c("DC", 'RI')
    newvars <- acs_bybg(variables = c("B01001_001", paste0("B01001_0", 31:39)),
                        state = mystates)
    setnames(newvars, "GEOID", "bgfips")
    newvars[, ST := fips2state_abbrev(bgfips)]
    names(newvars) <- gsub("E$", "", names(newvars))

    # provide formulas for calculating new indicators from ACS raw data:
    formula1 <- c(
      " pop = B01001_001",
      " age1849female = (B01001_031 + B01001_032 + B01001_033 + B01001_034 +
        B01001_035 + B01001_036 + B01001_037 + B01001_038 + B01001_039)",
      " pct1849female = ifelse(pop == 0, 0, age1849female / pop)"
    )
    newvars <- calc_ejam(newvars, formulas = formula1,
                         keep.old = c("bgid", "ST", "pop", 'bgfips'))

    newvars[, pct1849female := round(100 * pct1849female, 1)]

  })

  expect_true(
    is.data.table(newvars)
  )
  expect_true(
    NROW(newvars) > 0 & NCOL(newvars) > 0
  )
  expect_true(
    is.numeric(newvars$pct1849female)
  )

})



