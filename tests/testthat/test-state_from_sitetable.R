
## TESTS FOR state_from_sitetable()  

testthat::test_that("state_from_sitetable() works", {
  suppressWarnings({
    junk = capture.output({
      
      x = state_from_sitetable(data.frame(ST = state.abb), ignorelatlon = TRUE)  # x$ST == state.abbr
      expect_equal(x$ST, state.abb)
      
      x = state_from_sitetable(data.frame(FIPS = fips_state_from_state_abbrev(c("DE","MT"))), ignorelatlon = TRUE)  # x$ST == c("DE","MT")
      expect_equal(x$ST, c("DE","MT"))
      
      x = state_from_sitetable(data.frame(stadfasdf = fips_state_from_state_abbrev(c("DE","MT"))), ignorelatlon = TRUE) # all(is.na(x$ST))
      expect_true(all(is.na(x$ST)))
      
      x = state_from_sitetable(fips_counties_from_state_abbrev("DE")) # x$ST == c("DE","DE","DE")
      expect_equal(x$ST, c("DE","DE","DE"))
      
      x = state_from_sitetable(data.frame(stadfasdf = fips_state_from_state_abbrev(c("DE","MT"))), ignorelatlon = FALSE) # x$ST == c(NA,NA)
      expect_true(all(is.na(x$ST)))
      
      x = x = state_from_sitetable(testpoints_10, ignorelatlon = FALSE) # x$ST == c("GA", "AL", "IL", "CA", "CA", "NJ", "CA", "MN", "UT", "NJ")
      expect_equal(x$ST, c("GA", "AL", "IL", "CA", "CA", "NJ", "CA", "MN", "UT", "NJ"))
    })
  })
  
})


# x = state_from_sitetable(data.frame(ST = state.abb), ignorelatlon = TRUE)  # x == state.abbr
# x = state_from_sitetable(data.frame(FIPS = fips_state_from_state_abbrev(c("DE","MT"))), ignorelatlon = TRUE)  # x$ST == c("DE","MT")
# x = state_from_sitetable(data.frame(stadfasdf = fips_state_from_state_abbrev(c("DE","MT"))), ignorelatlon = TRUE) # all(is.na(x$ST))
# x = state_from_sitetable(fips_counties_from_state_abbrev("DE")) # x$ST == c("DE","DE","DE")
# x = state_from_sitetable(data.frame(stadfasdf = fips_state_from_state_abbrev(c("DE","MT"))), ignorelatlon = FALSE) # x$ST == c(NA,NA)
# x = x = state_from_sitetable(testpoints_10, ignorelatlon = FALSE) # x$ST == c("GA", "AL", "IL", "CA", "CA", "NJ", "CA", "MN", "UT", "NJ")
