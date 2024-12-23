
#######  THIS TEST IS SLOW !!!!!!!!!!

testthat::test_that("Estimate of radius, inferred from reported distances from getblocksnearby(), is accurate if radius specified with 2 or 3 decimals", {


  radius_error <- function(trials = 30, sitespertrial = 100, DECIMALS_USED_FOR_ACTUAL_RADIUS = 2)  {

    ##  try a number of trials of say 100 sites each analysis, to see how often this function infers radius accurately
    # trials <- 100
    # sitespertrial <- 100
    # DECIMALS_USED_FOR_ACTUAL_RADIUS <- 2

    out <- data.frame(actual = NA, trial = 1:trials, inferred = NA, diff = NA, pctdiff = NA)
    errs <- rep(0, trials)

    for (i in 1:trials) {

      actual <- runif(1,
                      min = 0.5,
                      max = convert_units(5, from = "km", towhat = "miles"))
      actual <- round(actual, DECIMALS_USED_FOR_ACTUAL_RADIUS)
capture_output({
      x <- getblocksnearby(
        testpoints_n(sitespertrial),
        radius = actual , quiet = T
      )
})
      # print(
      # system.time(
      out[i, "actual"]   <- actual # single number
      out[i, "inferred"] <- inferred <- radius_inferred(x) # single number
      out[i, "diff"]     <- diff <- (inferred - actual) # single number
      out[i, "pctdiff"]  <- pctdiff <- 100 * diff / actual # single number

      # cat("Inferred / estimated radius is", inferred,
      #     "which is an error of", round(diff, 4), "miles",
      #     paste0("(", round(pctdiff, 2), "%)\n"))
      # )
      # )

    }
    # print(out) # shows results of every single trial
    mean_abs_error <- mean(abs(out$diff))
    cat("Average absolute error (difference in miles inferred and actual) is",
        mean_abs_error, "\n"
    )
    max_abs_error <- max(abs(out$diff))

    cat("Max absolute value of error among all trials is",
        max_abs_error, "miles error\n"
    )
    return(c(mean_abs_error = mean_abs_error, max_abs_error = max_abs_error))
  }

  # testthat::expect_lt(
  #   radius_error(trials = 30, DECIMALS_USED_FOR_ACTUAL_RADIUS = 2)['max_abs_error'],
  #   0.1  # see if error is always under a tenth of a mile, for all trials
  # )
  #
  testthat::expect_lt(
    radius_error(trials = 25, DECIMALS_USED_FOR_ACTUAL_RADIUS = 3)['max_abs_error'],
    0.1  # see if error is average of under a tenth of a mile, for all trials
  )

})
