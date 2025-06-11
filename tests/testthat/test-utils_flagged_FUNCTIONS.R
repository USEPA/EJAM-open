## testing flagged_ functions for aggregating flags as summary stats

# count certain areas overlapped & if certain features are here ####

### Separate note:
## These should be the same? but are not quite the same!
# sum(tout$results_overall$pop , na.rm = T)
## [1] 8,609,095
# sum(tout$results_bybg_people$pop * tout$results_bybg_people$bgwt, na.rm = T)
## [1] 9,211,802

# make test data
tout <- ejamit(testpoints_1000, radius = 1) # ensures latest version of output is used but slower for testing
# tout <- testoutput_ejamit_1000pts_1miles # 
################################### #################################### #

test_that("ejam2barplot_areafeatures()", {
  expect_no_error({
    x = ejam2barplot_areafeatures(tout)
  })
  expect_true(
    "ggplot" %in% class(x)
  )
})
################################### #################################### #

test_that("ejam2areafeatures()", {
  expect_no_error({
    x = ejam2areafeatures(tout)
  })
  expect_equal(
    tout$results_summarized$flagged_areas, 
    x
  )
})
################################### #################################### #
test_that("flagged_pct_sites ok", {
  
  # make test data
  
  bysite  = tout$results_bysite
  bybg    = tout$results_bybg_people
  overall = tout$results_overall
  bybg_us = blockgroupstats
  ### bybg_st = ???
  mycols <- c(names_featuresinarea, names_flag, names_criticalservice)
  
  t1 = round(100 * bysite[, lapply(.SD, function(z) {sum(z, na.rm = TRUE) }), 
                          .SDcols = mycols] / NROW(bysite), 1)
  x = flagged_pct_sites(bysite)
  expect_equal(
    data.frame(x)[, setdiff(names(x), names_featuresinarea)],
    data.frame(t1)[, setdiff(colnames(t1), names_featuresinarea)] # school/hosp/chu are counts we now count sum(x>0) not sum(x)
  )
  t1b = round(100 * bysite[, lapply(.SD, function(z) {sum(z > 0, na.rm = TRUE) }), 
                          .SDcols = mycols] / NROW(bysite), 1)
  expect_equal(
    data.frame(x)[, names_featuresinarea],
    data.frame(t1b)[, names_featuresinarea] 
  )
})
################################### #################################### #
test_that("flagged_pct_pop ok", {
  
  # make test data
  # tout <- testoutput_ejamit_1000pts_1miles
  bysite  = tout$results_bysite
  bybg    = tout$results_bybg_people
  overall = tout$results_overall
  bybg_us = blockgroupstats
  ### bybg_st = ???
  mycols <- c(names_featuresinarea, names_flag, names_criticalservice)
  
  t2 = round(100 * bybg[, lapply(.SD, function(z) {sum(z * pop * bgwt, na.rm = TRUE) }), 
                        .SDcols = mycols] / sum(bybg$pop * bybg$bgwt, na.rm = TRUE) , 1) # overall$pop  is a bit different as denominator
  x = flagged_pct_pop(bybg)
  expect_equal(
    data.frame(x)[, setdiff(colnames(x), names_featuresinarea)],
    data.frame(t2)[, setdiff(colnames(t2), names_featuresinarea)]
  )
})
################################### #################################### #
test_that("flagged_pct_pop calculate for 2 bgs", {
  
  out <- ejamit(testpoints_10[2,], radius = 1)
  via_function <- out$results_summarized$flagged_areas$Percent_of_these_People[out$results_summarized$flagged_areas$rname == 'num_church']
  
  x <- data.frame(out$results_bybg_people)[, c('pop', 'bgwt', names_criticalservice, names_featuresinarea, names_flag)] 
  via_formula_here <- sum(x$pop * x$bgwt * (x$num_church > 0)) / sum(x$pop * x$bgwt )

expect_equal( 
  via_function,
  round(100 * via_formula_here, 1)
)
})
################################### #################################### #
test_that("flagged_pct_pop_us ok", {
  
  # make test data
  # tout <- testoutput_ejamit_1000pts_1miles
  bysite  = tout$results_bysite
  bybg    = tout$results_bybg_people
  overall = tout$results_overall
  bybg_us = blockgroupstats
  ### bybg_st = ???
  mycols <- c(names_featuresinarea, names_flag, names_criticalservice)
  
  capture.output({
    via_function = as.data.frame(flagged_pct_pop_us())
  })
  via_function = via_function[, setdiff(colnames(via_function), names_criticalservice)]
    
  
  t3 = round(100 * blockgroupstats[, lapply(.SD, function(z) {sum((z > 0) * pop, na.rm = TRUE)}), 
                                   .SDcols = mycols] / sum(blockgroupstats$pop, na.rm = T), 1)
  t3 = as.data.frame(t3)[, setdiff(colnames(via_function), names_criticalservice)]

  expect_equal(
    via_function,
    t3
  )
})
################################### #################################### #
test_that("flagged_count_pop_st multiple states add up", {
  expect_equal(
    flagged_count_pop_st(c('pr', 'dc')), 
    flagged_count_pop_st('dc') + flagged_count_pop_st('pr')
  )
})
###################################################################################### # 
# format these summary stats for barplot ####
###################################################################################### # 

test_that("flagged_areas_from_ejam", {
  
  f_expected = structure(list(
    Indicator = c("Any schools", "Any hospitals",
                  "Any places of worship", "Overlapping with Tribes", "Overlapping with Non-Attainment Areas",
                  "Overlapping with Impaired Waters", "Overlapping with CJEST Disadvantaged Communities",
                  "Overlapping with EPA IRA Disadvantaged Communities", "Overlapping with Housing Burden Communities",
                  "Overlapping with Transportation Disadvantaged Communities",
                  "Overlapping with Food Desert Areas", "% Households without Broadband Internet",
                  "% Households without Health Insurance"),
    Percent_of_these_Sites = c(92, 61.9, 92.6, 5.1, 59.6, 97.2, 69.3, 83.2, 43.1, 97.1, 70.9, 12.5, 2.8),
    Percent_of_these_People = c(35, 5.3, 39.1, 0.4, 96.3, 49.2, 44.1, 55.8, 31, 70.4, 17.9, 12.5, 4.6),
    Percent_of_all_People_Nationwide = c(31.3, 4.6, 38.8, 1.6, 57.2, 56.7, 32.3, 42.8, 12.5, 69.5, 25, 11.5, 3.4),
    ratio = c(1.12, 1.15, 1.01, 0.25, 1.68, 0.87, 1.37, 1.3, 2.48, 1.01, 0.72, 1.09, 1.35),
    rname = c("num_school", "num_hospital", "num_church", "yesno_tribal", "yesno_airnonatt", "yesno_impwaters", "yesno_cejstdis", "yesno_iradis", "yesno_houseburden", "yesno_transdis", "yesno_fooddesert", "pctnobroadband", "pctnohealthinsurance")
  ), row.names = c(NA, -13L), class = "data.frame")
  
  # out_test = ejamit(testpoints_1000, radius = 1)
  out_test = list(results_summarized = list(flagged_areas = f_expected))
  
  expect_no_error({
    expect_equal(
      flagged_areas_from_ejam(out_test),
      f_expected
    )
  })
})
################################################################# #

test_that("flagged_areas_ratiosvector_from_flagged_areas", {
  
  f_expected = structure(list(
    Indicator = c("Any schools", "Any hospitals",
                  "Any places of worship", "Overlapping with Tribes", "Overlapping with Non-Attainment Areas",
                  "Overlapping with Impaired Waters", "Overlapping with CJEST Disadvantaged Communities",
                  "Overlapping with EPA IRA Disadvantaged Communities", "Overlapping with Housing Burden Communities",
                  "Overlapping with Transportation Disadvantaged Communities",
                  "Overlapping with Food Desert Areas", "% Households without Broadband Internet",
                  "% Households without Health Insurance"),
    Percent_of_these_Sites = c(92, 61.9, 92.6, 5.1, 59.6, 97.2, 69.3, 83.2, 43.1, 97.1, 70.9, 12.5, 2.8),
    Percent_of_these_People = c(35, 5.3, 39.1, 0.4, 96.3, 49.2, 44.1, 55.8, 31, 70.4, 17.9, 12.5, 4.6),
    Percent_of_all_People_Nationwide = c(31.3, 4.6, 38.8, 1.6, 57.2, 56.7, 32.3, 42.8, 12.5, 69.5, 25, 11.5, 3.4),
    ratio = c(1.12, 1.15, 1.01, 0.25, 1.68, 0.87, 1.37, 1.3, 2.48, 1.01, 0.72, 1.09, 1.35),
    rname = c("num_school", "num_hospital", "num_church", "yesno_tribal", "yesno_airnonatt", "yesno_impwaters", "yesno_cejstdis", "yesno_iradis", "yesno_houseburden", "yesno_transdis", "yesno_fooddesert", "pctnobroadband", "pctnohealthinsurance")
  ), row.names = c(NA, -13L), class = "data.frame")
  
  # out_test = ejamit(testpoints_1000, radius = 1)
  out_test = list(results_summarized = list(flagged_areas = f_expected))
  
  v_expected = c(`Any schools` = 1.12, `Any hospitals` = 1.15, `Any places of worship` = 1.01,
                 `Overlapping with Tribes` = 0.25, `Overlapping with Non-Attainment Areas` = 1.68,
                 `Overlapping with Impaired Waters` = 0.87, `Overlapping with CJEST Disadvantaged Communities` = 1.37,
                 `Overlapping with EPA IRA Disadvantaged Communities` = 1.3, `Overlapping with Housing Burden Communities` = 2.48,
                 `Overlapping with Transportation Disadvantaged Communities` = 1.01,
                 `Overlapping with Food Desert Areas` = 0.72, `% Households without Broadband Internet` = 1.09,
                 `% Households without Health Insurance` = 1.35)
  expect_no_error({
    expect_equal(
      v_expected,
      flagged_areas_ratiosvector_from_flagged_areas(f_expected)
    )
  })
})
################################################################# #

test_that("flagged_areas_ratios_from_ejam", {
  
  f_expected = structure(list(
    Indicator = c("Any schools", "Any hospitals",
                  "Any places of worship", "Overlapping with Tribes", "Overlapping with Non-Attainment Areas",
                  "Overlapping with Impaired Waters", "Overlapping with CJEST Disadvantaged Communities",
                  "Overlapping with EPA IRA Disadvantaged Communities", "Overlapping with Housing Burden Communities",
                  "Overlapping with Transportation Disadvantaged Communities",
                  "Overlapping with Food Desert Areas", "% Households without Broadband Internet",
                  "% Households without Health Insurance"),
    Percent_of_these_Sites = c(92, 61.9, 92.6, 5.1, 59.6, 97.2, 69.3, 83.2, 43.1, 97.1, 70.9, 12.5, 2.8),
    Percent_of_these_People = c(35, 5.3, 39.1, 0.4, 96.3, 49.2, 44.1, 55.8, 31, 70.4, 17.9, 12.5, 4.6),
    Percent_of_all_People_Nationwide = c(31.3, 4.6, 38.8, 1.6, 57.2, 56.7, 32.3, 42.8, 12.5, 69.5, 25, 11.5, 3.4),
    ratio = c(1.12, 1.15, 1.01, 0.25, 1.68, 0.87, 1.37, 1.3, 2.48, 1.01, 0.72, 1.09, 1.35),
    rname = c("num_school", "num_hospital", "num_church", "yesno_tribal", "yesno_airnonatt", "yesno_impwaters", "yesno_cejstdis", "yesno_iradis", "yesno_houseburden", "yesno_transdis", "yesno_fooddesert", "pctnobroadband", "pctnohealthinsurance")
  ), row.names = c(NA, -13L), class = "data.frame")
  
  # out_test = ejamit(testpoints_1000, radius = 1)
  out_test = list(results_summarized = list(flagged_areas = f_expected))
  
  v_expected = c(`Any schools` = 1.12, `Any hospitals` = 1.15, `Any places of worship` = 1.01,
                 `Overlapping with Tribes` = 0.25, `Overlapping with Non-Attainment Areas` = 1.68,
                 `Overlapping with Impaired Waters` = 0.87, `Overlapping with CJEST Disadvantaged Communities` = 1.37,
                 `Overlapping with EPA IRA Disadvantaged Communities` = 1.3, `Overlapping with Housing Burden Communities` = 2.48,
                 `Overlapping with Transportation Disadvantaged Communities` = 1.01,
                 `Overlapping with Food Desert Areas` = 0.72, `% Households without Broadband Internet` = 1.09,
                 `% Households without Health Insurance` = 1.35)
  expect_no_error({
    expect_equal(
      flagged_areas_ratios_from_ejam(out_test),
      v_expected
    )
  })
})
################################################################# #

test_that("flagged_areas_shortlabels_from_ejam", {
  
  flagged_areas_testnames = c("num_school", "num_hospital", "num_church", "yesno_tribal",
                              "yesno_airnonatt", "yesno_impwaters", "yesno_cejstdis", "yesno_iradis",
                              "yesno_houseburden", "yesno_transdis", "yesno_fooddesert", "pctnobroadband",
                              "pctnohealthinsurance")
  
  out_test <- list(results_summarized = list(flagged_areas = data.frame(
    rname = flagged_areas_testnames,
    ratio = c(1.2, 2.54, 1.23, 0, 2.01, 0.72, 0.67, 0.69, 1.98, 1.38, 0.5,
              0.89, 0.62)
  )))
  # # out_test = testoutput_ejamit_1000pts_1miles
  # # out_test = ejamit(testpoints_10, radius = 1)
  
  expect_no_error({
    expect_equal(
      flagged_areas_shortlabels_from_ejam(out_test),
      c("Schools", "Hospitals", "Worship Places", "Tribes", "Nonattainment Area",
        "Impaired Waters", "CJEST Disadvantaged", "EPA IRA Disadvantaged",
        "Housing Burden Community", "Transportation Disadvantaged", "Food Desert",
        "% hhlds no Broadband Internet", "% hhlds no Health Insurance")
    )
  })
})
################################################################# #

