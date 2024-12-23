

# see notes about weighted means below


############################################### # 


#' aggregate blockwt values by blockgroup, by site they are in or near
#'
#' @param sites2blocks like output of [getblocksnearby()]
#'   or input to [doaggregate()] or [custom_doaggregate()]
#' @seealso [custom_doaggregate()]
#'
#' @return data.table, 1 row per site-bg pair.
#'   May have same bgid or bgfips in 2,3, more rows
#'   since it is here once per site that the bg is near.
#'   It is like a sites2blockgroups table.
#' 
#' @keywords internal
#'
calc_bgwts_bysite <- function(sites2blocks) {
  sites2blocks[, .(bgwt = sum(blockwt)), keyby = c("ejam_uniq_id", "bgid")]
  # maybe add bgfips for convenience
  # This is the same rows as results_bybg_people,
  # but lacks any joined or calculated results.
}
############################################### # 


#' aggregate blockwt values by blockgroup
#'
#' @param sites2blocks like output of [getblocksnearby()]
#'   or input to [doaggregate()] or [custom_doaggregate()]
#' @seealso [custom_doaggregate()]
#' @return data.table, 1 row per blockgroup (even if bg is near 2+ sites),
#'   so it is a table of all the unique block groups in the overall
#'   analysis (merged across all sites), with a weight that indicates
#'   what fraction of that bg population is included in the overall
#'   analysis. This can be used to get overall results if it is
#'   joined to block group demographics, etc.,
#'   to aggregate each indicator over all block groups using the weights.
#' 
#' @keywords internal
#'
calc_bgwts_overall <- function(sites2blocks) {
  sites2blocks[, .(bgwt = sum(blockwt)), keyby = "bgid"]
  # maybe add bgfips for convenience
}
############################################### # 

#' utility - what type of formula is used to aggregate this variable?
#'
#' @param varnames vector like names_d
#'
#' @return vector same length as varnames, like c("sum of counts", "wtdmean")
#' @examples 
#'  calctype("pop")
#'  calctype(names_d)
#'  x = names(testoutput_ejamit_10pts_1miles$results_overall)
#'  cbind(x, calctype(x))
#'  
#' @export
#' @keywords internal
#'
calctype <- function(varnames) {
  varinfo(varnames, "calculation_type")[, "calculation_type"]
  # map_headernames$calculation_type[match(varnames, map_headernames$rname)]
}
############################################### # 

#' utility - what variable is the weight used to aggregate this variable as a weighted mean?
#'
#' @param varnames vector like names_d
#'
#' @return vector same length as varnames, like c("pop", "povknownratio", "hhlds")
#' @examples calcweight(names_these)
#' 
#' @export
#' @keywords internal
#' 
calcweight <- function(varnames) {
  varinfo(varnames, "denominator")[, "denominator"]
  # map_headernames$denominator[match(varnames, map_headernames$rname)]
}
############################################### # 

# custom_doaggregate_from_sites2blocks

#' custom version of doaggregate(), to calculate user-provided indicators
#'
#' @param sites2blocks see [doaggregate()]
#' 
#' @param custom_blockgroupstats like blockgroupstats but with custom
#'   indicators, one value per block group, with colnames bgid, bgfips, pop
#' @param countcols vector of colnames in custom_blockgroupstats to be
#'   aggregated as sums of counts, like population counts
#' @param popmeancols vector of colnames in custom_blockgroupstats to be
#'   aggregated as weighted means, population weighted or with other weights
#'   
#' @param wtcols vector of colnames to use as the weights for wtd means,
#'   same length as popmeancols, but not used yet
#'   
#' @param custom_formulas like formulas_all,  not used yet
#' @param custom_cols not used yet
#' @param custom_map_headernames like map_headernames but for the
#'   custom indicators
#'
#' @return list of tables similar to what [doaggregate()] returns
#' 
#' @export
#'
custom_doaggregate <- function(sites2blocks,
                               custom_blockgroupstats = blockgroupstats,
                               countcols = "pop",
                               popmeancols = names_these,
                               wtcols = "pop", # or a vector exactly as long as popmeancols
                               custom_formulas = NULL, # formulas_d,
                               custom_cols = NULL,
                               custom_map_headernames = map_headernames) {
  
  # could add validation of inputs here
  
  
  #  had 2 options: 
  #   1. doaggregate() modified to do the work as usual PLUS take as new param the custom_blockgroupstats,  add in the new columns. 
  #     That would save some duplicated work since only would aggregate block weights in doagg not again in custom_doagg
  #     # and consolidate duplicated code that does error checks, etc. etc.
  #     # and could use the percentile lookup and other code that is there, and add results to 
  # or 
  #   2. custom_doaggregate() to separately do the work calculations
  #     and you can separately then merge that with final results of doaggregate (i.e.,  results_bysite and results_overall and results_bybgetcetc)
  # no percentiles looked up or reported
  # no averages looked up or created
  # no ratios calculated
  # no EJ Indexes created
  # no summary stats like from batch.summarize(), etc.
  
  #################### #
  # aggregate from blocks up to blockgroups
  
  bybg_bysite  <- calc_bgwts_bysite( sites2blocks) # 1 row per site-bg pair (like results_bybg_people) (not rolled up yet to just sites)
  bybg_overall <- calc_bgwts_overall(sites2blocks) # 1 row per bg (not rolled up yet to just one overall total)
  
  
  ################# ------------------------------------------------------------------------- ################ #
  
  ######## here we have something like  sites2blockgroups, aka  bybg_bysite,  
  ##  and sometimes you want to start from that and do the rest of doaggregate() without starting from sites2blocks !
  # therefore,
  ##            *** the code below should be made available on its own without the part above here.
  #
  # for example, if you had done this:
  # x = EJAM::counties_as_sites(fips_counties_from_state_abbrev("DE"))
  # > x
  # ejam_uniq_id countyfips  bgid
  # <int>     <char> <int>
  #   1:            1      10001 43878
  #   2:            1      10001 43879
  ################# ------------------------------------------------------------------------- ################ #
  
  # custom_doaggregate_from_sites2blocks_from_sites2blocks <- function(sites2blockgroups, 
  #                                                                    etc.,
  #                                                                    etc.,
  #                                                                    etc.) {
  #   
  # }
  
  
  #################### #
  # join nationwide indicator data to these places analyzed
  bybg_bysite  <- merge(bybg_bysite,  custom_blockgroupstats, by = "bgid")
  bybg_overall <- merge(bybg_overall, custom_blockgroupstats, by = "bgid")
  
  #################### #
  # calculations just for EACH block group 
  #  No aggregation yet (sum of counts, percentage as ratio, avg of 2 values, etc.)
  if (!is.null(custom_formulas)) {
    bybg_bysite   <- calc_ejam(bybg_bysite,  keep.old = c("ejam_uniq_id", "bgid", "pop", "bgwt"), keep.new = "all", formulas = custom_formulas)
    bybg_overall  <- calc_ejam(bybg_overall, keep.old = c("ejam_uniq_id", "bgid", "pop", "bgwt"), keep.new = "all", formulas = custom_formulas)
  }
  
  ################# ------------------------------------------------------------------------- ################ #
  
  #################### #
  # calculations that AGGREGATE across all blockgroups within each site and overall
  # wtd mean of bgs, sum of counts at bgs, or min or max of bgs.
  # bgwt is the block group weight to use since some bg are only partially included in zone
  
  #################### #
  ## sums of counts
  
  ### started this idea but would need to apply calc_ejam() by group, and would need to include bgwt * x, etc. 
  # see datacreate_formulas_d or formulas_all
  # 
  # results_overall <- calc_ejam(bybg_overall, keep.old = "", keep.new = "all", formulas = formulas_all)
  # results_bysite <- 
  # need to do rollup by group, so could apply calc_ejam by group here
  # or more efficiently for common formulas like sum or popwtdmean
  
  # how it is done in doaggregate() is this:
  
  # sum the counts but weight the counts by the bgwt (since some bgs are only partially included)
  
  countcols_inbgstats = countcols[countcols %in% names(bybg_overall)]
  
  results_overall <- bybg_overall[, lapply(.SD, FUN = function(x) {
    round(sum(x * bgwt, na.rm = TRUE), 1)
  }), .SDcols = countcols_inbgstats]
  
  # sum the counts but weight the counts by the bgwt, but for each site
  
  results_bysite <- bybg_bysite[, lapply(.SD, FUN = function(x) {
    round(sum(x * bgwt, na.rm = TRUE), 1)
  }), .SDcols = countcols_inbgstats, by = .(ejam_uniq_id)]
  
  results_bybg <- bybg_bysite # table of 1 row per bg-site pair is already done
  
  #################### #
  ## wtd means

 ####   see drafted  calc_wtdmeans() and see doaggregate() as updated
stop('not done yet - see newer version of doaggregate() for weighted means')
  ################################################################################################################ #
  
  popmeancols_inbgstats = popmeancols[popmeancols %in% names(bybg_overall)]
  
  ## popwtd mean by SITE ###
  results_bysite_popmeans <- bybg_bysite[   ,  lapply(.SD, FUN = function(x) {
    collapse::fmean(x, w = bgwt * pop)  ## how to use value of wtcols as the colname, not always "pop" here? ***
  }), .SDcols = popmeancols_inbgstats, by = .(ejam_uniq_id) ]
  
  results_bysite <- merge(results_bysite, results_bysite_popmeans, by = "ejam_uniq_id")
  
  
  ## popwtd mean OVERALL ###
  results_overall_popmeans <- bybg_overall[ ,  lapply(.SD, FUN = function(x) {
    collapse::fmean(x, w = bgwt * pop) ## how to use value of wtcols as the colname, not always "pop" here? ***
  }), .SDcols = popmeancols_inbgstats  ]
  
  results_overall <- cbind(results_overall, results_overall_popmeans)
  
  # results_bybg  table of 1 row per bg-site pair is already done
  
  
  if (!is.null(custom_formulas)) {
    ################################################# #
    # aggregation via CUSTOM FORMULAS would be HANDLED HERE #
    ################################################# #
    
    # if ("WORKING YET?" == "YES NOW" && !is.null(custom_formulas)) {
    #   
    #   # to be written...
    #   ## *** PROBLEM HOW TO ALLOW CUSTOM FORMULAS THAT
    #   ##  ALSO WILL  INCORPORATE THE bgwt multiplication 
    #   ##   needed to rollup across block groups correctly??
    #   
    #   
    #   if (is.null(custom_cols)) {
    #     custom_cols = EJAM:::formula_varname(custom_formulas)
    #   }
    # 
    # results_bysite_custom  <-  bybg_bysite[ , calc_ejam( ..???? ), by = "ejam_uniq_id"] 
    
    # need to aggregate from 1 row per site-bg pair into just 1 row per site
    
    #   setDT(bybg_bysite)[, sitepop := sum(pop * bgwt, na.rm = TRUE), by = .(ejam_uniq_id)]
    # bybg_bysite
    
    results_bysite_custom <- list()
    ids = unique(bybg_bysite$ejam_uniq_id)
    n = length(ids)
    for (sitenum in 1:n) {
      
      # THIS IS WRONG - IT SHOULD ROLL UP BY SITE BUT STILL KEEPS ALL THE BLOCKGROUPS...
      # and should use btwt
      
      # NEED A WAY TO DO AGGREGATION BYSITE AND FORMULAS AT THE SAME TIME OR CORRECTLY SEPARATELY.
      #  calc_ejam() has each formula but  does no aggregation.
      #  and just doing data.table   dt[, xyz, by = "ejam_uniq_id"]  
      #   would aggregate but need the formula(s) in there.
      #  check formulas_all, which seemed to allow for aggregation-like calculation??
      
      results_bysite_custom[[sitenum]] <- calc_ejam(
        bybg_bysite[bybg_bysite$ejam_uniq_id == ids[sitenum], ],
        keep.old = c("bgid" ,"pop"),
        keep.new = "all",
        formulas = custom_formulas
      )
    }
    results_bysite_custom <- rbindlist(results_bysite_custom)
    
    # 
    # 
    # 
    # results_overall_custom <-  aggregate to 1 row only
    # 
    #   
    #   
    #   # add it to the other outputs
    
    #   results_overall <- .........................
    
    # }
  }
  
  ########## no ratios, percentiles, averages, etc. etc.
  ########## no other columns added like radius.miles, lat/lon, URLs, block counts, etc. etc. 
  
  ## but also note newer table_signif_round_x100()
  # results_overall = table_x100(results_overall, cnames = names_pct_as_fraction_ejamit)
  # results_bysite  = table_x100(results_bysite, cnames = names_pct_as_fraction_ejamit)
  # results_bybg    = table_x100(results_bybg, cnames = names_pct_as_fraction_ejamit)

  return(
    list(
      results_overall = results_overall, 
      results_bysite  = results_bysite, 
      results_bybg    = results_bybg, 
      longnames = fixcolnames(names(results_overall), 'r', 'long', custom_map_headernames)
    )
  )
}
############################################### # 


#' custom version of ejamit() for calculating user-provided indicators
#'
#' @param sitepoints see [ejamit()]
#' @param radius  see [ejamit()]
#' @param fips  see [ejamit()]
#' @param shapefile  see [ejamit()]
#' @param custom_blockgroupstats like blockgroupstats but with custom
#'   indicators, one value per block group, with colnames bgid, bgfips, pop
#' @param countcols vector of colnames in custom_blockgroupstats to be
#'   aggregated as sums of counts, like population counts
#' @param popmeancols vector of colnames in custom_blockgroupstats to be
#'   aggregated as weighted means, population weighted or with other weights
#'   
#' @param wtcols vector of colnames to use as the weights for wtd means,
#'   same length as popmeancols, but not used yet
#'   
#' @param custom_formulas like formulas_all,  not used yet
#' @param custom_cols not used yet
#' @param custom_map_headernames like map_headernames but for the
#'   custom indicators
#'
#' @return returns the output of [custom_doaggregate()]
#' 
#' @export
#' 
custom_ejamit <- function(sitepoints, radius = 3, fips = NULL, shapefile = NULL,
                          custom_blockgroupstats = blockgroupstats,
                          countcols = names_wts,
                          popmeancols = names_these,
                          wtcols = names_wts, # "pop"  or a vector exactly as long as wtdmeancols
                          custom_formulas = NULL, # formulas_d,
                          custom_cols = NULL,
                          custom_map_headernames = map_headernames) {
  
  if (!is.null(fips)) {
    sites2blocks <- getblocksnearby_from_fips(fips)
  } else {
    if (!is.null(shapefile)) {
      sites2blocks <- get_blockpoints_in_shape(shapefile_from_any(shapefile))
    } else {
      sites2blocks <- getblocksnearby(sitepoints = sitepoints, radius = radius)
    }
  }
  
  return(
    custom_doaggregate(sites2blocks = sites2blocks,
                       custom_blockgroupstats = custom_blockgroupstats,
                       countcols = countcols,
                       popmeancols = popmeancols,
                       wtcols = wtcols,
                       custom_formulas = custom_formulas,
                       custom_cols = custom_cols,
                       custom_map_headernames = custom_map_headernames
    )
  )
}
############################################### # 



############################################### # 
# stop() !!!

### test/debug/ try these new functions ...
# 
# outapi = ejscreenit_for_ejam(testpoints_10, radius = 1)
# 
#   ## s2b = getblocksnearby(testpoints_10, radius = 1)
#   s2b = testoutput_getblocksnearby_10pts_1miles
#   x = custom_doaggregate(s2b)
if (1 == 0) {
  data.frame(
    custom = round(t(x$results_overall[, ..names_these]),3),
    ejamit = round(t(testoutput_ejamit_10pts_1miles$results_overall[, ..names_these]),3)
  )
  
  ##                    custom    ejamit
  ## Demog.Index         0.413    0.402   ## pop mean not quite same as how it was calculated as ratio of sums of counts or correct-denominator-wtd-mean. 
  ## Demog.Index.Supp    0.180    0.177   ## pop mean not quite same as how it was calculated as ratio of sums of counts or correct-denominator-wtd-mean.
  ## pctlowinc           0.417    0.396   ## pop mean not quite same as how it was calculated as ratio of sums of counts or correct-denominator-wtd-mean.
  ## pctlingiso          0.070    0.068   ## pop mean not quite same as how it was calculated as ratio of sums of counts or correct-denominator-wtd-mean.
  ## pctunemployed       0.059    0.057   ## pop mean not quite same as how it was calculated as ratio of sums of counts or correct-denominator-wtd-mean.
  ## pctlths             0.141    0.149   ## pop mean not quite same as how it was calculated as ratio of sums of counts or correct-denominator-wtd-mean.
  ## lowlifex            0.216    0.216
  ## pctunder5           0.064    0.064
  ## pctover64           0.108    0.108
  ## pctmin              0.408    0.408
  ## pcthisp            25.471    0.255  ******* 100x
  ## pctnhba             8.590    0.086  ******* 100x
  ## pctnhaa             2.968    0.030  ******* 100x
  ## pctnhaiana          0.884    0.009  ******* 100x
  ## pctnhnhpia          0.002    0.000  ******* 100x
  ## pctnhotheralone     0.091    0.001  ******* 100x
  ## pctnhmulti          2.836    0.028  ******* 100x
  ## pctnhwa            59.158    0.592  ******* 100x
  ## pm                  8.034    8.034
  ## o3                 60.623   60.623
  ## cancer             26.527   26.527
  ## resp                0.289    0.289
  ## dpm                 0.363    0.363
  ## pctpre1960         38.380    0.413  ******* 100x and ## pop mean not quite same as how it was calculated as ratio of sums of counts or correct-denominator-wtd-mean.
  ## traffic.score     179.121  179.121
  ## proximity.npl       0.387    0.387
  ## proximity.rmp       0.595    0.595
  ## proximity.tsdf      1.353    1.353
  ## proximity.npdes     0.014    0.014
  ## ust                 5.489    5.489
  ## rsei             6492.776 6492.776
  
  
  i = 3 
  
  cbind(
    custom = round(t(x$results_bysite[i, ..names_these]),3),
    ejamit = round(t(testoutput_ejamit_10pts_1miles$results_bysite[i, ..names_these]),3),
    ejscreenit = round(t(outapi[i, ..names_these]),3) # just one site
  )
  
  
  ######################################################### # 
  ######################################################### # 
  
  
  supressWarnings({
    x = custom_ejamit(testpoints_10, 
                      custom_blockgroupstats = data.frame(blockgroupstats[,.(pop, bgfips, bgid, pctlowinc)]), 
                      countcols = "pop", 
                      popmeancols = "pctlowinc", 
                      wtcols = "pop", 
                      custom_formulas = formulas_d, 
                      custom_map_headernames = map_headernames)
  })
  
  y = ejamit(testpoints_10)
  x$results_bysite[,.(ejam_uniq_id,      pop,  pctlowinc )]
  y$results_bysite[,.(ejam_uniq_id,      pop,  pctlowinc )]
  
  ######################################################### # 
  ######################################################### # 
  
  # define some test inputs  
  
  formulas_test = c("high_pctlowinc <- pctlowinc >= 0.50", "high_pop <- pop >= 5000")
  
  formulas_test = c(formulas_test, formulas_d)
  
  bg_test = data.frame(blockgroupstats[, .(bgid, bgfips, pop, pctlowinc)])
  
  map_headernames_test = data.frame(
    rname  = c("pop", "pctlowinc", "high_pctlowinc", "high_pop"),
    shortlabel = c("Pop", "%low-inc.", "High %lowinc?", "Large Pop.?"),
    longname_tableheader = c("Pop", "% low income", "% low income is High", "Population is Large"),
    pct_as_fraction_blockgroupstats = FALSE,
    pct_as_fraction_ejamit = FALSE
  )
  
  
  sitepoints = testpoints_10
  custom_blockgroupstats = bg_test
  countcols = "pop"
  popmeancols = "pctlowinc"
  wtcols = "pop"
  # custom_cols = NULL
  custom_formulas = formulas_test
  custom_map_headernames = map_headernames_test
  radius = 3
  
  # do this which is in custom_ejamit()
  sites2blocks <- getblocksnearby(sitepoints = sitepoints, radius = radius)
  
  
  
  # out_test <- custom_ejamit(
  #   sitepoints = testpoints_10,
  #   custom_blockgroupstats = bg_test, 
  #   countcols = "pop",
  #   popmeancols = "pctlowinc", 
  #   wtcols = "pop", 
  #   # custom_cols = NULL,
  #   custom_formulas = formulas_test, 
  #   custom_map_headernames = map_headernames_test
  # )
  
  out_test$results_bysite
  
  
  # out_test$results_bysite[out_test$results_bysite$high_pctlowinc, ]
  
}

