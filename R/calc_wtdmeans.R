if (1 == 0) {
  
  
  
  ##              started drafting this to go in doaggregate()
  
  
  
  
  
  
  
  #             some of doaggregate() upstream of the wtdmeans step is this:
  ## test data examples:
  sites2bgs_bysite = testoutput_getblocksnearby_10pts_1miles
  countcols_inbgstats = c("pop", names_d_count)
  calculatedcols_inbgstats = "pctunder18"
  wtdmeancols_inbgstats = names_d # intersect(names_all_r[calctype(names_all_r) %in%  c("wtdmean")], names(blockgroupstats))
  
  sites2bgs_plusblockgroupdata_bysite  <- merge(sites2bgs_bysite,
                                                # dt <- merge(sites2bgs_bysite,
                                                ##  but has other cols like   "distance_avg" , "proximityscore"  etc.
                                                blockgroupstats[ , c('bgid', 'ST', ..countcols_inbgstats, ..wtdmeancols_inbgstats, ..calculatedcols_inbgstats)],
                                                all.x = TRUE, all.y = FALSE, by = 'bgid')
  
  ################################# # 
  
  
  ##              started drafting this to go in doaggregate()
  
  
  # cbind(table(EJAM::map_headernames$denominator))
  
  # age25up           1
  # builtunits        2
  # disab_universe    1
  # hhlds             2
  # lan_universe     15
  # lingiso           4
  # occupiedunits     1
  # pop             122
  # povknownratio     1
  # unemployedbase    1
  
  ## mean person (or hhld etc.) at each SITE ###
  ## cant really update by reference, adding new columns, bc aggregating at the same time

  # had been doing this:
  # 
  #   ## mean OVERALL ###
  #   ## later, for results_overall, will calc state pctiles once we have them for each site
  #   
  #   results_overall_popmeans <- sites2bgs_plusblockgroupdata_overall[ ,  lapply(.SD, FUN = function(x) {
  #     collapse::fmean(x, w = bgwt * denom) # stats::weighted.mean(x, w = bgwt * pop, na.rm = TRUE)
  #   }), .SDcols = wtdmeancols_inbgstats  ]
  #   
  # 
  # results_bysite <- merge(results_bysite, results_bysite_popmeans, by = "ejam_uniq_id") # dont we need by = "ejam_uniq_id" or just to be clear?  It defaults to merging by the shared key columns between the two tables. If y has no key columns, this defaults to the key of x.
  # results_overall <- cbind(results_overall, results_overall_popmeans) # many columns (the popwtd mean cols)
  
  
}
################### # 


calc_wtdmeans <- function(dt, 
                          score_colnames = NULL, # wtdmeancols_inbgstat, 
                          wts_bg_colname = "bgwt", 
                          wts_denom_colnames = NULL, 
                          by_colname = "ejam_uniq_id") {
  
  ##  looping over unique denominator types, which makes fmean easier to code
  ## but makes it a bit harder to reassemble results
  ## but... maybe fmean(g= ) parameter can do the grouping by weighting type ("pop" vs "hhld" vs etc.)
  
  if (is.null(score_colnames)) {
    score_colnames <- intersect(names_all_r[calctype(names_all_r) %in%  c("wtdmean")], names(blockgroupstats))
  }
  if (is.null(wts_denom_colnames)) {
    wts_denom_colnames <- calcweight(score_colnames)
  } 
  denomtypes <- unique(wts_denom_colnames)
  for (i in seq_along(denomtypes)) {
    
    colnums_using_this_denom_type <- which(wts_denom_colnames == denomtypes[i])
    
    colnum_by <- which(colnames(dt) == by_colname)
    
    wts <- dt$wts_bg_colname * dt[, denomtypes[i], with = FALSE]
    
    # define inner func
    wtdmeans_basic <- function(y, wts, by_colname) {
      
      y[, lapply(.SD, FUN = function(x) {
        collapse::fmean(x,
                        na.rm = TRUE,
                        # g = by_colname, # if we let fmean do the grouping by site rather than having data.table do grouping by site.
                        w = wts
        )
      }), .SDcols = colnames(y), by = by_colname, with = FALSE]
      
    }
    
    # use it
    results_thisdenomtype <- wtdmeans_basic(dt[, c(colnums_using_this_denom_type, colnum_by), with = FALSE], 
                                            wts = wts,
                                            by_colname = by_colname)
    
    ##  not finished ... work in progress
    
# need to assemble after loop
    
  }
  return(
    results_thisdenomtype  
  )
  
}
############################################################# # 

# then to use that, might do something like this:

#
# results_bysite_wtdmeans <- calc_wtdmeans(
#   sites2bgs_plusblockgroupdata_bysite,
#   score_colnames = wtdmeancols_inbgstat,
#   wts_denom_colnames = map_headernames$denominator[map_headernames$rname == wtdmeancols_inbgstats],
#   wts_bg_colname = "bgwt",
#   by_colname = "ejam_uniq_id"
# )
# # 
