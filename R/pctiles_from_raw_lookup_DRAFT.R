

# # drafting some code to try to replace percentiles lookup that is slow in doaggregate()

# but note pctile_from_raw_lookup() now can also handle 1 zone multiple indicators at once, so that should be built in here too if this approach is pursued.

# # assumes we have
# include_ejindexes  - logical parameter
# results_bysite, results_overall  - large data.tables as parameters or by reference?
# pctile_from_raw_lookup()  - old slow function for doing this on just 1 indicator (vector of sites), but any zones.
# usastats, statestats   - largish lookup tables as either data.frame (old way) or data.table (may switch to this for speed)
# c(names_e,  names_d, names_d_subgroups), c(names_ej, names_ej_state, names_ej_supp, names_ej_supp_state)  - lists of indicator names that must be /should be in colnames of usastats, statestats, and 

# statestats has some NA values and some missing columns:
# 
# pm and ozone had raw and EJ eo and EJ supp indicators in AK, HI, PR all NA values
# also, NPDES all 3 vars all NA in AK only.
# also, lowlifex all NA in PR only. 
# also, no Island Areas here
# also, no d_subgroups
#                                   var  AK  AL  AR  AZ  CA  CO  CT  DC  DE  FL  GA  HI  IA  ID  IL  IN  KS  KY  LA  MA  MD  ME  MI  MN  MO  MS  MT  NC  ND  NE  NH  NJ  NM  NV  NY  OH  OK  OR  PA  PR  RI  SC  SD  TN  TX  UT  VA  VT  WA  WI  WV  WY
# 14                                pm   0 102 102 102 102 102 102 102 102 102 102   0 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102   0 102 102 102 102 102 102 102 102 102 102 102 102
# 15                                o3   0 102 102 102 102 102 102 102 102 102 102   0 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102   0 102 102 102 102 102 102 102 102 102 102 102 102
# 26                   proximity.npdes   0 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102
# 27                EJ.DISPARITY.pm.eo   0 102 102 102 102 102 102 102 102 102 102   0 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102   0 102 102 102 102 102 102 102 102 102 102 102 102
# 28              EJ.DISPARITY.pm.supp   0 102 102 102 102 102 102 102 102 102 102   0 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102   0 102 102 102 102 102 102 102 102 102 102 102 102
# 29                EJ.DISPARITY.o3.eo   0 102 102 102 102 102 102 102 102 102 102   0 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102   0 102 102 102 102 102 102 102 102 102 102 102 102
# 30              EJ.DISPARITY.o3.supp   0 102 102 102 102 102 102 102 102 102 102   0 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102 102   0 102 102 102 102 102 102 102 102 102 102 102 102

if (1 == 0) {

##################################################### #
## PERCENTILES - express raw scores (from results_bysite AND  results_overall) in percentile terms #### 
#  VIA  lookup tables of US/State  percentiles, called EJAM::usastats   and statestats
#  note: usastats is  like ejscreen package file lookupUSA , and EJAM::pctile_from_raw_lookup is like ejanalysis package file lookup.pctile()
#
#  *** this should be extracted as a function (but keeping the efficiency of data.table changes by reference using := or set___)
#      so that it can be used again later to assign percentiles to EJ indexes once they are calculated from other scores and percentiles.
#
##################################################### #
# these lines about names of variables should be pulled out of here and defined as params or another way   xxx
# specify which variables get converted to percentile form
  # ONLY IF THESE ARE ALL IN LOOKUP TABLES AND blockgroupstats?
varsneedpctiles <- c(names_e,  names_d, names_d_subgroups   ) # older line said: # varsneedpctiles <- intersect(varsneedpctiles, names(blockgroupstats))
if (include_ejindexes) {
  varsneedpctiles <- c(varsneedpctiles, c(names_ej, names_ej_state, names_ej_supp, names_ej_supp_state))
  # ** assuming RAW EJ Indexes were aggregated as popwtd means, then convert them here to report as percentiles
  # (even though previously had thought they would be calculated by formula for each buffer so thought could not do PERCENTILES yet for them.)
}

varnames.us.pctile    <- paste0(      'pctile.', varsneedpctiles)
varnames.state.pctile <- paste0('state.pctile.', varsneedpctiles)
# set up empty tables to store the percentiles we find
us.pctile.cols_bysite     <- data.frame(matrix(nrow = NROW(results_bysite),  ncol = length(varsneedpctiles))); colnames(us.pctile.cols_bysite)     <- varnames.us.pctile
state.pctile.cols_bysite  <- data.frame(matrix(nrow = NROW(results_bysite),  ncol = length(varsneedpctiles))); colnames(state.pctile.cols_bysite)  <- varnames.state.pctile
us.pctile.cols_overall    <- data.frame(matrix(nrow = NROW(results_overall), ncol = length(varsneedpctiles))); colnames(us.pctile.cols_overall)    <- varnames.us.pctile
# done later: state.pctile.cols_overall <- data.frame(matrix(nrow = NROW(results_overall), ncol = length(varsneedpctiles))); colnames(state.pctile.cols_overall) <- varnames.state.pctile

# SURELY THERE IS A FASTER / VECTORIZED WAY TO DO THIS (this actually is noticeably slow, at least the line that starts with state.pctile.cols_bysite[ , varnames.state.pctile[[i]]] <- pctile_from_raw_lookup( ):
#  >>>> VERY SLOW STEP; also the function  pctile_from_raw_lookup()  may need to be optimized or avoid passing dt as param to it. ####

for (i in seq_along(varsneedpctiles)) {
  
  myvar <- varsneedpctiles[i]
  
  # USA ########################## # 
  
  if ((myvar %in% names(usastats)) & (myvar %in% names(results_bysite)) & (myvar %in% names(results_overall))) {  
    # use this function to look in the lookup table to find the percentile that corresponds to each raw score value:
    
    us.pctile.cols_bysite[    , varnames.us.pctile[[i]]]    <- pctile_from_raw_lookup(
      unlist(results_bysite[  , ..myvar]), varname.in.lookup.table = myvar, lookup = usastats) 
    us.pctile.cols_overall[   , varnames.us.pctile[[i]]]    <- pctile_from_raw_lookup(
      unlist(results_overall[ , ..myvar]), varname.in.lookup.table = myvar, lookup = usastats) 
    # (note it is a bit hard to explain using an average of state percentiles   in the "overall" summary)
    
  } else { # cannot find that variable in the percentiles lookup table
    
    us.pctile.cols_bysite[    , varnames.us.pctile[[i]]] <- NA
    us.pctile.cols_overall[   , varnames.us.pctile[[i]]] <- NA
  }
  
  # STATES ########################## # 
  
  if ((myvar %in% names(statestats)) & (myvar %in% names(results_bysite)) ) {
    
    
    ### VERY SLOW STEP 289 msec
    
    state.pctile.cols_bysite[ , varnames.state.pctile[[i]]] <- pctile_from_raw_lookup(
      unlist(results_bysite[  , ..myvar]), 
      varname.in.lookup.table = myvar, 
      lookup = statestats, 
      zone =  results_bysite$ST
    )
    
    
    
    ## These must be done later, as avg of sites:
    # state.pctile.cols_overall[, varnames.state.pctile[[i]]] <- pctile_from_raw_lookup(unlist(results_overall[ , ..myvar]), varname.in.lookup.table = myvar, lookup = statestats, zone =  results_overall$ST)
    
  } else {
    
    state.pctile.cols_bysite[ , varnames.state.pctile[[i]]] <- NA
    
    # state.pctile.cols_overall[, varnames.state.pctile[[i]]] <- NA #  must be done later, as avg of sites
  }
  
  
} # end loop over indicators that need to be reported as percentiles


# Q: does this convert it from data.table to data.frame? I think not. xxx

results_overall <- cbind(ejam_uniq_id = NA, results_overall, us.pctile.cols_overall ) # , state.pctile.cols_overall)
results_bysite  <- cbind(                   results_bysite,  us.pctile.cols_bysite,  state.pctile.cols_bysite )

}

