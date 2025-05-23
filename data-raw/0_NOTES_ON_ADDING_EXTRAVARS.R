########## # 
# "OTHER" INDICATORS WE WANT TO SEE IN OUTPUTS/RESULTS
# global_defaults_*.R define  default_extratable_list_of_sections

vars = as.vector(unlist(default_extratable_list_of_sections))

########## # 
# INDICATORS ALREADY BEING CALCULATED AS RESULTS

out = ejamit(testpoints_10, radius = 2)
varsout = names(out$results_overall)

########## # 
# INDICATORS MISSING -- NOT CALCULATED YET  - NEED FORMULA OR METADATA OR LISTS OF NAMES TO SHOW, UPDATES/FIXES

vars_missing = setdiff(vars, varsout) 

vars_missing
#  "p_chinese" "p_korean"
#   "count.NPL" "count.TSDF" "num_waterdis"  "num_airpoll" "num_brownfield" "num_tri"  
#   "num_school" "num_hospital"  "num_church"
# "yesno_tribal" "yesno_airnonatt" "yesno_impwaters" "yesno_cejstdis"   "yesno_iradis"
# "pctflood"  "pctfire"
# "yesno_houseburden"  "yesno_transdis"  "yesno_fooddesert"
# "pctnobroadband"  "pctnohealthinsurance"
# "nonmins"
# "count.ej.80up" "count.ej.80up.supp" 
# "count.ej.80up2.eo" "count.ej.80up2.supp" "state.count.ej.80up" "state.count.ej.80up.supp"

########## # 
# INDICATORS NOT IN DATASET blockgroupstats, so probably cannot calculate (with some exceptions)

vars_missing_fixable = intersect(vars_missing, names(blockgroupstats))
vars_missing_fixable

vars_missing_cannotfix = setdiff(vars_missing, names(blockgroupstats))
vars_missing_cannotfix

all.equal(
  sort(union(vars_missing_cannotfix, vars_missing_fixable)),
  sort(vars_missing)
) # TRUE


not_in_blockgroupstats <- c(
  'p_chinese', 'p_korean', 
  'pctflood', 'pctfire',
  'nonmins',
  
  # 'count.ej.80up',      # in blockgroupstats but typically in outputs of ejamit as out$results_summarized$cols$Number.of.EJ.US.or.ST.at.above.threshold.of.80
  # 'count.ej.80up.supp', # in blockgroupstats but typically in outputs of ejamit as out$results_summarized$cols$Number.of.Supp.US.or.ST.at.above.threshold.of.80
  'count.ej.80up2.eo', 
  'count.ej.80up2.supp',
  'state.count.ej.80up',
  'state.count.ej.80up.supp'
)
# out$results_summarized$cols$Number.of.EJ.US.or.ST.at.above.threshold.of.80
# out$results_summarized$cols$Number.of.Supp.US.or.ST.at.above.threshold.of.80

all.equal(sort(not_in_blockgroupstats) , sort(vars_missing_cannotfix))
# TRUE

#   not in blockgroupstats but calculated elsewhere, so they are in outputs:
c(
  'sitecount_avg',
  'sitecount_unique',
  'sitecount_max',
  'distance_min_avgperson',
  'distance_min'
)
########## # 

## see map_headernames metadata about the missing ones

checkon = function(varnames) {
  print(
    cbind(
      varlist = varinfo(varnames, "varlist"),
      rname = varnames,
      in.bgstats = varnames %in% names(blockgroupstats),
      in.outputs.already = varnames %in% varsout,
      missing.fixable = varnames %in% vars_missing_fixable,
      missing.unfixable = varnames %in% vars_missing_cannotfix,
      calctype = EJAM:::calctype(  varnames),
      wt       = EJAM:::calcweight(varnames)
    )
  )
}

## should remove these from the list of default_extratable_list_of_sections
## or at least ensure they are in  default_extratable_hide_missing_rows_for
## Could actually fix these but not critical: 

checkon(vars_missing_cannotfix)
 
#                                      varlist                    rname in.bgstats in.outputs.already missing.fixable missing.unfixable      calctype           wt
# p_chinese                   names_d_language                p_chinese      FALSE              FALSE           FALSE              TRUE       wtdmean lan_universe
# p_korean                    names_d_language                 p_korean      FALSE              FALSE           FALSE              TRUE       wtdmean lan_universe
# pctflood                       names_climate                 pctflood      FALSE              FALSE           FALSE              TRUE       wtdmean          pop
# pctfire                        names_climate                  pctfire      FALSE              FALSE           FALSE              TRUE       wtdmean          pop
# nonmins                  names_d_other_count                  nonmins      FALSE              FALSE           FALSE              TRUE sum of counts             
# count.ej.80up2.eo           names_countabove        count.ej.80up2.eo      FALSE              FALSE           FALSE              TRUE    countabove             
# count.ej.80up2.supp         names_countabove      count.ej.80up2.supp      FALSE              FALSE           FALSE              TRUE    countabove             
# state.count.ej.80up         names_countabove      state.count.ej.80up      FALSE              FALSE           FALSE              TRUE    countabove             
# state.count.ej.80up.supp    names_countabove state.count.ej.80up.supp      FALSE              FALSE           FALSE              TRUE    countabove            

checkon(vars_missing_fixable)

#### for count.NPL, num_tri, etc., if the original data was count inside the blockgroup, 
####  then we just want sum over bgs, not count of unique ids !!

# count.NPL                names_sitesinarea            count.NPL       TRUE              FALSE            TRUE             FALSE count of unique ids   use sumbg if data was just sites inside a bg not all nearby
# count.TSDF               names_sitesinarea           count.TSDF       TRUE              FALSE            TRUE             FALSE count of unique ids   "
# num_waterdis             names_sitesinarea         num_waterdis       TRUE              FALSE            TRUE             FALSE count of unique ids   "
# num_airpoll              names_sitesinarea          num_airpoll       TRUE              FALSE            TRUE             FALSE count of unique ids   "
# num_brownfield           names_sitesinarea       num_brownfield       TRUE              FALSE            TRUE             FALSE count of unique ids   "
# num_tri                  names_sitesinarea              num_tri       TRUE              FALSE            TRUE             FALSE count of unique ids   "
# num_school            names_featuresinarea           num_school       TRUE              FALSE            TRUE             FALSE count of unique ids   "
# num_hospital          names_featuresinarea         num_hospital       TRUE              FALSE            TRUE             FALSE count of unique ids   "
# num_church            names_featuresinarea           num_church       TRUE              FALSE            TRUE             FALSE count of unique ids   "

# yesno_tribal                    names_flag         yesno_tribal       TRUE              FALSE            TRUE             FALSE                flag  use maxbg to return "1" if any bg was "1"
# yesno_airnonatt                 names_flag      yesno_airnonatt       TRUE              FALSE            TRUE             FALSE                flag   "
# yesno_impwaters                 names_flag      yesno_impwaters       TRUE              FALSE            TRUE             FALSE                flag   "
# yesno_cejstdis                  names_flag       yesno_cejstdis       TRUE              FALSE            TRUE             FALSE                flag   "
# yesno_iradis                    names_flag         yesno_iradis       TRUE              FALSE            TRUE             FALSE                flag   "
# yesno_houseburden    names_criticalservice    yesno_houseburden       TRUE              FALSE            TRUE             FALSE                flag   "
# yesno_transdis       names_criticalservice       yesno_transdis       TRUE              FALSE            TRUE             FALSE                flag   "
# yesno_fooddesert     names_criticalservice     yesno_fooddesert       TRUE              FALSE            TRUE             FALSE                flag   "
# pctnobroadband       names_criticalservice       pctnobroadband       TRUE              FALSE            TRUE             FALSE       sum of counts -WRONG - FIX  
# pctnohealthinsurance names_criticalservice pctnohealthinsurance       TRUE              FALSE            TRUE             FALSE       sum of counts WRONG - FIX
# count.ej.80up             names_countabove        count.ej.80up       TRUE              FALSE            TRUE             FALSE          countabove   
# count.ej.80up.supp        names_countabove   count.ej.80up.supp       TRUE              FALSE            TRUE             FALSE          countabove  


