library(EJAM)
library(magrittr)
library(dplyr)
dim(blockgroupstats)

## note these are all read in as character vars
api_vars <- arrow::read_parquet('data-raw/EJScreen_addl_variables.arrow', as_data_frame = T)
api_vars <- api_vars %>% arrange(bgfips)

names(api_vars)
# [1] "bgfips"                   "RAW_CG_NOHINCPCT"         "S_CG_NOHINCPCT_AVG"       "S_CG_NOHINCPCT_PCTILE"   
# [5] "N_CG_NOHINCPCT_AVG"       "N_CG_NOHINCPCT_PCTILE"    "RAW_HI_HEARTDISEASE"      "S_HI_HEARTDISEASE_AVG"   
# [9] "S_HI_HEARTDISEASE_PCTILE" "N_HI_HEARTDISEASE_AVG"    "N_HI_HEARTDISEASE_PCTILE" "RAW_HI_ASTHMA"           
# [13] "S_HI_ASTHMA_AVG"          "S_HI_ASTHMA_PCTILE"       "N_HI_ASTHMA_AVG"          "N_HI_ASTHMA_PCTILE"      
# [17] "RAW_HI_CANCER"            "S_HI_CANCER_AVG"          "S_HI_CANCER_PCTILE"       "N_HI_CANCER_AVG"         
# [21] "N_HI_CANCER_PCTILE"       "RAW_CI_FLOOD30"           "S_CI_FLOOD30_AVG"         "S_CI_FLOOD30_PCTILE"     
# [25] "N_CI_FLOOD30_AVG"         "N_CI_FLOOD30_PCTILE"      "RAW_CI_FIRE30"            "S_CI_FIRE30_AVG"         
# [29] "S_CI_FIRE30_PCTILE"       "N_CI_FIRE30_AVG"          "N_CI_FIRE30_PCTILE"       "NUM_NPL"                 
# [33] "NUM_TSDF"                 "NUM_WATERDIS"             "NUM_AIRPOLL"              "NUM_BROWNFIELD"          
# [37] "NUM_TRI"                  "NUM_SCHOOL"               "NUM_HOSPITAL"             "NUM_CHURCH"              
# [41] "YESNO_TRIBAL"             "YESNO_CEJSTDIS"           "YESNO_IRADIS"             "YESNO_AIRNONATT"         
# [45] "YESNO_IMPWATERS"          "YESNO_HOUSEBURDEN"        "YESNO_TRANSDIS"           "YESNO_FOODDESERT"        
# [49] "P_ENGLISH"                "P_SPANISH"                "P_FRENCH"                 "P_RUS_POL_SLAV"          
# [53] "P_OTHER_IE"               "P_VIETNAMESE"             "P_OTHER_ASIAN"            "P_ARABIC"                
# [57] "P_OTHER"                  "P_NON_ENGLISH"            "RAW_CG_LIMITEDBBPCT"      "S_CG_LIMITEDBBPCT_AVG"   
# [61] "S_CG_LIMITEDBBPCT_PCTILE" "N_CG_LIMITEDBBPCT_AVG"    "N_CG_LIMITEDBBPCT_PCTILE"


## track variable names and lists they come from
api_vars_meta <- data.frame(apiname = names(api_vars))
api_vars_meta <- api_vars_meta %>% 
  dplyr::mutate(rname = fixcolnames(apiname, 'api','rname'),
         longname = fixcolnames(apiname, 'api','long'),
         varlist = fixcolnames(apiname, 'api','varlist'),
         vartype = fixcolnames(apiname,'api','vartype'),
         in_bgstats = rname %in% names(blockgroupstats))

## replace with rnames
names(api_vars) <- fixcolnames(names(api_vars), 'api','rname')

# [1] "bgfips"                            "pctnohealthinsurance"              "state.avg.pctnohealthinsurance"    "state.pctile.pctnohealthinsurance"
# [5] "avg.pctnohealthinsurance"          "pctile.pctnohealthinsurance"       "rateheartdisease"                  "state.avg.rateheartdisease"       
# [9] "state.pctile.rateheartdisease"     "avg.rateheartdisease"              "pctile.rateheartdisease"           "rateasthma"                       
# [13] "state.avg.rateasthma"              "state.pctile.rateasthma"           "avg.rateasthma"                    "pctile.rateasthma"                
# [17] "ratecancer"                        "state.avg.ratecancer"              "state.pctile.ratecancer"           "avg.ratecancer"                   
# [21] "pctile.ratecancer"                 "pctflood30"                        "state.avg.pctflood30"              "state.pctile.pctflood30"          
# [25] "avg.pctflood30"                    "pctile.pctflood30"                 "pctfire30"                         "state.avg.pctfire30"              
# [29] "state.pctile.pctfire30"            "avg.pctfire30"                     "pctile.pctfire30"                  "count.NPL"                        
# [33] "count.TSDF"                        "num_waterdis"                      "num_airpoll"                       "num_brownfield"                   
# [37] "num_tri"                           "num_school"                        "num_hospital"                      "num_church"                       
# [41] "yesno_tribal"                      "yesno_cejstdis"                    "yesno_iradis"                      "yesno_airnonatt"                  
# [45] "yesno_impwaters"                   "yesno_houseburden"                 "yesno_transdis"                    "yesno_fooddesert"                 
# [49] "pctlan_english"                    "pctlan_spanish"                    "pctlan_french"                     "pctlan_rus_pol_slav"              
# [53] "pctlan_other_ie"                   "pctlan_vietnamese"                 "pctlan_other_asian"                "pctlan_arabic"                    
# [57] "pctlan_other"                      "pctlan_nonenglish"                 "pctnobroadband"                    "state.avg.pctnobroadband"         
# [61] "state.pctile.pctnobroadband"       "avg.pctnobroadband"                "pctile.pctnobroadband"            

intersect(names(blockgroupstats), names(api_vars))
# [1] "bgfips"            "count.NPL"         "count.TSDF"        "pctlan_nonenglish" "pctlan_spanish"    "pctlan_other"      "pctnobroadband"  

## Note: need to check the followinbg things
### scaling of pct-based columns a
### convert "Yes"/"No" vars to 1/0
### how to handling "nan" values for count variables: looks like replace with 0?
### convert all variables to numeric

## find new raw variables, add to bgstats
new_raw_vars <- api_vars_meta %>% filter(vartype == 'raw', in_bgstats == F)


api_vars_clean <- api_vars %>% 
  ## select only the new raw variables
  select(bgfips, all_of(new_raw_vars$rname)) %>% 
  ## for yes-no vars, convert yes -> 1, no -> 0, and 'nan' to NA
  mutate(across(contains('yesno'), \(x){ifelse(x == 'Yes',1, ifelse(x == 'No',0, NA))})) %>% 
  ## for numeric variables, convert and make 'nan' NA 
  ## Note: if stats don't look right, these may need to become 0s instead of NAs
  mutate(across(!contains('yesno') & !bgfips, \(x){ifelse(x == 'nan',NA, as.numeric(x) )})) %>% 
  ## for percentage variables, scale down by factor of 100
  mutate(across(contains('pct'), \(x) { x/100}))

## bgstats now has 150 columns
blockgroupstats <- left_join(blockgroupstats, api_vars_clean, by='bgfips')

usethis::use_data(blockgroupstats, overwrite = TRUE)
