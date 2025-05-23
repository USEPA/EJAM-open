

recalculate_names_d_languageli <- function(dt) {
  
  dt[ , c(names_d_languageli) :=  lapply(.SD, FUN = function(x) {
    ifelse(lingiso == 0, 0, x / lingiso)
    }),
      .SDcols = names_d_languageli_count]
  
}

# some = sample(1:NROW(blockgroupstats), 200)
# 
# x = data.frame(blockgroupstats[some, c('pctspanish_li', 'spanish_li', 'hhlds')], 
#                calc = blockgroupstats$spanish_li[some] / blockgroupstats$hhlds[some])
# 
# plot(x$calc, x$pctspanish_li, xlab = 'calculated from counts', ylab = 'pctspanish_li in blockgroupstats' , col="red")
# abline(0,1)
# 
# 
# acs22[acs22$STCNTRBG == '060372077111', c( "PCT_HLI_SPANISH_LI" , 'HLI_SPANISH_LI', 'HSHOLDS')]
# acs22[acs22$STCNTRBG == '060372077111' , grep('span', names(acs22), ignore.case = T, value = T)]
# 
# 
# 
# # > fixcolnames('pctspanish_li', 'r', 'acs')
# # [1] "PCT_HLI_SPANISH_LI"
# # > fixcolnames('spanish_li', 'r', 'acs')
# # [1] "HLI_SPANISH_LI"
# # > fixcolnames('hhlds', 'r', 'acs')
# # [1] "HSHOLDS"
# # > fixcolnames('lingiso', 'r', 'acs')
# # [1] "LINGISO"
# # > rstudioapi::documentOpen("./data-raw/datacreate_formulas.R")
# # [1] "055AA0F3"
# # > fixcolnames('lan_universe', 'r', 'acs')
# # [1] "LAN_UNIVERSE"
# 
# 
# 
# This already works:
# PCT_LAN_SPANISH  is what comm report on ejscreen site calls "languages spoken at home"
# and is clearly the ratio of 
# LAN_SPANISH  / LAN_UNIVERSE 
# so that is already available as PCT_LAN_SPANISH aka  pctlan_spanish in EJAM
# or generally as 
# fixcolnames(names_d_language,'r','long')
# 
# This is in the dataset and we initially tried to use it but it is not the stat we actually want to show on the report:
# in the ACS table from the ejscreen team, 
# although the resulting percentage is NOT shown anywhere in the community report,
# this is clearly the formula they used for that dataset:
# PCT_HLI_SPANISH_LI = HLI_SPANISH_LI / HSHOLDS
# 
# What is actually shown on a community report from the ejscreen site:
# LIMITED ENGLISH SPEAKING BREAKDOWN  
# % speak Spanish (as % of lingiso hhlds) =  HLI_SPANISH_LI  /  LINGISO 
# should be called PCT_HLI_SPANISH_LI, but ACS22 used that variable name to mean something else
#   or essentially used the wrong formula for that variable (wrong denominator).
# 
