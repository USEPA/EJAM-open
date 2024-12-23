
#########   ONLY NEEDS TO BE DONE AFTER blockpoints or blockwts changes
## i.e., if there are new FIPS codes or boundaries for any block groups in US


#########     THIS IS ONLY A DRAFT -- NEEDS CLEANUP BEFORE USED AGAIN

library(data.table)
warning("see notes in `./data-raw/datacreate_bg_cenpop2020.R`-- bgpts and bg_cenpop2020 are very, very similar, so may want to consolidate to use only one.")



# help(bgpts, package = "EJAM")

# Start from blockpoints (Census 2020 block internal points) ####
# blockpoints and blockwts are data for EJAM package

bgpts_blocks <- copy(blockpoints)
# not essential but ok to make sure we do not change blockpoints itself by reference in data.table operations
# all.equal(bgpts$blockid , blockwts$blockid)
bgpts_blocks[ , bgid    := blockwts$bgid]
bgpts_blocks[ , blockwt := blockwts$blockwt]

# Find popwtd centroid lat lon in each block group ####
## but note weight is zero sometimes.
# > sum(bgpts_blocks$blockwt == 0)
# [1] 2368443

bgpts <- bgpts_blocks[ , lapply(.SD, FUN = function(x) {
  stats::weighted.mean(x, w = blockwt, na.rm = TRUE)
}),  .SDcols = c('lat', 'lon'),  by = 'bgid']
emptybgids = bgpts$bgid[is.na(bgpts$lat)]
bgpts[bgid %in% emptybgids, ]  <- bgpts_blocks[bgid %in% emptybgids, lapply(.SD, mean),
                                               .SDcols = c('lat', 'lon'),  by = 'bgid']
rm(bgpts_blocks)

## add bgfips column, so it has bgfips, bgid, lat, lon
stopifnot( all.equal(bgpts$bgid, bgid2fips$bgid) )
bgpts[ , bgfips := bgid2fips$bgfips]

############################################# # 
#  THE  v2.32 block table from EJScreen team with weights already had PR at least,

## bgid2fips has only 50 states plus DC and PR, but not AS GU MP VI, which is fine:
length(fips2state_abbrev(rownames(table(substr(bgid2fips$bgfips,1,2)))))
## AS GU MP VI were in blockgroupstats but NOT in bgpts! will drop from blockgroupstats. 
# unique(blockgroupstats[, .(statename, island = is.island(ST), bgcount = .N), by = "ST"])
## ST abbrev is in blockgroupstats and will not be stored in bgpts or bgid2fips etc.
# blockid2fips[, ST := fips2state_abbrev(substr(blockfips,1,2))][,.N,by = "ST"]
# bgid2fips[, ST := fips2state_abbrev(substr(bgfips,1,2))][,.N,by = "ST"]

# stcounts = table(fips2state_abbrev(substr(bgpts$bgfips,1,2)))
# names(stcounts)
## [1] "AK" "AL" "AR" "AZ" "CA" "CO" "CT"   "DC"   "DE" "FL" "GA" "HI" "IA" "ID" "IL" "IN" "KS" "KY" "LA"
## [20] "MA" "MD" "ME" "MI" "MN" "MO" "MS" "MT" "NC" "ND" "NE" "NH" "NJ" "NM" "NV" "NY" "OH" "OK" "OR"
## [39] "PA"   "PR"   "RI" "SC" "SD" "TN" "TX" "UT" "VA" "VT" "WA" "WI" "WV" "WY"
# setdiff(names(stcounts), state.abb)
## [1] "DC" "PR"
# 
## if using downloaded file though,
## PR, but not "AS" "GU" "MP" "VI", were found in downloaded census2020 block table ***

############################################# # 

## view those block group points on a map (plot only a subset which is enough)
# sam <- sample(seq_along(bgpts$bgid),5000)
# plot(x = bgpts$lon[sam], y = bgpts$lat[sam], pch = '.')
# 
## view one state, florida, where 12 are the 1st 2 digits of the FIPS:
# bgpts[bgid2fips[substr(bgfips,1,2) == '12', ], on = 'bgid']
# xx <- '12'
# mystate <- bgpts[bgid2fips[substr(bgfips, 1, 2) == xx, ], on = 'bgid'][ , .(lon, lat)]
# plot(mystate, pch = '.')
# rm(mystate, xx)

############################################# # 
## How blockcounts can be done:

## need  data.table pkg loaded

bg_blockcounts2 <- blockwts[ , .(blockcount = .N), by = bgid]
bgpts[ , blockcount := bg_blockcounts2$blockcount]

  # sum(bg_blockcounts2$blockcount == 1)
## [1] 1874 blockgroups have only 1 block
# sum(bg_blockcounts2$blockcount == 1000) # the max is 1000 blocks in a bg
# # [1] 22
  # round(100 * table(bg_blockcounts2[blockcount < 20, blockcount]) / nrow(bg_blockcounts2), 1)
## about 1 to 3 % of all bg contain 10 blocks, 5 blocks, 2 blocks, etc:
##   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19
## 0.8 1.2 1.3 1.4 1.5 2.1 2.2 2.4 2.6 2.8 2.8 3.0 3.0 2.9 3.0 2.9 2.8 2.7 2.5
 all.equal(bgpts$bgid, bg_blockcounts2$bgid)
rm(bg_blockcounts2)
# dim(bgpts)
# 242355    5
############################################# # 

# add metadata ####
# EJAM :::  metadata_add(), 
# latest source version of this internal function is available after devtools::load_all()

bgpts <- EJAM:::metadata_add(bgpts)

############################################# # 

# save for EJAM package ####

usethis::use_data(bgpts, overwrite = TRUE)

# documentation ####
dataset_documenter('bgpts', 
  title = "bgpts (DATA) lat lon of popwtd center of blockgroup, and count of blocks per block group",
  description = "This is just a list of US block groups and how many blocks are in each. It also has the lat and lon roughly of each blockgroup", 
  details = 'The point used for each bg is the Census 2020 population weighted mean of the internal points of the blocks. It gives an approximation of where people live and where each bg is, which is useful for some situations. Has all US States, DC, PR, but not  "AS" "GU" "MP" "VI" (and not U.S. Minor Outlying Islands FIPS 74 UM)', 
)

############################################# # 
