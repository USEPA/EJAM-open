

datacreate_names_pct_as_fraction <- function(map_headernames)   {
  
  # datacreate_names_pct_as_fraction.R
  
  ## this should happen after map_headernames is recreated if necessary
  
  # map_headernames$pct_as_fraction_blockgroupstats is TRUE when  map_headernames$rname %in% names_pct_as_fraction_blockgroupstats
  # map_headernames$pct_as_fraction_ejamit          is TRUE when  map_headernames$rname %in% names_pct_as_fraction_ejamit
  # map_headernames$pct_as_fraction_ejscreenit      is TRUE when  map_headernames$rname %in% names_pct_as_fraction_ejscreenit
  
  names_pct_as_fraction_ejamit          <- unique(map_headernames$rname[map_headernames$pct_as_fraction_ejamit])
  names_pct_as_fraction_ejscreenit      <- unique(map_headernames$rname[map_headernames$pct_as_fraction_ejscreenit])
  names_pct_as_fraction_blockgroupstats <- unique(map_headernames$rname[map_headernames$pct_as_fraction_blockgroupstats])
  names_pct_as_fraction_blockgroupstats <- unique(names_pct_as_fraction_blockgroupstats[names_pct_as_fraction_blockgroupstats %in% names(blockgroupstats)])
  
  
  names_pct_as_fraction_blockgroupstats <- metadata_add(names_pct_as_fraction_blockgroupstats)
  names_pct_as_fraction_ejscreenit      <- metadata_add(names_pct_as_fraction_ejscreenit)
  names_pct_as_fraction_ejamit          <- metadata_add(names_pct_as_fraction_ejamit)
  
  usethis::use_data(names_pct_as_fraction_blockgroupstats, overwrite = T)
  usethis::use_data(names_pct_as_fraction_ejscreenit,      overwrite = T)
  usethis::use_data(names_pct_as_fraction_ejamit,          overwrite = T)
  
  ## Documentation ####
  
  dataset_documenter("names_pct_as_fraction_blockgroupstats",
                     title = "which indicators are percentages stored as 0-1 not 0-100, in blockgroupstats\n#' @keywords internal")
  dataset_documenter("names_pct_as_fraction_ejscreenit",
                     title = "which indicators are percentages stored as 0-1 not 0-100, in blockgroupstats\n#' @keywords internal")
  dataset_documenter("names_pct_as_fraction_ejamit",
                     title = "which indicators are percentages stored as 0-1 not 0-100, in blockgroupstats\n#' @keywords internal")
  
  # return them just in case that is useful, but this is really to save them as datasets and document them
  return(list(
    names_pct_as_fraction_blockgroupstats = names_pct_as_fraction_blockgroupstats,
    names_pct_as_fraction_ejscreenit = names_pct_as_fraction_ejscreenit, 
    names_pct_as_fraction_ejamit = names_pct_as_fraction_ejamit
  ))
}
################################################## # 


# USE THE FUNCTION ####

datacreate_names_pct_as_fraction(map_headernames = map_headernames)   # Does metadata_add and use_data
print("remember to use document() to use the updated .R files to make new .Rd files")
rm("datacreate_names_pct_as_fraction")






################################################## # 
## how those were found in ejscreenit() outputs
### x = ejscreenit(testpoints_5)$table
# x = testoutput_ejscreenit_500$table
# x = cbind(summary(x)[6,])
# x = data.frame(rownames(x), x)
# rownames(x) <- NULL
# x = x[order(x[,2]), ]
# head(x, 30)
### 116       US average for Lead Paint Indicator (% pre-1960s housing)       Max.   :0.3  
### 44     State average for Lead Paint Indicator (% pre-1960s housing)    Max.   :0.5500  
### 21                       Lead Paint Indicator (% pre-1960s housing)   Max.   :0.90000  
###
###    pctpre1960, state.avg.pctpre1960, avg.pctpre1960
################################################## # 
