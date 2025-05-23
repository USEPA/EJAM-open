
# UPDATE ALL METADATA AND RE-SAVE DATASETS

#   normally done while updating the data but this could retroactively update metadata for all datasets 

############################################################ #

# update all metadata for datasets in EJAM/data/

x = datapack('EJAM')

stop('to be continued - NOT TESTED/TRIED OUT YET JUST DRAFTED')

for (i in 1:length(x$Item))  {
  
  val = metadata_add(
    get(x$Item[i])
  )
  assign(x = x$Item[i], value = val)
  
  usethis::use_data(
    get(x$Item[i]),
    overwrite = TRUE
  )
  
}
rm(i)
############################################################ #

# update all metadata for datasets, and resave to ??? 

x = download_dynamic('all')

stop('to be continued - NOT TESTED/TRIED OUT YET JUST DRAFTED')

for (i in 1:length(x$Item))  {
 
  val = metadata_add(
    get(x$Item[i])
  )
  assign(x = x$Item[i], value = val)
}


### this may be obsolete if not using pins now: see vignettes on how to update datasets
  # EJAM:::datawrite_to_pins('all')


rm(i)
############################################################ #
