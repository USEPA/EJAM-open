
# UPDATE ALL METADATA AND RE-SAVE DATASETS

#   normally done while updating the data but this could retroactively update metadata for all datasets

############################################################ #

#   see metadata_update_attr()

############################################################ #

# update ALL metadata for datasets in EJAM/data/

x = datapack('EJAM')

stop('to be continued - NOT TESTED/TRIED OUT YET JUST DRAFTED')

update_date_saved_in_package = FALSE

for (i in 1:length(x$Item))  {

  val = metadata_add(
    get(x$Item[i]),
    update_date_saved_in_package = update_date_saved_in_package
  )
  assign(x = x$Item[i], value = val)

  src = paste0(
    "usethis::use_data(",
    x$Item[i],
    ", overwrite = TRUE)"
    )
   print(src)
   # eval(parse(text = src))
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
