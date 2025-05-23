## code to prepare `ejscreenRESTbroker2table_na_filler` dataset


ejscreenRESTbroker2table_na_filler <- ejscreenRESTbroker2table(
  ejscreenRESTbroker(lon = -90, lat = 40, radius = 0.5))

ejscreenRESTbroker2table_na_filler[1,] <- NA

ejscreenRESTbroker2table_na_filler <- metadata_add(ejscreenRESTbroker2table_na_filler)

usethis::use_data(ejscreenRESTbroker2table_na_filler, overwrite = TRUE)

#document it
dataset_documenter("ejscreenRESTbroker2table_na_filler",
  "used to fill in NA values in correctly-formatted output of [ejscreenRESTbroker2table()] when no data are available"
)

# dput(ejscreenRESTbroker2table_na_filler)

