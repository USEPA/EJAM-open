## code to prepare `lat_alias` and `lon_alias`  data constants

lat_alias <- c('lat', 'latitude83',  'latitude',  'latitudes',  'faclat',  'lats', "y")
lon_alias <- c('lon', 'longitude83', 'longitude', 'longitudes', 'faclong', 'lons', 'long', 'longs', 'lng', "x")

usethis::use_data(lat_alias, lon_alias, overwrite = TRUE)

dataset_documenter("lat_alias",
                   "lat_alias, lon_alias (DATA) Synonyms for lat and lon\n#' @aliases lon_alias",
                   description = "lists of synonyms for 'latitude' and 'longitude' used when guessing which column is what in user-provided tables of coordinates")

