## code to prepare `default_points_shown_at_startup` dataset

default_points_shown_at_startup <- structure(
  list(
    lat = c(33.70369, 33.6611), 
    lon = c(-117.86675, -117.86959), 
    REGISTRY_ID = c("110071293460", "110070874073"), 
    PRIMARY_NAME = c("EJAM INC", "EJAM INC."), 
    NAICS = c("", ""), 
    PGM_SYS_ACRNMS = c("ICIS:3601439158", "ICIS:3601252181")
  ),
  row.names = c(NA, -2L), 
  class = "data.frame", 
  download_date = "2023-03-25", 
  released = structure(19441, class = "Date"))

usethis::use_data(default_points_shown_at_startup, overwrite = TRUE)

dataset_documenter("default_points_shown_at_startup",
  title = "Default Points Shown at Startup\n#' keywords internal"
)
