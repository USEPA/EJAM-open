# obtain shapefile to be used to see Which state contains each site 

  # help("states_shapefile")

# stop("must be in local source package EJAM/data-raw folder when running this script")
# setwd("~/../../R/mysource/EJAM/data-raw")

td = tempdir()
td = file.path(td, "shp")
if (!dir.exists(td)) {dir.create(td)}
if (!dir.exists(td)) {stop('failed to create temp subfolder /shp')}

# browseURL("https://www2.census.gov/geo/tiger/Directory_Contents_ReadMe.pdf")

# baseurl = "https://www2.census.gov/geo/tiger/TIGER2020/STATE/tl_2020_us_state.zip"
baseurl = "https://www2.census.gov/geo/tiger/TIGER2022/STATE/tl_2022_us_state.zip"
# baseurl = "https://www2.census.gov/geo/tiger/TIGER2023/STATE/tl_2023_us_state.zip"

zname = basename(baseurl)

curl::curl_download(
  url = baseurl, 
  destfile = file.path(td, zname),
  quiet = FALSE
)
# download.file(
#   url = baseurl, 
#   destfile = file.path(td, zname),
# )

if (!file.exists(file.path(td, zname))) {stop('tried to download but cannot find ', file.path(td, zname))}

################################################################### # 

fnames = unzip(
       zipfile = file.path(td, zname),
       exdir = td, list = TRUE
   )$Name
shpname = paste0(unique(gsub("\\..*$", "", fnames)), ".shp")
if (length(shpname) > 1) {stop("unclear which shapefile to import")}

unzip(
  zipfile = file.path(td, zname),
  exdir = td
  )
if (!file.exists(file.path(td, zname))) {stop('tried to download but cannot find ', file.path(td, zname))}

states_shapefile <- sf::st_read(td)

# add the dataset to this package as a dataset to be installed with the package and lazy loaded when needed

attr(states_shapefile, "source_url")       <- baseurl
attr(states_shapefile, "date_downloaded")       <- as.character(Sys.Date())
attr(states_shapefile, "date_saved_in_package") <- as.character(Sys.Date())

print(attributes(states_shapefile))
cat("saving in package\n")
usethis::use_data(states_shapefile, overwrite = TRUE)

dataset_documenter("states_shapefile", 
                   title = "This is used to figure out which state contains each point (facility/site).",
                   seealso = "seealso [state_from_latlon()] [get_blockpoints_in_shape()]",
                   description = "This is used to figure out which state contains each point (facility/site).",
                   details = "This is used by [state_from_latlon()] to find which state is associated with each point 
#'   that the user wants to analyze. That is needed to report indicators in 
#'   the form of State-specific percentiles 
#'   (e.g., a score that is at the 80th percentile within Texas).
#'   It is created by the package via a script at EJAM/data-raw/datacreate_states_shapefile.R
#'   which downloads the data from Census Bureau.")

################################################################### # 
## alternative way, from EJSCREENbatch   (format differs)
# library # ( # tigris)
# library(sf)
# states_shapefile2 <- tigris # :: # states() %>% sf::st_as_sf() %>%
#   sf::st_transform(crs ="ESRI:102005") %>%
#   dplyr::select('NAME') %>%
#   dplyr::rename(facility_state = NAME)
# #    facility_buff =  Polygons representing buffered areas of interest

################################################################### # 
#  methods for downloading other
# TIGER/Line Shapefiles 
# from the U.S. Census Bureau.

# ** Website Interface
# https://www.census.gov/cgi-bin/geo/shapefiles/index.php  
# allows download of only 1 state at a time if block resolution
# •	For detailed instructions, please see the educational brochure on Downloading TIGER/Line Shapefiles.
# https://www2.census.gov/geo/pdfs/education/tiger/Downloading_TIGERLine_Shp.pdf
# •	Note: Not all versions of TIGER/Line Shapefiles are available through the web interface. 

# ** Direct from FTP site (or via FTP client) to access the full set of files.
# ftp://ftp2.census.gov/geo/tiger/

# ** Direct from Data.gov 
# Census haf not yet added the ability download Shapefiles directly on data.census.gov, (as of 5/2023)
