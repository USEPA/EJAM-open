# x <- 
# "lat,lon,ST,limit,corner
# 12.9515582,144.21516,GU,min,SW
# 13.7783567,145.05135,GU,max,NE
# 14.0129859,145.067686,MP,min,SW
# 15.396985,145.9186,MP,max,NE
# 17.611087,-65.0974,VI,min,SW
# 18.45958,-64.52,VI,max,NE
# -15.811126,-173.48980,AS,min,SW
# -9.74635292,-168,AS,max,NE
# " 

islandareas_check <- structure(list(
  lat = c(12.951558,   13.7783567,
          14.0129859,  15.39699, 
          17.611087,   18.45958, 
          -15.811126,  -9.74635292), 
  lon = c(144.21516,  145.05135, 
          145.067686, 145.9186, 
          -65.0974,   -64.52,   
          -173.4898, -168), 
  ST    = c("GU",  "GU",  
            "MP",  "MP",  
            "VI",  "VI",  
            "AS",  "AS"),
  limit = c("min", "max",
            "min", "max", 
            "min", "max", 
            "min", "max"), 
  corner = c("SW", "NE",  
             "SW",  "NE",  
             "SW",  "NE",  
             "SW",  "NE")
), 
class = "data.frame", 
row.names = c(NA, -8L))


getwd()

islandareas <- as.data.frame(readr::read_csv("data-raw/datafile_islandareas.csv"))

all.equal(islandareas_check, islandareas_check)


writexl::write_xlsx(islandareas,    "./data-raw/datafile_islandareas.xlsx")

usethis::use_data(islandareas, overwrite = TRUE)

dataset_documenter("islandareas",
                   "islandareas (DATA) table, bounds info on lat lon of US Island Areas",
                   description = "data.frame of info on approximate lat lon bounding boxes around
#'   American Samoa, Guam, the 
#'   Commonwealth of the Northern Mariana Islands (Northern Mariana Islands),
#'   and the United States Virgin Islands.
#'   
#'   See also [stateinfo] and [stateinfo2]
#'   
#'   See [Census documentation](http://www.census.gov/geo/reference/gtc/gtc_island.html)
#'   
#'   See source package files datacreate_islandareas.R or EJAM/data-raw/islandareas.xlsx
#'   
#'   Note the US minor outlying islands are not in that list and are widely dispersed.
#'   They include Midway Islands, etc.")

