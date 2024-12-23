# datacreate_censusplaces

############################ # ############################ # ############################ # ############################ # 

# READ FILE FROM CENSUS.GOV ####

# also see 
# https://www2.census.gov/geo/pdfs/reference/GARM/Ch9GARM.pdf
# https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2023/TGRSHP2023_TechDoc_Ch3.pdf 
# https://github.com/walkerke/tigris?tab=readme-ov-file#readme
# https://walker-data.com/census-r/census-geographic-data-and-applications-in-r.html#tigris-workflows
# tigris only returns feature geometries for US Census data which default to the coordinate reference system NAD 1983 (EPSG: 4269). For US Census demographic data (optionally pre-joined to tigris geometries), try the tidycensus package. For help deciding on an appropriate coordinate reference system for your project, take a look at the crsuggest package.

if (!exists("setnames")) {stop("data.table package needs to be loaded, via library(data.table) or by attaching the EJAM package ")}

myurl <- "https://www2.census.gov/geo/docs/reference/codes/PLACElist.txt"
censusplaces <- data.table::fread(myurl, sep = "|", header = TRUE, colClasses = list(character = 1:7))
# censusplaces
# STATE|STATEFP|PLACEFP|PLACENAME|TYPE|FUNCSTAT|COUNTY
# AL|01|00100|Abanda CDP|Census Designated Place|S|Chambers County
############################# #

# ADD/DROP COLUMNS ####

censusplaces[ , FUNCSTAT := NULL]

censusplaces[ , fips := paste0(STATEFP, PLACEFP)] # adds 2 MB
censusplaces[ , PLACEFP := NULL]  # saves 2MB
setnames(censusplaces, old = "STATEFP", new = "stfips")
############################# #

### Handle ENCODING characters like accents

#"Do\xf1a Ana County" "NM"  ***   ISO-8859-1  ??  stringi::stri_enc_detect(censusplaces$COUNTY)
x = stringi::stri_enc_detect(censusplaces$COUNTY)
y = lapply(x, FUN = function(z) z$Encoding[1])
z = table(unlist(y))
### z
### IBM420_ltr ISO-8859-1 ISO-8859-2 ISO-8859-9   UTF-16BE      UTF-8 
###          1      39401       1758         35          8        211 
bestguess = names(which.max(z)) # bestguess =  "ISO-8859-1"
cat("There are", length(which(censusplaces$COUNTY !=  stringi::stri_enc_toascii(censusplaces$COUNTY))), "places where encoding is an issue. 
    Converting from what appears to be", bestguess,"\n")
censusplaces$PLACE  <-  stringi::stri_encode(censusplaces$PLACE,  bestguess, to = '') # convert to default
censusplaces$COUNTY <-  stringi::stri_encode(censusplaces$COUNTY, bestguess, to = '') # convert to default
# left PLACENAME unchanged so it no longer is same as PLACE in those locations that used accents, etc.
# print( censusplaces[(censusplaces$PLACE != censusplaces$PLACENAME), c('PLACENAME', 'PLACE', 'COUNTY', 'STATE' ) ][order(PLACE,STATE), ] )
censusplaces$PLACENAME <- NULL
############################ #

#  TYPE OF PLACE  - drop County Subdivision if redundant with Incorporated Place or Census Designated Place ####

# None of these places are Counties - they are all cities, towns, etc.
# sum(censusplaces$fips %in% substr(blockgroupstats$bgfips,1,5))
cbind(TYPE = table(censusplaces$TYPE))
#                          TYPE
# Census Designated Place  9974
# County Subdivision      11900
# Incorporated Place      19540

# 3,219 PLACE ARE LISTED TWICE, where TYPE "Incorporated Place" vs another type is the only way to distinguish them:
## 3/4 are in PA, WI, MN 
# sum(duplicated(censusplaces[,c(1:3,5:6)]))

#      STATE stfips               PLACENAME                    TYPE                            COUNTY    fips                   PLACE
# 
#  9:     NY      36             Geneva city      County Subdivision     Ontario County, Seneca County 3628640             Geneva city
# 10:     NY      36             Geneva city      Incorporated Place     Ontario County, Seneca County 3628640             Geneva city

## They have the same FIPS so we have no way to distinguish them except by TYPE so we will drop the County Subdivision ones

fips_multitype <- censusplaces$fips[duplicated(censusplaces[, .(STATE, stfips, COUNTY, fips, PLACE)])]
candrop <- censusplaces$TYPE == "County Subdivision" & censusplaces$fips %in% fips_multitype 
censusplaces <- censusplaces[!candrop, ]

############################ #

# HANDLE PLACES STRADDLING 2+ COUNTIES  ####
#
# PLACES DO NOT ALWAYS FIT IN A SINGLE COUNTY LIKE TRACTS DO,
# so the FIPS here is NOT a way to get the countyfips of a place - there may not be a single one.
# censusplaces[12,]
# STATE STATEFP PLACEFP    PLACENAME               TYPE FUNCSTAT                       COUNTY
# 1:    AL      01   01660 Altoona town Incorporated Place        A Blount County, Etowah County
#
#  *** SAVE ONLY THE FIRST COUNTY LISTED FOR A PLACE, TO SIMPLIFY THIS...
#
# otherwise,
# to be able to filter all places/cities to view and select just those within at least partially within 
#  a select list of counties, you would need to make longer the censusplaces table to have separate rows for all county-place pairs !
# seems like more work than it is worth - 
# You can just filter on states and select all relevant counties
# and maybe we just show each place as being within the first of the listed counties to simplify it??

# DELETE TEXT SHOWING ADDITIONAL COUNTIES BEYOND 1ST COUNTY PER PLACE

censusplaces$COUNTY <- gsub(",.*", "", censusplaces$COUNTY)

# RENAME 

setnames(censusplaces, "COUNTY", 'countyname')
setnames(censusplaces, "STATE", 'ST')
setnames(censusplaces, "TYPE", 'type')
setnames(censusplaces, "PLACE", 'placename')

# To get the fips of that one county:    SLOW SINCE THERE ARE 40K PLACES
censusplaces[ , countyfips := fips_counties_from_countyname(countyname, ST, exact = TRUE)]
censusplaces[ , countyfips := as.integer(countyfips)]

censusplaces[ , fips := as.integer(fips)]   # saves 3 MB 
censusplaces[ , eparegion := fips_st2eparegion(stfips)]
censusplaces <- censusplaces[!duplicated(censusplaces), ]


print(censusplaces[ , .N, by = "type"])
#                       TYPE      N
# 1: Census Designated Place  9,974
# 2:      Incorporated Place 19,540
# 3:      County Subdivision 11,900 but now 8681 
# censusplaces[ , type := as.factor(type)]
censusplaces[ , type := NULL]

setcolorder(censusplaces, neworder = c("eparegion", "ST", "stfips", "countyname", "countyfips", "placename", "fips"))
censusplaces <- censusplaces[!duplicated(censusplaces), ]
censusplaces <- censusplaces[order(censusplaces$ST, censusplaces$countyname, censusplaces$placename),]
rownames(censusplaces) <- NULL
gc()

print(head(censusplaces,20))

setDF(censusplaces)

# metadata ####
attr(censusplaces, "date_created") <- Sys.Date()
censusplaces <- metadata_add(censusplaces)

# use_data ####
usethis::use_data(censusplaces, overwrite = TRUE)

# documentation ####
dataset_documenter('censusplaces',
                   title = "censusplaces (DATA) Census FIPS and other basic info on roughly 40,000 cities/towns/places",
                   description = "Table of US cities and other Census Designated Places, Incorporated Places, and County Subdivisions",
                   details = paste0("from [https://www2.census.gov/geo/docs/reference/codes/PLACElist.txt](https://www2.census.gov/geo/docs/reference/codes/PLACElist.txt) 
#' Column names: ", paste0(colnames(censusplaces), collapse = ", "))
)

