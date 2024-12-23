# script to download and clean tables of facilities by MACT subpart

# requires 1st updating 
#   frs_by_programid

# creates  frs_by_mact
# creates  mact_table
# creates  epa_programs



stop("THIS IS NOT WORKING YET/ANYMORE - THE CODE WAS OUT OF ORDER SO THE SCRIPT DOES NOT WORK -
     WAS IT REPLACED BY frs_update_datasets() ??")
# check if rollback to main/earlier version would work- 
# seems like it may have worked and then was edited to update those datasets using the already existing versions of some files
# by using EJAM ::  xyzdataset  etc.


###################################################### #
# ~ ####

# CREATE y aka "frs_by_mact"  ####

################################# #
## DOWNLOAD DATA ####
#
# One can download from ECHO all the 14,000 or so CAA Major Actives  
# https://echo.epa.gov/tools/data-downloads
# and then remove the ones without  "MACT" listed in the column called AIRPrograms
# which are the one that have nothing in the column called  AIRMacts.
#  AIRMacts  column is a csv list of codes like DDDDD, EEEE, FFFF, HHHHH, JJJJ, KK, N, UU, ZZZZ
#  that apply to the given facility.
# https://echo.epa.gov/tools/data-downloads/icis-air-download-summary 
# ICIS-Air Datasets

td = tempdir()
zipname = "https://echo.epa.gov/files/echodownloads/ICIS-AIR_downloads.zip"
download.file(
  zipname, 
  destfile = file.path(td, "ICIS-AIR_downloads.zip")) # about 60 MB
unzip(       file.path(td, "ICIS-AIR_downloads.zip"), exdir = file.path(td, "icis"))
dir(file.path(td, "icis"), pattern = "ICIS")
# [1] "ICIS-AIR_FACILITIES.csv"        "ICIS-AIR_FCES_PCES.csv"         "ICIS-AIR_FORMAL_ACTIONS.csv"   
# [4] "ICIS-AIR_INFORMAL_ACTIONS.csv"  "ICIS-AIR_POLLUTANTS.csv"        "ICIS-AIR_PROGRAM_SUBPARTS.csv" 
# [7] "ICIS-AIR_PROGRAMS.csv"          "ICIS-AIR_STACK_TESTS.csv"       "ICIS-AIR_TITLEV_CERTS.csv"     
# [10] "ICIS-AIR_VIOLATION_HISTORY.csv"
################################# #

## check operating vs closed ?  ignored here - frs datacreate handled that 
x = "ICIS-AIR_FACILITIES.csv"
x = read.csv(file.path(td, "icis", x), stringsAsFactors = FALSE)
cbind(count = table(x$AIR_OPERATING_STATUS_DESC), pct = round(100 * table(x$AIR_OPERATING_STATUS_DESC)/NROW(x),0))
#                     count pct
#     [blank!]        11243   4
# Operating          183540  67
# Permanently Closed  73727  27  ** 27% are closed
# Planned Facility      709   0
# Seasonal              330   0
# Temporarily Closed   4063   1
# Under Construction    534   0
rm(x)
################################# #

# Get the main table with info about MACT NESHAP subparts

y = "ICIS-AIR_PROGRAM_SUBPARTS.csv"
y = read.csv(file.path(td, "icis", y), stringsAsFactors = FALSE)
# names(y)
# > table(y$PROGRAM_CODE == "CAAMACT")
#  FALSE   TRUE 
# 113887  68375   5/2024
y <- y[y$PROGRAM_CODE == "CAAMACT", ]
y$subpart <- gsub("CAAMACT", "", y$AIR_PROGRAM_SUBPART_CODE)
y <- y[, c("PGM_SYS_ID", "subpart", "AIR_PROGRAM_SUBPART_DESC")]
################################# #
## (clean subpart codes) #### 
#
# unique(y$subpart)   ## e.g., FFFF, 6C, etc.
# note that some are 6B or 6J not BBBBBB or JJJJJJ
# cbind(sort(unique(y$subpart)))
#
# function to convert something like 6B into BBBBBB. Done differently via tidyverse below also.
expandit <- function(old) {
  acount <-  (substr(old,1,1) %in% 0:9)
  for (i in 1:length(old)) {
    if (acount[i]) {
      old[i] <- paste(rep(substr(old[i] ,2, 2), as.numeric(  substr(old[i] ,1,1))), collapse = "")
    } 
  }
  return(old)
}
y$subpartclean <- expandit(y$subpart)
y = y[ , c("PGM_SYS_ID", "subpartclean", "subpart", "AIR_PROGRAM_SUBPART_DESC")]
# double check that against text within the description field
z <- y$AIR_PROGRAM_SUBPART_DESC
# cbind(gsub(".*Subpart (.*) - .*", "\\1", z ), z)
# unique(gsub(".*Subpart (.*) - .*", "\\1", z ) )
z <- gsub("(.*) - .*", "\\1", gsub(".*Subpart (.*) - .*", "\\1", z ))
# z
table(z == y$subpartclean)
rm(z)
# all true 5/2024
################################# #
## clean titles  
# Extract just the title of the category, without all the duplicative text that field has

y$title <-  gsub((".* Subpart .{1,8} - ([a-zA-Z]*)"), "\\1", y$AIR_PROGRAM_SUBPART_DESC)
# (cbind(y$AIR_PROGRAM_SUBPART_DESC,  gsub((".* Subpart .{1,8} - ([a-zA-Z]*)"), "\\1", y$AIR_PROGRAM_SUBPART_DESC)))[700:710,]
y <- y[ , c("PGM_SYS_ID", "subpartclean", "title")]
names(y) <- c("programid", "subpart", "title")
dim(y) # [1] 68375     3
################################# #

## (y gets renamed "frs_by_mact") ####

data.table::setDT(y, key = c("subpart", "programid"))
frs_by_mact <- data.table::copy(y)
rm(y)
############################################# #


############################################# #
############################################# #

## see which MACT categories have more/less location info

x = frs_by_mact[, .( n = .N, mact_table$dropdown_label[mact_table$subpart %in% subpart], haslatlon = sum(!is.na(lat)), pctok = round(100 * sum(!is.na(lat)) / .N, 0)), by = 'subpart']
setorder(x, n)
print(x, nrows = 138)
plot(x$n / 1000, x$pctok, xlim = c(0, 10), ylab = "% with latlon", 
     xlab = "Thousands of facilities (not showing 43k in RICE ZZZZ)", 
     main = "How many facilities in each MACT Subpart have location info?")
bigpct = x$pctok > 50
text(x$n[bigpct] / 1000, x$pctok[bigpct],
     pos = 4,
     labels = x$V2[bigpct],
     # labels =  x$subpart[bigpct]
     )

# ***   largest % of sites missing lat lon are in these subparts (excluding very small categories)
print(x[n > 10,][pctok < 50, ][order(pctok), ][1:25, ] ) 

# ***   largest numbers (not %) of sites with missing latlon are in these subparts
x[, nolatlon := n - haslatlon] 
x[order(-nolatlon), ][1:25,]
# 
#    subpart     n                                                                                          V2 haslatlon pctok nolatlon
#     <char> <int>                                                                                      <char>     <int> <num>    <int>
# 1:    ZZZZ 42819                 ZZZZ - Stationary Reciprocating Internal Combustion Engines (Rice) (42,556)     24981    58    17838
# 2:      HH  8343                                      HH - Oil And Natural Gas Production Facilities (8,325)      2718    33     5625
# 3:       A  7944                                                              A - General Provisions (7,939)      2738    34     5206
# 4:       M  8791                                                  M - Dry Cleaners Perchloroethylene (8,790)      4526    51     4265
# 5:   DDDDD  4324 DDDDD - Major Sources: Industrial/Commercial/Institutional Boilers & Process Heater (4,315)      1351    31     2973
# 6:       N  2453                                                         N - Chromium Electroplating (2,453)       684    28     1769
# 7:       T  2357                                                    T - Halogenated Solvent Cleaning (2,357)       758    32     1599
# 8:  HHHHHH  6906              HHHHHH - Paint Strip & Misc Surface Coating Operations At Area Sources (6,906)      5340    77     1566
# 9:  CCCCCC  3507                                             CCCCCC - Gasoline Dispensing Facilities (3,508)      2436    69     1071
# 10:  JJJJJJ  2089             JJJJJJ - Industrial, Commercial, And Institutional Boilers Area Sources (2,091)      1059    51     1030
# 11:    MMMM  1375                            MMMM - Surface Coating Of Misc. Metal Parts And Products (1,372)       431    31      944
# 12:      JJ  1046                                        JJ - Wood Furniture Manufacturing Operations (1,046)       244    23      802
# 13:    WWWW  1252                                     WWWW - Reinforced Plastic Composites Production (1,253)       478    38      774
# 14:  BBBBBB  1935 BBBBBB - Gasoline Distribution Bulk Terminals, Bulk Plants, And Pipeline Facilities (1,936)      1189    61      746
# 15:    AAAA  1211                                              AAAA - Municipal Solid Waste Landfills (1,203)       585    48      626
# 16:    FFFF   673                             FFFF - Miscellaneous Organic Chemical Manufacturing (Mon) (674)       148    22      525
# 17:       R  1120                                                           R - Gasoline Distribution (1,120)       597    53      523
# 18:      KK   710                                                            KK - Printing & Publishing (710)       210    30      500
# 19:       H   601                                             H - Equipment Leaks Of Hazardous Organics (602)       121    20      480
# 20:    DDDD   740                                            DDDD - Plywood And Composite Wood Products (738)       266    36      474
# 21:    EEEE   571                                    EEEE - Organic Liquids Distribution (Non-Gasoline) (570)       111    19      460
# 22:    PPPP   662                                  PPPP - Surface Coating Of Plastic Parts And Products (663)       206    31      456
# 23:       G   538             G - Socmi Process Vents, Storage Vessels, Transfer Operations, Wastewater (539)       115    21      423
# 24:     RRR   655                                                   RRR - Secondary Aluminum Production (653)       237    36      418
# 25:    JJJJ   538                                                     JJJJ - Paper & Other Web Coatings (535)       142    26      396
# subpart     n                                                                                          V2 haslatlon pctok nolatlon
############################################# #
############################################# #


############################################# #
# ~ ####

# CREATE "mact_table" from "frs_by_mact" for site counts by Subpart ####

# a table of the unique types (MACT codes) and the title of each  

types <- unique(frs_by_mact[, c("subpart", "title")])

## make dropdown_label (paste letters & titles together to show in dropdown menu) ####
types$dropdown_label <- paste(types$subpart, stringr::str_to_title(types$title), sep = ' - ')
types <- types[order(types$subpart), ]
rownames(types) <- NULL
names(types) <- c("subpart", "title", 'dropdown_label')
dim(types)  # 136  3
# rename it 
mact_table <- types  
mact_counts <- frs_by_mact[, .N, by = 'subpart'] 
mact_table <- dplyr::left_join(mact_table, mact_counts)
mact_table$dropdown_label <- paste0(mact_table$dropdown_label, ' (',prettyNum(mact_table$N, big.mark = ','), ')')

rm(mact_counts, types)

########################################################################### # 
########################################################################### # 
# ~ ####

# CREATE "afs_mact" that helps make "frs_by_mact" (but that is used above??) ####

# add AFS MACT data #

## DOWNLOAD DATA ####
## source: https://echo.epa.gov/tools/data-downloads, AFS Dataset
zipurl = "https://echo.epa.gov/files/echodownloads/afs_downloads.zip"
td = tempdir()
zname = "afs_downloads.zip"
download.file(zipurl, destfile = file.path(td, zname) )
if (!file.exists(file.path(td,zname))) {stop("zip not downloaded")}
unzip(file.path(td, zname), exdir = file.path(td, "afs_downloads"))
if (!dir.exists(file.path(td, "afs_downloads"))) {stop("temp subfolder not created")}

AIR_PROGRAM    <- readr::read_csv(file.path(td, "afs_downloads/AIR_PROGRAM.csv"))
AFS_FACILITIES <- readr::read_csv(file.path(td, "afs_downloads/AFS_FACILITIES.csv"))
# > class(AIR_PROGRAM) # [1] "spec_tbl_df" "tbl_df"      "tbl"         "data.frame" 

AIR_PROGRAM$AFS_ID <- stringr::str_pad(AIR_PROGRAM$AFS_ID, width = 10, pad = "0")

AFS_FACILITIES$AIR_PROGRAM_CODE_SUBPARTS <- AIR_PROGRAM$AIR_PROGRAM_CODE_SUBPARTS[match(AFS_FACILITIES$PLANT_ID,AIR_PROGRAM$PLANT_ID)]
AFS_FACILITIES$AIR_PROGRAM_CODE <- AIR_PROGRAM$AIR_PROGRAM_CODE[match(AFS_FACILITIES$AIR_PROGRAM_CODE_SUBPARTS,AIR_PROGRAM$AIR_PROGRAM_CODE_SUBPARTS)]

AIR_PROGRAM_MACT <- AFS_FACILITIES[AFS_FACILITIES$AIR_PROGRAM_CODE == "M",]
AIR_PROGRAM_MACT$AFS_ID <- stringr::str_pad(AIR_PROGRAM_MACT$AFS_ID, 10, pad = "0")
AIR_PROGRAM_MACT <- data.table::data.table(AIR_PROGRAM_MACT)
AIR_PROGRAM_MACT <- tidyr::unnest(AIR_PROGRAM_MACT, cols = AIR_PROGRAM_CODE_SUBPARTS)

AIR_PROGRAM_MACT_PLANT_ID <- tidyr::separate_longer_delim(AIR_PROGRAM_MACT, c(AIR_PROGRAM_CODE_SUBPARTS), delim = ",") #AIR_PROGRAM_MACT[,list(AIR_PROGRAM_CODE_SUBPARTS = unlist( strsplit( AIR_PROGRAM_CODE_SUBPARTS , "," ) ) ) , by = PLANT_ID ]
AIR_PROGRAM_MACT_PLANT_ID$AFS_ID <- AIR_PROGRAM_MACT$AFS_ID[match(AIR_PROGRAM_MACT_PLANT_ID$PLANT_ID,AIR_PROGRAM_MACT$PLANT_ID)]
AIR_PROGRAM_MACT_PLANT_ID$PLANT_NAME <- AIR_PROGRAM_MACT$PLANT_NAME[match(AIR_PROGRAM_MACT_PLANT_ID$PLANT_ID,AIR_PROGRAM_MACT$PLANT_ID)]
AIR_PROGRAM_MACT_PLANT_ID$EPA_REGION <- AIR_PROGRAM_MACT$EPA_REGION[match(AIR_PROGRAM_MACT_PLANT_ID$PLANT_ID,AIR_PROGRAM_MACT$PLANT_ID)]
AIR_PROGRAM_MACT_PLANT_ID$PLANT_STREET_ADDRESS <- AIR_PROGRAM_MACT$PLANT_STREET_ADDRESS[match(AIR_PROGRAM_MACT_PLANT_ID$PLANT_ID,AIR_PROGRAM_MACT$PLANT_ID)]
AIR_PROGRAM_MACT_PLANT_ID$PLANT_CITY <- AIR_PROGRAM_MACT$PLANT_CITY[match(AIR_PROGRAM_MACT_PLANT_ID$PLANT_ID,AIR_PROGRAM_MACT$PLANT_ID)]
AIR_PROGRAM_MACT_PLANT_ID$PLANT_COUNTY <- AIR_PROGRAM_MACT$PLANT_COUNTY[match(AIR_PROGRAM_MACT_PLANT_ID$PLANT_ID,AIR_PROGRAM_MACT$PLANT_ID)]
AIR_PROGRAM_MACT_PLANT_ID$STATE <- AIR_PROGRAM_MACT$STATE[match(AIR_PROGRAM_MACT_PLANT_ID$PLANT_ID,AIR_PROGRAM_MACT$PLANT_ID)]
AIR_PROGRAM_MACT_PLANT_ID$STATE_NUMBER <- AIR_PROGRAM_MACT$STATE_NUMBER[match(AIR_PROGRAM_MACT_PLANT_ID$PLANT_ID,AIR_PROGRAM_MACT$PLANT_ID)]
AIR_PROGRAM_MACT_PLANT_ID$ZIP_CODE <- AIR_PROGRAM_MACT$ZIP_CODE[match(AIR_PROGRAM_MACT_PLANT_ID$PLANT_ID,AIR_PROGRAM_MACT$PLANT_ID)]
AIR_PROGRAM_MACT_PLANT_ID$PRIMARY_SIC_CODE <- AIR_PROGRAM_MACT$PRIMARY_SIC_CODE[match(AIR_PROGRAM_MACT_PLANT_ID$PLANT_ID,AIR_PROGRAM_MACT$PLANT_ID)]
AIR_PROGRAM_MACT_PLANT_ID$SECONDARY_SIC_CODE <- AIR_PROGRAM_MACT$SECONDARY_SIC_CODE[match(AIR_PROGRAM_MACT_PLANT_ID$PLANT_ID,AIR_PROGRAM_MACT$PLANT_ID)]
AIR_PROGRAM_MACT_PLANT_ID$NAICS_CODE <- AIR_PROGRAM_MACT$NAICS_CODE[match(AIR_PROGRAM_MACT_PLANT_ID$PLANT_ID,AIR_PROGRAM_MACT$PLANT_ID)]

# dim(AIR_PROGRAM_MACT_PLANT_ID)
# [1] 46913    23 as of 5/2024 
## this now contains >40k  additional facilities linked to EPA_PROGRAM 'AIRS/AFS' 
##  (old list had only 19K with valid latlons) 

afs_mact <- AIR_PROGRAM_MACT_PLANT_ID |> 
  dplyr::select(programid = "AFS_ID", subpart = "AIR_PROGRAM_CODE_SUBPARTS") |>
  dplyr::rowwise() |>
  ## (clean subpart codes) #### 
  ## some subparts written as "6C" instead of "CCCCCC", so converting these to all letters
  ## also see above the function expandit() that should do the same as this:
  dplyr::mutate(subpart = 
                  ifelse(stringr::str_detect(subpart, '[:digit:]'),
                         paste0(rep(substr(subpart, 2, 2), 
                                    as.numeric(substr(subpart,1,1))), collapse = ""),
                         subpart)
  ) |>
  dplyr::ungroup() |> 
  ## add title column
  dplyr::left_join(mact_table)    

# finished creating afs_mact 

####################################################### #


################### ################################## #################### ################################## #
################### ################################## #################### ################################## #

# >>>> STOP AND FIX <<<<< ####

stop(" check this join and code sequencing -- where do these steps belong?? they were much later but code got shuffled")


## _____WHY/WHEN DO JOIN ?  was frs_by_mact joined with types aka mact_table here or later ####
# code seemed to go back and forth where frs_by_mact helped make mact_table but they got joined, afs_mact rbinded??


frs_by_mact = frs_by_mact |>
  ## add title column
  dplyr::left_join(types) # types is created later @! --- DOES NOT MAKE SENSE  ****************


# dim(frs_by_mact) # [1] 68375     4
# "ICIS-AIR_PROGRAM_SUBPARTS.csv" was the source of frs_by_mact

# >>> ??? combine  afs_mact + frs_by_mact (aka y) tables ? but they overlap ! ####

# "https://echo.epa.gov/files/echodownloads/ICIS-AIR_downloads.zip", "ICIS-AIR_PROGRAM_SUBPARTS.csv" 
#  was the source of y
# dim(y)        # [1] 68375     4 as of 5/2024
# y was used to create  frs_by_mact

# "https://echo.epa.gov/files/echodownloads/afs_downloads.zip", AIR_PROGRAM.csv and AFS_FACILITIES.csv 
#  was the source of afs_mact
# dim(afs_mact) # [1] 46913     4 as of 5/2024

# > intersect(y$programid, afs_mact$programid)
# character(0)
# > unique(substr(afs_mact$programid,1,2))
# [1] "09" "23" "25" "34" "33" "44" "50" "36" "24" "72" "10" "11" "78" "42" "51" "54" "12" "01" "13" "21" "37" "28" "45" "17" "47" "18" "26" "27" "39" "22" "55" "05" "35" "40" "48" "19" "20" "29" "31"
# [40] "08" "30" "49" "46" "38" "02" "06" "56" "04" "53" "32" "66" "69" "15" "16" "41"
# > unique(substr(y$programid,1,2))
# [1] "AR" "CT" "FL" "IA" "IL" "IN" "LA" "MA" "MD" "ME" "MI" "MN" "MS" "MT" "NC" "NJ" "02" "NM" "NY" "OH" "OK" "OR" "PA" "PR" "RI" "TN" "TX" "VA" "WA" "WI" "WV" "AL" "AK" "09" "CA" "CO" "DC" "DE" "GA"
# [40] "HI" "ID" "KS" "KY" "MO" "ND" "NE" "NH" "SC" "SD" "UT" "VI" "WY" "10" "08" "VT" "05" "06" "AZ" "NV" "MP" "GU" "SU" "04"



################### ################################## #################### ################################## #
################### ################################## #################### ################################## #

# >>>> STOP AND FIX <<<<< ####

stop(" check this rbind !! ")


## _____WHY DO THIS rbindlist ??? ####
# they seem to overlap a lot and just have different ways of saving the programid !!  --- DOES NOT MAKE SENSE ****************

frs_by_mact <- data.table::rbindlist(list(frs_by_mact, afs_mact))

## filter out missing latlons, now 55,411 valid records as of 6/2023
frs_by_mact <- frs_by_mact %>% 
  ## join by programid to get lat and lon columns
  dplyr::left_join(frs_by_programid, by = c('programid' = 'pgm_sys_id')) 



################### ################################## #################### ################################## #
################### ################################## #################### ################################## #



#################################################### #
# ~ ####

# CREATE "epa_program_counts"  from  frs_by_programid  ####

epa_program_counts <- dplyr::count(frs_by_programid, program, name = 'count') # EJAM :: frs_by_programid
epa_program_counts$pgm_text_dropdown <- paste0(epa_program_counts$program, ' (',prettyNum(epa_program_counts$count, big.mark = ','), ')')
epa_programs <- setNames(epa_program_counts$program, epa_program_counts$pgm_text_dropdown)


#################################################### #
# ~ ####

# SAVE DATASETS for package ####

## save mact_table ####
mact_table     <- metadata_add(mact_table)
usethis::use_data(mact_table, overwrite = TRUE)  # data.frame

## save frs_by_mact ####
frs_by_mact   <- metadata_add(frs_by_mact)
usethis::use_data(frs_by_mact, overwrite = TRUE)    # data.table

## save epa_programs ####
epa_programs <- metadata_add(epa_programs)
usethis::use_data(epa_programs, overwrite = TRUE) # data.frame

#################################################### #

stop("done")



################################################################################################ #
################################################################################################ #

# save(frs_by_mact, file = "frs_by_mact.rda")
# save(mact_table, "mact_table.rda")
 
################################################################################################ #

# Facility/Source Level Identifying Data (ICIS-AIR_FACILITIES.csv)
#
# PGM_SYS_ID   -   this is the key for linking between tables 
# REGISTRY_ID
# SIC_CODES	Char	4000
# NAICS_CODES	Char	4000
# FACILITY_TYPE_CODE	Char	3
# AIR_OPERATING_STATUS_CODE	Char	5   e.g.  OPR  = 	Operating
# AIR_OPERATING_STATUS_DESC	Char	100

# Air Program Subparts (ICIS-AIR_PROGRAM_SUBPARTS.csv)
#
# Element Name	Data Type	Length
# PGM_SYS_ID1	Char	30
# PROGRAM_CODE	Char	9      Code values include:   CAAMACT
# PROGRAM_DESC	Char	100
# AIR_PROGRAM_SUBPART_CODE	Char	20
# AIR_PROGRAM_SUBPART_DESC	Char	200

# AIR_PROGRAM_CODE_SUBPARTS - A field indicating applicable air program subparts. Subpart code values can be found on the ICIS-Air Program Code Subpart Descriptions page.
# https://echo.epa.gov/tools/data-downloads/icis-air-download-summary/air-program-code-subpart-descriptions#caamact


################################################################################################ #
################################################################################################ #

if (1 == 0) {
  
# also tried to use web services but without success


# https://echo.epa.gov/tools/web-services

# FRS via ECHO does have a field called AIRMacts with comma separated list of subparts like CC, FFFF, YY
#
#  *** BUT so far cannot get that type of query to work in their API - using MACT subparts as query of air systems services.
#
# ECHO API has air services that let you query on MACT subpart, but it fails the way tried.
# ECHO API example of Air services - fails when try to search on MACT subparts:
# https://echodata.epa.gov/echo/air_rest_services.get_facilities?output=JSON&p_mact=FFFF&qcolumns=2%2C8%2C22%2C23%2C25%2C26

# https://echodata.epa.gov/echo/air_rest_services.get_facility_info?p_act=Y&p_maj=Y&p_mact=ZZZZZ&qcolumns=2%2C8%2C22%2C23%2C25%2C26

# AIRPrograms	                  AIRMacts
# MACT, NSPS, NSR, SIP, TVP	    DDDDD, EEEE, FFFF, HHHHH, JJJJ, KK, M, ZZZZ


# AIRStatus = Operating
# 
# FacLat	FacLong
# 31.671177	-98.996513
# 
# AIRNsps	 
# NSPS Part 60 - Subpart Dc - SMALL INDUS-COMMER-INSTITUTL STEAM GENERATING UNITS, NSPS Part 60 - Subpart JJJJ - STATIONARY SPARK IGNITION INTERNAL COMBUSTION ENGINES, NSPS Part 60 - Subpart RR - PRESSR-SENST TAPE, LABEL SURFACE COATING OPERATIONS	

stop("stopped here")
 
subpart = 'FFFF'

x <- get_facility_info_via_ECHO(
  qcolumns = c(2,8,22,23,25,26), url_not_query = T, otherparameters = "&registry_id=110015778176")

x <- httr::GET(x)  # seems to get stuck for a VERY long wait

# one query worked this way but others do not: - this all needs to be rewritten or dropped.
x <- jsonlite::fromJSON(rawToChar(x$content))$Results$ClusterOutput$ClusterData[,1:8]


#   that does not include a field called AIRMacts
# 
# https://echodata.epa.gov/echo/air_rest_services.metadata
# get_facilities, get_qid, get_map, and get_downoad
# Use get_facilities to validate passed query parameters, obtain summary statistics and to obtain a query_id (QID). QIDs are time sensitive and will be valid for approximately 30 minutes.
# Use get_download, with the returned QID, to generate a Comma Separated Value (CSV) file of facility information that meets the QID query criteria.


# # FRS query on registry ID
# https://ofmpub.epa.gov/frs_public2/frs_rest_services.get_facility_wbd?registry_id=110015778176
# 
# # FRS API query example - within 3 miles of 1 point, find all FRS sites (of certain type)
# # URL for searching SEMS (Superfund) facilities within a 3 mile radius of latitude 38.8/longitude -77.01.
# https://ofmpub.epa.gov/frs_public2/frs_rest_services.get_facilities?latitude83=38.8&longitude83=-77.01&search_radius=3&pgm_sys_acrnm=SEMS&output=JSON
# 
# # FRS QUERY PAGE FOR USERS TO MANUALLY ENTER NAICS TO SEARCH FOR AND GET LIST OF SITES/LAT/LON,WHATEVER
# https://frs-public.epa.gov/ords/frs_public2/ez_frs_column.list?table_name=D_EF_FAC.V_PUB_FRS_NAICS_EZ



# ECHO API has air services that let you query on MACT subpart, but it fails when I try.
# ECHO API example of Air services - fails when I try to search on MACT subparts:
# https://echodata.epa.gov/echo/air_rest_services.get_facilities?output=JSON&p_mact=FFFF&qcolumns=2%2C8%2C22%2C23%2C25%2C26
# browseURL("https://echodata.epa.gov/echo/air_rest_services.get_facilities?output=JSON&p_mact=ZZZZ&qcolumns=2%2C8%2C22%2C23%2C25%2C26")
# Key columns to ask for:
# SourceID 2
# RegistryID 8
# FacLat 22
# FacLong 23
# AIRMacts 25
# AIRStatus  26
# 2,8,22,23,25,26

# ECHO API example of Air services downloading csv based on QID already gotten
# https://echodata.epa.gov/echo/air_rest_services.get_download?qid=339&qcolumns=2%2C8%2C22%2C23%2C25%2C26


# "ColumnName": "AIR_MACTS",
# "DataType": "VARCHAR2",
# "DataLength": "4000",
# "ColumnID": "25",
# "ObjectName": "AIRMacts",
# "Description": "The Maximum Achievable Control Technology (MACT) Subpart associated with the facility."
}
