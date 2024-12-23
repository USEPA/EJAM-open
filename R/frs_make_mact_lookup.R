#' Create updated version of frs_by_mact and mact_table
#' @seealso  [frs_update_datasets()]
#' @param frs_by_programid from output of frs_make_programid_lookup()
#' @param folder optional, where to download ICIS-AIR_downloads.zip to, tempdir() by default
#'
#' @return list, of frs_by_mact data.table and mact_table data.frame
#'
frs_make_mact_lookup <- function(frs_by_programid, folder=NULL) {
  
  if (is.null(folder)) {folder <- tempdir()}
  
  ###################################################### #
  
  # script to download and clean a table of facilities by MACT subpart
  # This uses the frs_by_programid dataset, so when that is updated, this should get updated after that.
  
  # oldwd <- getwd()
  # setwd("~/../Downloads")
  
  # One can download from ECHO all the 14,000 or so CAA Major Actives   ####
  # https://echo.epa.gov/tools/data-downloads
  # and then remove the ones without  "MACT" listed in the column called AIRPrograms ####
  # which are the one that have nothing in the column called  AIRMacts.
  #  AIRMacts  column is a csv list of codes like DDDDD, EEEE, FFFF, HHHHH, JJJJ, KK, N, UU, ZZZZ
  #  that apply to the given facility.
  # https://echo.epa.gov/tools/data-downloads/icis-air-download-summary 
  # ICIS-Air Datasets
  
  fname = "https://echo.epa.gov/files/echodownloads/ICIS-AIR_downloads.zip"
  cat('downloading ICIS-AIR_downloads.zip\n')
  download.file(fname, destfile = file.path(folder, "ICIS-AIR_downloads.zip"))
  unzip(file.path(folder, "ICIS-AIR_downloads.zip"), exdir = folder)
  
  # dir(pattern = "ICIS")
  # [1] "ICIS-AIR_downloads.zip"         "ICIS-AIR_FACILITIES.csv"       
  # [3] "ICIS-AIR_FCES_PCES.csv"         "ICIS-AIR_FORMAL_ACTIONS.csv"   
  # [5] "ICIS-AIR_INFORMAL_ACTIONS.csv"  "ICIS-AIR_POLLUTANTS.csv"       
  # [7] "ICIS-AIR_PROGRAM_SUBPARTS.csv"  "ICIS-AIR_PROGRAMS.csv"         
  # [9] "ICIS-AIR_STACK_TESTS.csv"       "ICIS-AIR_TITLEV_CERTS.csv"     
  # [11] "ICIS-AIR_VIOLATION_HISTORY.csv"
  
  ###################################################### #
  
  # Get the operating status to see which are Active/Operating or seasonal ?  ignored that here.
  #
  # x = "ICIS-AIR_FACILITIES.csv"
  # x = read.csv(file = file.path(folder, x), stringsAsFactors = FALSE)
  # names(x)
  # # unique(x$AIR_OPERATING_STATUS_DESC)
  # # "Operating"  "Permanently Closed"  ""  "Temporarily Closed" "Planned Facility" "Seasonal"  "Under Construction"
  # 
  # rm(x)
  ###################################################### #
  
  # Get the main table with info about MACT NESHAP subparts ####
  
  y = "ICIS-AIR_PROGRAM_SUBPARTS.csv"
  y = read.csv(file.path(folder, y), stringsAsFactors = FALSE)
  
  # setwd(oldwd)
  
  # names(y)
  # > table(y$PROGRAM_CODE == "CAAMACT")
  # FALSE   TRUE 
  # 113547  67812 
  y <- y[y$PROGRAM_CODE == "CAAMACT", ]
  y$subpart <- gsub("CAAMACT", "", y$AIR_PROGRAM_SUBPART_CODE)
  y <- y[, c("PGM_SYS_ID", "subpart", "AIR_PROGRAM_SUBPART_DESC")]
  # unique(y$subpart)
  # note that some are 6B or 6J not BBBBBB or JJJJJJ
  # cbind(sort(unique(y$subpart)))
  
  ###################################################### #
  # function to convert something like 6B into BBBBBB ####
  expandit <- function(old) {
    acount <-  (substr(old,1,1) %in% 0:9) 
    for (i in 1:length(old)) {
      if (acount[i]) {
        old[i] <-     old[i] <- paste(rep(substr(old[i] ,2,2), as.numeric(  substr(old[i] ,1,1))), collapse = "") 
      } 
    }
    old
  }
  ###################################################### #
  
  y$subpartclean <- expandit(y$subpart)
  y = y[ , c("PGM_SYS_ID", "subpartclean", "subpart", "AIR_PROGRAM_SUBPART_DESC")]
  
  # double check that against text within the description field
  # z <- y$AIR_PROGRAM_SUBPART_DESC
  # cbind(gsub(".*Subpart (.*) - .*", "\\1", z ), z)
  # unique(gsub(".*Subpart (.*) - .*", "\\1", z ) )
  # z <- gsub("(.*) - .*", "\\1", gsub(".*Subpart (.*) - .*", "\\1", z ))
  # z
  # table(z == y$subpartclean)
  # rm(z)
  # all true
  ###################################################### #
  
  # Extract just the title of the category, without all the duplicative text that field has ####
  
  y$title <-  gsub((".* Subpart .{1,8} - ([a-zA-Z]*)"), "\\1", y$AIR_PROGRAM_SUBPART_DESC)
  # (cbind(y$AIR_PROGRAM_SUBPART_DESC,  gsub((".* Subpart .{1,8} - ([a-zA-Z]*)"), "\\1", y$AIR_PROGRAM_SUBPART_DESC)))[700:710,]
  y <- y[ , c("PGM_SYS_ID", "subpartclean", "title")]
  names(y) <- c("programid", "subpart", "title")
  
  ###################################################### #
  
  # Make a table of the unique MACT codes and the title of each category ####
  
  types <- unique(y[,c("subpart", "title")])
  ## paste letters and titles together for displaying in dropdown menu
  types$dropdown_label <- paste(types$subpart, stringr::str_to_title(types$title), sep = ' - ')
  types <- types[order(types$subpart), ]
  rownames(types) <- NULL
  names(types) <- c("subpart", "title",'dropdown_label')
  
  ###################################################### #
  
  # add AFS MACT data -------------------------------------------------------
  
  ## source: https://echo.epa.gov/tools/data-downloads, AFS Dataset
  ## https://echo.epa.gov/files/echodownloads/afs_downloads.zip
  download.file("https://echo.epa.gov/files/echodownloads/afs_downloads.zip", destfile = file.path(folder, "afs_downloads.zip"))
  unzip(file.path(folder, "afs_downloads.zip") , overwrite = TRUE, exdir = folder)
  
  AIR_PROGRAM <- readr::read_csv(file.path(folder, "AIR_PROGRAM.csv"))  # NOTE THIS IMPORTS IT AS A TIBBLE !
  # Warning message:
  #   One or more parsing issues, call `problems()` on your data frame for details, e.g.:
  #   dat <- vroom(...)
  # problems(dat) 
  # > vroom::problems()
  # # A tibble: 3 × 5
  #     row   col expected actual   file                                                           
  #     <int> <int> <chr>    <chr>    <chr>                                                          
  # 1 368124     9 a double E1732543 C:/Users/.../AppData/Local/Temp/Rtmp6HqiAl/AIR_PROGRAM.csv
  # 2 779088     9 a double E966515  C:/Users/.../AppData/Local/Temp/Rtmp6HqiAl/AIR_PROGRAM.csv
  # 3 779090     9 a double E966515  C:/Users/.../AppData/Local/Temp/Rtmp6HqiAl/AIR_PROGRAM.csv
  
  AFS_FACILITIES <- readr::read_csv(file.path(folder, "AFS_FACILITIES.csv"))  # NOTE THIS IMPORTS IT AS A TIBBLE !
  
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
  
  ## this now contains approx 44K (19K with valid latlons) additional facilities linked to EPA_PROGRAM 'AIRS/AFS' 
  afs_mact <- AIR_PROGRAM_MACT_PLANT_ID %>% 
    dplyr::select(programid = "AFS_ID", subpart = "AIR_PROGRAM_CODE_SUBPARTS") %>% 
    dplyr::rowwise() %>% 
    ## some subparts written as "6C" instead of "CCCCCC", so converting these to all letters
    dplyr::mutate(subpart = 
                    ifelse(stringr::str_detect(subpart, '[:digit:]'),
                           paste0(rep(substr(subpart, 2, 2), 
                                      as.numeric(substr(subpart,1,1))),collapse = ""),
                           subpart)
    ) %>% 
    dplyr::ungroup() %>% 
    ## add title column
    dplyr::left_join(types) 
  
  ## create dropdown labels
  y$dropdown_label <- paste(y$subpart, stringr::str_to_title(y$title), 
                            sep = " - ")
  y <- data.table::rbindlist(list(y, afs_mact))
  
  ###################################################### #
  
  ## filter out missing latlons, now 55,411 valid records as of 6/2023
  
  y <- y %>% 
    ## join by programid to get lat and lon columns
    dplyr::left_join(
      frs_by_programid, 
      by = c('programid' = 'pgm_sys_id')
    ) 
  
  ############### # 
  
  # setwd("~")
  
  ###################################################### #
  ################################################################################################ #
  ################################################################################################ #
  ################################################################################################ #
  
  
  data.table::setDT(y, key = c("subpart", "programid"))
  
  frs_by_mact <- data.table::copy(y)
  mact_table <- types
  

  ## add site counts to  mact_table , by MACT Subpart
  mact_counts <- frs_by_mact[, .N, by = 'subpart'] # EJAM #:: frs_by_programid
  mact_table <- dplyr::left_join(mact_table, mact_counts)
  mact_table$dropdown_label <- paste0(mact_table$dropdown_label, ' (',prettyNum(mact_table$N, big.mark = ','), ')')
  
  
  # to save in the package
  #
  # usethis::use_data(frs_by_mact, overwrite = TRUE)    # data.table
  # usethis::use_data(mact_table, overwrite = TRUE)  # data.frame
  # 
  # save(frs_by_mact, file = "frs_by_mact.rda")
  # save(mact_table, "mact_table.rda")
  
  
  return(
    list(
      frs_by_mact = frs_by_mact, 
      mact_table = mact_table
    )
  )
  
  ################################################################################################ #
  ################################################################################################ #
  ################################################################################################ #
  ################################################################################################ #
  
}


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



# tried to use web services but without success





# https://echo.epa.gov/tools/web-services

# FRS via ECHO does have a field called AIRMacts with comma separated list of subparts like CC, FFFF, YY
#
#  *** BUT so far I cannot get that type of query to work in their API - using MACT subparts as query of air systems services.
#
# ECHO API has air services that let you query on MACT subpart, but it fails the way I have tried.
# ECHO API example of Air services - fails when I try to search on MACT subparts:
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
################################################################################################ #


# stop("stopped here")


#  
# subpart = 'FFFF'
# 
# x <-  get_facility_info_via_ECHO(
#   qcolumns = c(2,8,22,23,25,26), url_not_query = T, otherparameters = "&registry_id=110015778176")
# 
# x <- httr::GET(x)  # seems to get stuck for a VERY long wait
# 
# # one query worked this way but others do not: - this all needs to be rewritten or dropped.
# 
# x <- jsonlite::fromJSON(rawToChar(x$content))$Results$ClusterOutput$ClusterData[,1:8]
################################################################################################ #

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

