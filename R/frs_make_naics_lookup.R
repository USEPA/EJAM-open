#' Reformat frs datatable to look up by NAICS
#'
#' @param x data.table frs from [frs_get()]
#' @import data.table
#' @return data.table with columns NAICS, REGISTRY_ID, etc.
#' @seealso [frs_update_datasets()] which uses [frs_get()] to create frs_by_naics
#'
frs_make_naics_lookup <- function(x) {
  
  # create the longer format lookup table for just matching on NAICS to get lat lon registry_id
  
  # create the same for matching on program system ID here or   
  # input is frs, the output of frs_get, a data.table
  # 
  # Classes ‘data.table’ and 'data.frame':
  lat <- lon <- REGISTRY_ID <- NAICS <- PGM_SYS_ACRNMS <- NULL # get rid of warnings for line below
    x <- x[ , .(lat, lon, REGISTRY_ID, NAICS, PGM_SYS_ACRNMS)] # PRIMARY_NAME, 
 
  ############################################################ #
  #  fix the NAICS column, 
  x <- tidyr::separate_rows(x, NAICS, sep = ',') # SLOW
  x$NAICS <- as.numeric(x$NAICS)
 
  # x$PRIMARY_NAME <- NULL
  
  # x$INTEREST_TYPES <- NULL
  x$PGM_SYS_ACRNMS <- NULL
  
  # x$SITE_TYPE_NAME <- NULL
  # x$SIC_CODES <- NULL
  
  x <- data.table::as.data.table(x)
  x <- x[ !is.na(NAICS), ]
  x <- x[naics_is.valid(NAICS),]
  setkey(x, NAICS, REGISTRY_ID)
#  head(x,20)
#   head(x, 20)
  attr(x, 'released') <- Sys.Date()
  # print('To use in package,  usethis::use_data(frs_by_naics, overwrite=TRUE)  ')
  
    invisible(x)
  
  
  # 
  # > str(x)  # frs as of 2022 download
  # Classes ‘data.table’ and 'data.frame':	3185964 obs. of  5 variables:
  #   $ REGISTRY_ID   : chr  "110058283236" "110071102848" "110038070002" "110070215939" ...
  # $ PGM_SYS_ACRNMS: chr  "NPDES:AKU000292" "SEMS:AK4170024323" "BRAC:AK4170024323, EIS:10606011, ICIS:22754, NCDB:C10#10-00179-01-ADL, NCDB:C10#10-88028-01-WR, NCDB:D10#10-900"| __truncated__ "NPDES:AKR06AA03" ...
  # $ NAICS         : chr  NA NA NA NA ...
  # $ lat           : num  51.9 51.9 51.9 51.9 51.9 ...
  # $ lon           : num  -177 -177 -177 -177 -177 ...
  # - attr(*, ".internal.selfref")=<externalptr> 
  #   - attr(*, "sorted")= chr "NAICS"
  # - attr(*, "index")= int(0) 
  # ..- attr(*, "__REGISTRY_ID")= int [1:3185964] 2702853 2003156 2630327 2628695 2577016 2650824 2702857 2554626 2655269 2816289 ...
  # - attr(*, "date")= Date[1:1], format: "2022-02-09"
  # # > 
  
  
  # tibble [3,352,822 x 4] (S3: tbl_df/tbl/data.frame)
  # $ REGISTRY_ID: chr [1:3352822] "110058283236" "110038070002" "110070215939" "110015787683" ...
  # $ NAICS_CODES: chr [1:3352822] NA NA NA "928110" ...
  # $ LAT        : num [1:3352822] 51.9 51.9 51.9 51.9 51.9 ...
  # $ LONG       : num [1:3352822] -177 -177 -177 -177 -177 ...
  
  
  #   > str(frs_naics_2016) now file is _2022 
  # 'data.frame':	1847061 obs. of  6 variables:
  #   $ PROGRAM    : Factor w/ 22 levels "AIRS/AFS","BR",..: 15 21 21 4 21 20 15 17 17 15 ...
  # $ PROGRAM_ID : Factor w/ 1718088 levels "000003GA001",..: 1511957 1384 1391 590866 1538 424 1512071 65002 65002 1511948 ...
  # $ REGISTRY_ID: num  1.1e+11 1.1e+11 1.1e+11 1.1e+11 1.1e+11 ...
  # $ NAICS      : Factor w/ 2693 levels "","1","10","1005",..: 1 791 1 1 445 1 1 450 536 540 ...
  # $ LAT        : num  18.4 18.4 18.4 18.5 18.5 ...
  # $ LONG       : num  -66.1 -66.1 -66.1 -66.8 -66.8 ...
  # 
  # frs_naics_2020
  
}
