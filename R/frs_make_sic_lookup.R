#' Reformat frs datatable to look up facilities by SIC code
#' @param x data.table frs from [frs_clean_sic()]
#' @import data.table
#' @return data.table with lat, lon, REGISTRY_ID, SIC columns
#' @seealso [frs_upate_datasets()] [frs_clean_sic()] and the frs_by_sic data.table
#'
frs_make_sic_lookup <- function(x) {
  
  # create the longer format lookup table for just matching on SIC to get lat lon registry_id
  # 
  # create the same for matching on program system ID here or   
  # input is frs, the output of frs_get, a data.table
  # 
  # Classes ‘data.table’ and 'data.frame':
  
  x <- x[ , .(lat, lon, REGISTRY_ID, SIC, PGM_SYS_ACRNMS)]
  
  #################### ##### #
  #  fix the SIC column, 
  x <- tidyr::separate_rows(x, SIC, sep = ',')
  #x$SIC <- as.numeric(x$SIC)
  x$SIC <- trimws(x$SIC)
  x$PGM_SYS_ACRNMS <- NULL
  
  x <- data.table::as.data.table(x)
  x <- x[ !is.na(SIC), ]
  
  ## filter out empty codes, codes with letters, codes with non-numeric symbols
  x <- x[ SIC != '',   ]
  x <- x[ !grepl('[a-zA-Z]',SIC), ]
  ## only keep 4 digit numeric codes
  x <- x[ SIC %in% sprintf(0:9999, fmt = '%04.f'), ]
  data.table::setkey(x, SIC, REGISTRY_ID)
  
  attr(x, 'released') <- Sys.Date()
  # print('To use in package,  usethis::use_data(frs_by_sic, overwrite=TRUE)  ')
  
  invisible(x)
   
  # > str(x)  # frs as of 5/2023 download
  # Classes ‘data.table’ and 'data.frame':	1081742 obs. of  4 variables:
  # $ lat        : num  46 41.2 42 40.5 37.6 ...
  # $ lon        : num  -112.5 -96.1 -97.4 -99 -120.9 ...
  # $ REGISTRY_ID: chr  "110000428555" "110000447623" "110000448150" "110000448490" ...
  # $ SIC        : num  0 0 0 0 0 0 0 0 0 0 ...
  # - attr(*, ".internal.selfref")=<externalptr> 
  #   - attr(*, "sorted")= chr [1:2] "SIC" "REGISTRY_ID"
  # - attr(*, "released")= Date[1:1], format: "2023-05-25"
}
###################################################################################
