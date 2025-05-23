
frs_clean_sic <- function(frs, usefulcolumns = c("LATITUDE83", "LONGITUDE83", 
                                                 "REGISTRY_ID", "PRIMARY_NAME", "SIC_CODES", "PGM_SYS_ACRNMS")) 
{
  frs <- frs[, ..usefulcolumns]
  setnames(frs, "LATITUDE83", "lat")
  setnames(frs, "LONGITUDE83", "lon")
  setnames(frs, "SIC_CODES", "SIC")
  cat("Finished cleaning file.\n")
  cat("Total rows: ", NROW(frs), "\n")
  frs <- frs[!is.na(frs$lat), ]
  cat("Rows with lat/lon: ", NROW(frs), "\n")
  data.table::setkey(frs, REGISTRY_ID)
  return(frs)
}

###################################################################################
# create frs_by_sic ####
###################################################################################

#' utility to reformat frs datatable to look up by SIC code
#'
#' @param x data.table frs from frs_clean_sic() 
#' @import data.table
#' @seealso frs_clean_sic() and [frs_by_sic]
#' @export
#'
frs_make_sic_lookup <- function(x) {
  
  # create the longer format lookup table for just matching on SIC to get lat lon registry_id
  
  # create the same for matching on program system ID here or   
  # input is frs, the output of frs_get, a data.table
  # 
  # Classes ‘data.table’ and 'data.frame':
  
  x <- x[ , .(lat, lon, REGISTRY_ID, SIC, PGM_SYS_ACRNMS)]
  
  ############################################################ #
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
  setkey(x, SIC, REGISTRY_ID)
  
  attr(x, 'released') <- Sys.Date()
  print('To use in package,  usethis::use_data(frs_by_sic, overwrite=TRUE)  ')
  
  invisible(x)
  # 
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




## import dataset used for frs_by_naics
## from https://ordsext.epa.gov/FLA/www3/state_files/
natl_single <- frs_read('NATIONAL_SINGLE.CSV', only_essential_cols = F)

## clean and extract SIC columns
natl_single_clean <- frs_clean_sic(natl_single)

## create lookup table 
frs_by_sic <- frs_make_sic_lookup(natl_single_clean)


#saveRDS(frs_by_sic, file = 'frs_by_sic.rds')
usethis::use_data(frs_by_sic)


###################################################################################
# create SIC ####
###################################################################################

# list of SIC codes
# from https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/sic-code-descriptions/sic88_97.txt
sic_txt <- readr::read_delim('sic88_97.txt', 
                             skip = 1, delim = '  ', col_names = c('code','desc'))
sic_txt <- sic_txt %>% dplyr::mutate(code = gsub("[^0-9]+", "", .data$code))

## separate 2- and 3-digit codes
sic_cats2 <- sic_txt %>% dplyr::filter(nchar(.data$code) == 2) #filter(str_detect(code,'--'))
sic_cats3 <- sic_txt %>% dplyr::filter(nchar(.data$code) == 3)#filter(str_detect(code,'\\'))

## save remaining codes as vector for list used in app
sic_cats4 <- sic_txt %>% dplyr::filter(nchar(.data$code) == 4) 

sictable <- sic_cats4
#saveRDS(sic_cats4, file = 'SIC_vec.rds')
#usethis::use_data(sictable)

## create named list for use in dropdown menu
SIC <- sic_cats4 %>% 
  dplyr::mutate(code_and_desc = paste(code, desc, sep = ' - ')) %>% 
  dplyr::select(code_and_desc, code) %>% 
  ## turn 2-column df into named list (col1 = names, col2 = values)
  deframe() 

## add counts to SIC code names for shiny dropdown
sic_counts_nosub <- frs_by_sic[, .N, by = 'SIC']
sic_counts_names <- enframe(SIC) %>%
  dplyr::left_join(sic_counts_nosub, by = c('value' = 'SIC')) %>%
  dplyr::mutate(name = ifelse(!is.na(.data$N),
                              paste0(.data$name, ' (', .data$N, ' sites)'), .data$name)) %>%
  select(-N) %>%
  deframe()

names(SIC) <- names(sic_counts_names)

#saveRDS(SIC, file='SIC.rds')
usethis::use_data(SIC, overwrite = TRUE)


## add counts to NAICS code names for shiny dropdown
naics_counts_nosub <- frs_by_naics[, .N, by = 'NAICS']
naics_counts_names <- enframe(NAICS) %>%
  dplyr::left_join(naics_counts_nosub, by = c('value' = 'NAICS')) %>%
  dplyr::mutate(name = ifelse(!is.na(.data$N),
                              paste0(.data$name, ' (', .data$N, ' sites)'), .data$name)) %>%
  select(-N) %>%
  deframe()

naics_counts_w_subs <- sapply(NAICS, function(x) {frs_from_naics(x, childrenForNAICS = T)[, .N]})

naics_counts <- data.frame(NAICS, count_w_subs = naics_counts_w_subs)
naics_counts <- enframe(NAICS,value = 'NAICS') %>% 
  left_join(naics_counts) %>% 
  dplyr::left_join(naics_counts_nosub) %>% 
  rename(count_no_subs = N) %>% 
  mutate(label_w_subs = ifelse(.data$count_w_subs > 0, paste0(.data$name, ' (', 
                                                        prettyNum(.data$count_w_subs, big.mark = ','),' sites)'),
                               name
  ),
  label_no_subs = ifelse(!is.na(count_no_subs) & count_no_subs > 0,
                         paste0(name, ' (', 
                                prettyNum(count_no_subs, big.mark = ','),' sites)'),
                         name)
  )

usethis::use_data(naics_counts, overwrite = TRUE)

