stop("NEED TO CONFIRM WHETHER THIS SCRIPT IS STILL NEEDED AT ALL")
library(magrittr)
# library(tibble) # for deframe() or is that different than the tibble::deframe() ??

names(SIC) <- sictable$num_name
sic_counts_nosub <- frs_by_sic[, .N, by = 'SIC']
sic_counts_names <- tibble::enframe(SIC) %>%
  dplyr::left_join(sic_counts_nosub, by = c('value' = 'SIC')) %>%
  dplyr::mutate(name = ifelse(!is.na(N),
                              paste0(name, ' (', N, ' sites)'), name)) %>%
  dplyr::select(-N) %>%
  tibble::deframe()

names(SIC) <- names(sic_counts_names)

attr(SIC, "date_saved_in_package") <- as.character(Sys.Date())

usethis::use_data(SIC, overwrite=T)
save(SIC, file = '../misc/SIC.rda')

### AND SEE  EJAM/data-raw/datacreate_sictable.R
