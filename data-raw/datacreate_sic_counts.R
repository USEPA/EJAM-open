stop("NEED TO CONFIRM WHETHER THIS SCRIPT IS STILL NEEDED AT ALL")
library(magrittr)

sic_counts_nosub <- frs_by_sic[, .N, by = 'SIC']
sic_counts_names <- tibble::enframe(SIC) %>%
  dplyr::left_join(sic_counts_nosub, by = c('value' = 'SIC')) %>%
  dplyr::mutate(name = ifelse(!is.na(N),
                              paste0(name, ' (', N, ' sites)'), name)) %>%
  select(-N) %>%
  deframe()

names(SIC) <- names(sic_counts_names)

save(SIC, file = '../misc/SIC.rda')

### AND SEE  EJAM/data-raw/datacreate_sictable.R
