
library(magrittr) # or maybe can now use |>  that is built into R 

## compute counts without subcodes 
naics_counts_nosub <- frs_by_naics[, .N, by = 'NAICS']



## compute counts with subcodes - extremely slow if done like this


naics_counts_w_subs <- sapply(NAICS, function(x) {frs_from_naics(x, childrenForNAICS = T)[, .N]})

# that does this
# frs[REGISTRY_ID %in% regid_from_naics(naics_from_any(naics_code_or_name, ...)$code, id_only = TRUE) , ]
# which does...
# naics_from_code ie
# results <- naicstable[code %in% mycodes, ] # this does not preserve order of mycodes queried, but cannot use match which would return only 1st match. 
# if (children) {
#   # add subcategories
#   results <- naics_subcodes_from_code(results$code)
# }



# a faster way to do that using data.table is something like the following:

# naics_counts_w_subs <- frs_by_naics[, .N, by = ????  c('NAICS', 'NAICS_description') ??? ]





## make counts data.frame
naics_counts <- data.frame(NAICS, count_w_subs = naics_counts_w_subs)

## join and add counts to labels
naics_counts <- tibble::enframe(NAICS,value = 'NAICS') %>% 
  dplyr::left_join(naics_counts) %>% 
  dplyr::left_join(naics_counts_nosub) %>% 
  dplyr::rename(count_no_subs = N) %>% 
  dplyr::mutate(label_w_subs = ifelse(.data$count_w_subs > 0, 
                                      paste0(.data$name, ' (', 
                                             prettyNum(.data$count_w_subs, big.mark = ','),' sites)'
                                      ),
                                      .data$name
  ),
  label_no_subs = ifelse(!is.na(count_no_subs) & count_no_subs > 0,
                         paste0(name, ' (', 
                                prettyNum(count_no_subs, big.mark = ','),' sites)'),
                         name)
  )

## save to EJAM package dataset

# naics_counts <- metadata_add(naics_counts)
attr(naics_counts, "date_saved_in_package") <- as.character(Sys.Date())
usethis::use_data(naics_counts, overwrite = TRUE)

dataset_documenter("naics_counts",
                   title = "naics_counts (DATA) data.frame with regulated facility counts for each industry code",
                   description = "data.frame with regulated facility counts for each NAICS code, with and without subcodes, and labels that include the site counts",
                   details = "This has all available NAICS codes, the count of sites for each of them in the frs data, both on their own and including all subcodes. Used by EJAM shiny app for dropdown menu.")
