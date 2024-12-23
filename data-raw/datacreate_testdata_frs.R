
## script for recreating test frs upload files
## should be re-run whenever frs.arrow data file is updated

# also see  testids_registry_id

## allow large numbers without scientific notation
options(scipen = 999)

## load frs data 
dataload_from_pins('frs')

## set location to save test files
test_folder <- 'inst/testdata/registryid'

## set random seed for replicable sampling
set.seed(as.numeric(as.Date('2024-07-09')))

## set sample sizes of test point objects
nvec <- c(10, 100, 1000, 10000, 100000)

for (n in nvec) {
  frs_cur <- testpoints_n(n = n, weighting = 'frs', validonly = TRUE)
  #assign(paste0('frs_', n), frs_cur)
  frs_out <- data.frame(num = 1:n, REGISTRY_ID = as.numeric(frs_cur$REGISTRY_ID))
  
  write.csv(x = frs_out, file = paste0(test_folder, '/frs_testpoints_',n,'.csv'), row.names = FALSE)
  writexl::write_xlsx(x = frs_out, path = paste0(test_folder, '/frs_testpoints_',n,'.xlsx'))
}

options(scipen = 0)
# 
# frs_10 <- testpoints_n(n = 10, weighting = 'frs', validonly = TRUE)
# frs_100 <- testpoints_n(n = 100, weighting = 'frs', validonly = TRUE)
# frs_1000 <- testpoints_n(n = 1000, weighting = 'frs', validonly = TRUE)
# frs_10000 <- testpoints_n(n = 10000, weighting = 'frs', validonly = TRUE)
# frs_100000 <- testpoints_n(n = 100000, weighting = 'frs', validonly = TRUE)



# write.csv(x = frs_10, file = file.path(test_folder, 'frs_testpoints_10.csv'), row.names = FALSE)
# writexl::write_xlsx(x = frs_10, path = 'frs_testpoints_10.xlsx')
