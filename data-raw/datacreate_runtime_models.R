#File to create models for ejamit and doaggregate analysis

results <- read.csv("data-raw/Analysis_timing_results_100.csv", stringsAsFactors = FALSE)


filtered <- results %>% filter(time_ejamit != 0)
modelEjamit <- lm(time_ejamit ~ input_number + I(radius^2) + I(radius^2*input_number), data = filtered)
usethis::use_data(modelEjamit, internal = FALSE, overwrite = TRUE)

filtered <- results %>% filter(time_doaggregate != 0)
modelDoaggregate <- lm(time_doaggregate ~ nrows_blocks, data = filtered)
usethis::use_data(modelDoaggregate, internal = FALSE, overwrite = TRUE)


EJAM:::dataset_documenter("modelDoaggregate",
                   title = "Regression model to predict runtime for doaggregate",
                   description = "Modeled runtime for doaggregate and ejamit based off 100 runs with random parameters. Use these models to make predictions in app_server",
                   details = "Ejamit's runtime is modeled off radius and number of rows of input dataset, doaggregate runtime is modeled off rows of getblocksnearby output")

EJAM:::dataset_documenter("modelEjamit",
                   title = "Regression model to predict runtime for ejamit",
                   description = "Modeled runtime for doaggregate and ejamitbased off 100 runs with random parameters. Use these models to make predictions in app_server",
                   details = "Ejamit's runtime is modeled off radius and number of rows of input dataset, doaggregate runtime is modeled off rows of getblocksnearby output")
