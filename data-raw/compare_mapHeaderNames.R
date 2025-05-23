library(readxl)
library(dplyr)
library(writexl)

#Script to compare map_headernames files after making changes
#Can get the latest development map_headernames at:
# EJAM/blob/development/data-raw/map_headernames_2.32.xlsx or 2.3.2 etc.

compare_xls <- function(old_file, new_file, output_file, sheet1 = 1, sheet2 = 1) {
  df_old <- read_excel(old_file, sheet = sheet1)
  df_new <- read_excel(new_file, sheet = sheet2)
  
  if (!"n" %in% colnames(df_old) | !"n" %in% colnames(df_new)) {
    stop("Both excels need the column n'")
  }
  
  df_old$n <- as.character(df_old$n)
  df_new$n <- as.character(df_new$n)
  
  only_in_old <- anti_join(df_old, df_new, by = "n")
  only_in_new <- anti_join(df_new, df_old, by = "n")
  
  common_n <- inner_join(df_old, df_new, by = "n", suffix = c("_old", "_new"))

  differing_rows <- common_n %>%
    filter(rowSums(select(., ends_with("_old")) != select(., ends_with("_new")), na.rm = TRUE) > 0) %>%
    mutate(across(ends_with("_old"), ~ ifelse(. != get(sub("_old", "_new", cur_column())), ., NA))) %>%
    mutate(across(ends_with("_new"), ~ ifelse(. != get(sub("_new", "_old", cur_column())), ., NA))) %>%
    select(n, where(~ any(!is.na(.))))  
  

  write_xlsx(list(
    "Only_in_Old" = only_in_old,
    "Only_in_New" = only_in_new,
    "Differences" = differing_rows
  ), output_file)
  
}



oldMapHeaderNames <- 
newMapHeaderNames <- 
output_file <- "comparisonResults.xlsx"

compare_xls(oldMapHeaderNames, newMapHeaderNames, output_file)
