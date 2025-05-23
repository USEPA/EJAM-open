## code to prepare `naicstable` dataset goes here

# library(data.table)
# EJAM::NAICS is where NAICS was

naicstable <- data.table(code = as.vector(NAICS), name = names(NAICS))
naicstable[ , num_name := trimws(name)]
naicstable[ , name := trimws(gsub(".* - ", "", name))]

naicstable[ , n2 := substr(code,1,2)]
naicstable[ , n3 := substr(code,1,3)]
naicstable[ , n4 := substr(code,1,4)]
naicstable[ , n5 := substr(code,1,5)]
naicstable[ , n6 := substr(code,1,6)]

# data.table::setcolorder(naicstable, neworder = c(""))
naicstable <- naicstable[ , .(code, n2, n3, n4, n5, n6, name, num_name)]

attr(naicstable, "date_saved_in_package") <- as.character(Sys.Date())

usethis::use_data(naicstable, overwrite = TRUE)

dataset_documenter("naicstable",
                   title = "naicstable (DATA) data.table of NAICS code(s) and industry names for each EPA-regulated site",
                   description = "data.table of NAICS code(s) and industry names for each EPA-regulated site in Facility Registry Service
#' Also has the 2,3,4,5,and 6-digit NAICS that this code falls under, where relevant for given length",
                   seealso = "[EJAM::naics_from_any()] [EJAM::NAICS]  [EJAM::naics_categories()]  [EJAM::naics_findwebscrape()]",
                   details = "This is similar to the data file EJAM::NAICS but in a more useful format and newer functions work with it.
#' see [NAICS.com](https://naics.com)")

# table(nchar(naicstable$code))
# 
#  2    3    4    5    6 
# 24   99  311  709 1057 

# > naicstable
#      code n2  n3   n4    n5     n6                                       name                                        num_name
# 1:     11 11  11   11    11     11 Agriculture, Forestry, Fishing and Hunting 11 - Agriculture, Forestry, Fishing and Hunting
# 2:    111 11 111  111   111    111                            Crop Production                           111 - Crop Production
# 3:   1111 11 111 1111  1111   1111                  Oilseed and Grain Farming                1111 - Oilseed and Grain Farming
# 4:  11111 11 111 1111 11111  11111                            Soybean Farming                         11111 - Soybean Farming
# 5: 111110 11 111 1111 11111 111110                            Soybean Farming                        111110 - Soybean Farming
# ---                                                                                                                           
# 2196:     33 33  33   33    33     33                              Manufacturing                              33 - Manufacturing
# 2197:     44 44  44   44    44     44                               Retail Trade                               44 - Retail Trade
# 2198:     45 45  45   45    45     45                               Retail Trade                               45 - Retail Trade
# 2199:     48 48  48   48    48     48             Transportation and Warehousing             48 - Transportation and Warehousing
# 2200:     49 49  49   49    49     49             Transportation and Warehousing             49 - Transportation and Warehousing
# >
