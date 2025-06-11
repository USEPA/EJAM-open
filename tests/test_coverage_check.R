
# see  https://covr.r-lib.org/
  
# see  RStudio addin that does  covr::report()   ?covr::package_coverage()  


################################ # 

# below is a draft function to get a very basic check on 
# which functions seem to clearly have unit test files already


test_coverage_check <- function() {
  
  # fs pkg is needed only for this script (but could be replaced by base or utils functions?) 
  library(fs)
  require(tidyverse)
  
  cat("Looking in the source package EJAM/R/ folder for files like xyz.R, and in the EJAM/tests/testthat/ folder for test files like test-xyz.R \n")
  tdat = bind_rows(
    tibble(
      type = "R",
      path = dir_ls("R/", regexp = "\\.[Rr]$"),
      name = as.character( fs::path_ext_remove(path_file(path))),
    ),
    tibble(
      type = "test",
      path = fs::dir_ls("tests/testthat/", regexp = "/test[^/]+\\.[Rr]$"),
      name = as.character( fs::path_ext_remove(str_remove( fs::path_file(path), "^test[-_]"))),
    )
  ) %>%
    pivot_wider(names_from = type, values_from = path) 
  ################################ # 
  cat("\n\nCOVERAGE CHECK \n\n")
  # tdat %>%   print(n = Inf) # to see everything
  ################################ # 
  
  cat(' -----------------------------------------------
  
  All test files that were not named based on a .R file, 
     making it hard to know which .R files really lack tests:\n\n')
  
  tdat[!is.na(tdat$test) & is.na(tdat$R),  ] |> print(n = 500)
  ################################ # 
  
  cat(" -----------------------------------------------
  
      All .R files that lack a test file with exactly matching name:\n\n")
  
  tdat[is.na(tdat$test) & !is.na(tdat$R) & "data_" != substr(tdat$name, 1,5), ] |> print(n = 500)
  ################################ # 
  
  cat(' -----------------------------------------------
  
      MATCHED EXACTLY -- all test files that exactly match name of a .R file: \n\n')
  
  tdat[!is.na(tdat$test) & !is.na(tdat$R), c("R", "test")] |> print(n = 500)
  ################################ # 
  cat('
      -----------------------------------------------\n\n')
  
  invisible(tdat)
}


## to use this function:
#
#  tdat <-  test_coverage_check() # to get report and see key info
#  tdat %>%   print(n = Inf) # to see everything



## also see 

# y = functions_in_pkg('EJAM')

## and 

# x = analyze.stuff::linesofcode('./..', packages = 'EJAM') 

