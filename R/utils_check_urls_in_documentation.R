
#' @noRd
#' 
util_check_urls_in_documentation <- function() {
  
  cat(' search in each .R file, or use ctrl-shift-f, to search for this: \n\n')
  cat(paste0(" grep(", '"',  "^#", "'", '.*[^<]http", x) ',  "\n "))
  cat('\n\n')
  
  cat(paste0("  ",   "^#", "'", '.*[^<]http  ',  "\n "))
  cat("could go in search window for find in all files. \n\n")
  
  cat("
 Also note there can be strange problems in what gets put into NAMESPACE if you comment out code 
 by putting a # sign immediately before a single quote, like if you had 'text' and put a # before it. 
 roxygen seems to interpret that as roxygen markup in some cases, even though it is buried inside a function. 
 Search in RStudio in all files via ctrl-shift-f and mark it as Regex and type \n",
paste0("^.+#", "'"), "
 in the search box.

")
  
}



