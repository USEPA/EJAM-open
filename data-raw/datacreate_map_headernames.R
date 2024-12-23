if (!exists("askquestions")) {askquestions <- FALSE}
if (!exists("rawdir")) {rawdir <- './data-raw'}

#createorupdatethetablethatmapsfromoneversionof
#variablenames(e.g.,long,clearerones)
#toanother(e.g.,shortereasierforanalysisorprogramminginR,etc.)

datacreate_map_headernames <- function(rawdir = "./data-raw", fname = 'map_headernames_2.32.xlsx', sheet = "map_headernames") {
  
  fpath <- file.path(rawdir, fname)
  if (!file.exists(fpath)) {stop("did not find (but this requires) ", fpath)}
  
  map_headernames <- as.data.frame(readxl::read_xlsx(fpath, sheet = sheet))
  
  map_headernames[is.na(map_headernames)] <- ''  #changeNAvaluestoemptycell,soitiseasiertosubsetetc.
  
  cat('must redo sample dataset outputs in EJAM/inst/testdata/  via
  EJAM/data-raw/datacreate_testpoints_testoutputs.R
  and possibly also EJAMejscreen/ files via
  datacreate_testoutput_ejscreenit_or_ejscreenapi_plus_50.R
      \n')
  
  # cbind(names(map_headernames))
  invisible(map_headernames)
}
################################################################################# #

#  UPDATE map_headernames_2.32.xlsx MANUALLY, 
#  then read .xlsx and save as dataset for package
if (askquestions && interactive()) {
  y <- askYesNo("Want to open .xlsx to edit it now?")
  if (!is.na(y) && y) {
    fpath = rstudioapi::selectFile(path = rawdir, filter = "xlsx")
    browseURL(fpath)
    y <- askYesNo("Y if done editing and ready to go on, N to abort/stop")
    if (is.na(y) || !y) {stop("stopping script")} 
  }
}
if (!exists("fpath")) {
  map_headernames <- datacreate_map_headernames()
} else {
  map_headernames <- datacreate_map_headernames(fpath)
}

map_headernames <- metadata_add(map_headernames)
usethis::use_data(map_headernames, overwrite = TRUE)

rm(datacreate_map_headernames)
rm(y)
cat("FINISHED A SCRIPT\n")
cat("\n In globalenv() so far: \n\n")
print(ls())
################################################################################# #

# # which sources provide which variables or indicators? 

some = unique(map_headernames$rname[map_headernames$varlist != "" & map_headernames$varlist != "x_anyother"])
info = varinfo(some, info = c('api', 'csv', 'acs', 'varlist'))
x = info[nchar(paste0(info$api, info$csv, info$acs)) > 0, ]
cat("\nSee a table of which source (api, csv, etc.) uses which variable names\n\n")
cat( 
"some = unique(map_headernames$rname[map_headernames$varlist != '' & map_headernames$varlist != 'x_anyother']) \n",
"info = varinfo(some, info = c('api', 'csv', 'acs', 'varlist'))\n",
"x = info[nchar(paste0(info$api, info$csv, info$acs)) > 0, ]",
"head(x)",
"\n\n")
head(x)
