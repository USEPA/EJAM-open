
#' Utility to load a couple of datasets using data immediately instead of relying on lazy loading 
#' 
#' @details
#'  See also read_builtin() function from the readr package!
#' 
#'  Default is to load some but not all the datasets into memory immediately.
#'   [blockgroupstats], [usastats], [statestats], and some others are always essential to EJAM, but 
#'   [frs] and [frs_by_programid] are huge datasets (and [frs_by_sic] and [frs_by_naics])
#'    and not always used - only to find regulated facilities by ID, etc. 
#'    The frs-related datasets here can be roughly 1.5 GB in RAM, perhaps.
#' @param olist vector of strings giving names of objects to load using data(). 
#'   This could also include other large datasets that are slow to lazyload but not always needed: 
#'   "frs", "frs_by_programid ", "frs_by_naics", etc.
#' @param envir the environment into which they should be loaded
#' @return Nothing
#' @seealso [datapack()] [dataload_from_aws()] [dataload_from_pins()] [dataload_from_local()] [indexblocks()] [.onAttach()] 
#' @examples 
#'   x <- datapack("EJAM")
#'   subset(x, x$size >= 0.1) # at least 100 KB
#'   grep("names_", x$Item, value = T, ignore.case = T, invert = T) # most were like names_d, etc.
#'   ls()
#'   data("avg.in.us", package="EJAM") # lazy load an object into memory and make it visible to user
#'   ls()
#'   rm(avg.in.us, x)
#'   
#' @keywords internal
#' 
dataload_from_package <- function(olist = c("blockgroupstats", "usastats", "statestats"), envir=globalenv()) {
  
  # check if all of olist exist within the package as data! 
  
  data(list = olist, package = "EJAM", 
       envir = envir
  )
  
  # data(list=c("frs", "frs_by_programid ", "frs_by_naics"), package="EJAM") # would be to preload some very large ones not always needed. 
  
  # get full path and name for data file in locally installed package?
  # system.file("/data/blockgroupstats.rda", package="EJAM")
  # this works on a local source package, only:
  # fullnames <- list.files('./data', pattern = '\\.rda$' , full.names = F)
  # f.rda <- 
  #   oname <- gsub(".rda", "", f.rda)
  
  # Note:
  #  See also read_builtin() function from the readr package!
  # 
  #   Use of data within a function without an envir argument has the 
  # almost always undesirable side-effect of putting an object in the user's workspace 
  # (and indeed, of replacing any object of that name already there). 
  # It would almost always be better to put the object in the current evaluation environment 
  # by data(..., envir = environment()). However, two alternatives are usually preferable, both described in the ‘Writing R Extensions’ manual.
  # 
  # For sets of data, set up a package to use lazy-loading of data. (But that causes a delay when it is needed)
  # 
  # For objects which are system data, for example lookup tables used in calculations within the function, 
  #  use a file ‘R/sysdata.rda’ in the package sources
  #  or create the objects by R code at package installation time.
  # 
  # A sometimes important distinction is that the second approach places objects in the namespace but the first does not. 
  # So if it is important that the function sees mytable as an object from the package, it is system data and the second approach should be used.
}
