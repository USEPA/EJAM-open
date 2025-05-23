# 

#  EJAM/data-raw/datacreate_names_of_indicators.R

library(usethis)
########################################################################################################## #
# ***
#   to be eventually replaced with a newer approach... see issue #491
# FOR NOW we put variable names in 3 places:
#  one big named list, namez
#  the varlist column of  map_headernames
#  in multiple objects like names_d 
#
#  But could recode later to use namez$d instead of names_d etc.

#  and/or could recode later to use 1 big table, like A MERGED VERSION OF THESE:
# 
#  map_headernames (which is created from .xlsx using the script in /data-raw folder)
#  namez 
#  formulas_all as a possible approach using calc_ejam()

# In the short term, before some mapping file is used to track all variable names, we could simplify a bit by 
# recoding all functions to use namez$d  instead of names_d, etc., 
#  so there is only 1 .rda file and can see all names in 1 command or file,
#  Would need to replace all   names_([a-z|_]*)  with  namez$\1   in all the .R files (functions in EJAM package)
#  and then code to create all the small .rda files via usethis::use_data() in \data-raw\names_of_indicators.R will be obsolete,
#  and we can get rid of all those small data objects in the /EJAM/data/ folder
#    via   something like this:
# file.remove( list.files(path = "./data/", pattern = "names_", full.names = TRUE))  #  
#
# cat('\n', paste(paste0("names_", names(EJAM::namez)), collapse = "\n"), '\n\n'); cat('\n\n'); 
# cat(paste(paste0("names_", names(EJAM::namez)), collapse = ", "))

## note subgroups info is tricky since $varlist was "names_d_subgroups_nh" not "names_d_subgroups" 

########################################################################################################## #
########################################################################################################## #

datacreate_names_of_indicators <- function(redohelp = FALSE) {
  
  ### this will create but also assign metadata to and save for pkg via use_data()
  ### unlike some other datacreate_  functions that do not do the metadata and use_data steps!
  ### It is really kind of a script, but packaged as a function here so that
  ### all the variables created do not show up in the global environment - they get saved in pkg ready for lazy-loading if/when needed

  ################################################################ # 
  # Below creates all the lists of variable names and save them to the package as lazyloaded datasets
  # relying entirely on map_headernames$varlist and map_headernames$rname
  # 
  ## note these are the same:  all(names_climate == names_from_varlist("names_climate"))
  
  # to create ALL the lists all at once
  # 
  # load_all() # needs EJAM to work, including map_headernames has to be available
  ## some are left out of names_all_r as somewhat redundant
  # setdiff(unique(map_headernames$varlist), unique(varinfo(names_all_r, 'varlist')$varlist))
  ## some are not yet or never will be included in outputs
  # setdiff(unique(map_headernames$varlist),   (unique(varinfo(names(testoutput_ejamit_10pts_1miles$results_overall) , 'varlist')$varlist))   )
  
  vlists <- unique(map_headernames$varlist)
  # file.exists(paste0('R/data_', vlists, '.R'))
  
  # assign all the lists of variable names to the global environment
  for (i in seq_along(vlists)) {
    vns <- varlist2names(vlists[i])
    if (anyDuplicated(vns)) {
      warning(paste0("check order of indicators in ", vlists[i], " because duplicates had to be removed from it." ))
      vns = unique(vns)
    }
    assign(vlists[i], vns)
  }
  # assign metadata to all those lists
  for (i in seq_along(vlists)) {
    assign(vlists[i], metadata_add(get(vlists[i])))
  }
  # save all those lists to the package as lazyloaded datasets
  # golem::detach_all_attached() # to avoid any lazyloaded versions of these objects
  for (i in seq_along(vlists)) {
    # for (i in 1:2) {
    codetosource <- paste0("usethis::use_data(", vlists[i], ", overwrite=TRUE)")
    eval(parse(text = codetosource)) 
  }
  # make placeholders for any missing documentation of those datasets
  # - note this does not remove obsolete or update existing documentation!!
  
  ## avoid so many files of documentation... 
  ## see https://jsta.rbind.io/blog/automated-roxygen-documentation-of-r-package-data/
  ## see https://roxygen2.r-lib.org/reference/tags-reuse.html
  for (i in seq_along(vlists)) {
    fname <- paste0("./R/data_", vlists[i], ".R")
    if ((redohelp & file.exists(fname)) | !file.exists(fname)) {
      # if (file.exists(fname)) {file.remove(fname)} # trying to reset problem with git tracking it as binary before dataset_documenter() was fixed to handle that
      cat(paste0("Creating basic documentation: data_", vlists[i], ".R \n"))
      keepinTOC <-  c('names_d', 'names_e') 
      keywordsinternal <- ifelse(vlists[i] %in% keepinTOC, "", "\n#' @keywords internal")
      dataset_documenter(vlists[i], 
                         title = "a list of variable names for internal use in EJAM",
                         details = paste0("To see other groups of variables: unique(map_headernames$varlist)", 
                                          keywordsinternal),
                         seealso = " [varinfo()]  [map_headernames]  [names_d] [names_e]")
    }
  }
  rm(vns, i, codetosource, vlists)
  
  # setdiff(unique(map_headernames$varlist), unique(varinfo(names_all_r, 'varlist')$varlist))
  
  ############################################################################## ############################################################################### #
  
  # names_wts  so that .R documentation gets done etc but would cause problem below if done here so
  warning("need to fix names_wts if not fixed yet")
  # names_wts <- "pop"  # it should be put in map_headernames$varlist
  # names_wts <- metadata_add(names_wts)
  # use_data(names_wts, overwrite = TRUE)
  # dataset_documenter("names_wts", "a list of variable names for internal use in EJAM")
  
  # **names_these_ ####
  
  names_these                    <- c(names_d,              names_d_subgroups,              names_e)
  names_these_avg                <- c(names_d_avg,          names_d_subgroups_avg,          names_e_avg)                         # <- paste0("avg.",       names_these) #
  names_these_state_avg          <- c(names_d_state_avg,    names_d_subgroups_state_avg,    names_e_state_avg)  # paste0("state.avg.", names_these)
  names_these_ratio_to_avg       <- c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg, names_e_ratio_to_avg)      #<-  paste0("ratio.to.", names_these_avg )
  names_these_ratio_to_state_avg <- c(names_d_ratio_to_state_avg,  names_d_subgroups_ratio_to_state_avg,    names_e_ratio_to_state_avg)  # <-  paste0("ratio.to.", names_these_state_avg)
  
  names_these <- metadata_add(names_these) 
  names_these_avg <- metadata_add(names_these_avg) 
  names_these_state_avg <- metadata_add(names_these_state_avg) 
  names_these_ratio_to_avg <- metadata_add(names_these_ratio_to_avg) 
  names_these_ratio_to_state_avg <- metadata_add(names_these_ratio_to_state_avg) 
  # pctile and friendly not used here
  
  use_data(names_these, overwrite = TRUE)
  use_data(names_these_avg, overwrite = TRUE)
  use_data(names_these_state_avg, overwrite = TRUE)
  use_data(names_these_ratio_to_avg, overwrite = TRUE)
  use_data(names_these_ratio_to_state_avg, overwrite = TRUE)
  
  ############################################################################## #
  
  # **names_all_r ####
  
  names_all_r <- varlist2names(sapply(ls(pattern = "^names_"), c)) # all the ones that are in lists already
  names_all_r <- sort(unique(names_all_r))
  names_all_r <- metadata_add(names_all_r)
  use_data(names_all_r, overwrite = TRUE)
  
  ############################################################################## #
  # y = askYesNo("Do you want to continue and create the metadata for all the lists of variable names and save them to the package as lazyloaded datasets?")
  # if (is.na(y) || !y) {return()} 
  
  # **namez__ ####
  
  # try putting these all in one list instead of multiple objects? could recode later to use namez$d instead of names_d  etc.
  # and/or    just store them in a big table
  
  namesoflistsofnames = c('names_all_r', sort(unique(map_headernames$varlist)))
  namez <- lapply(namesoflistsofnames, get)
  names(namez) <- gsub("^names_","", namesoflistsofnames)
  #  metadata_add & USE_DATA # 
  namez <- metadata_add(namez)
  usethis::use_data(namez, overwrite = TRUE)
  ############################################################################## #
  
  # **names_all ####
  
  # NOTE THIS IS VERY DIFFERENT THAN names_all_batch !!
  names_all <- as.vector(unlist(namez))
  names_all <- unique(names_all) # pop would appear twice
  names_all <- metadata_add(names_all)
  use_data(names_all, overwrite = TRUE)
  
  ############################################################################## #
  
  # check what was not created
  
  namesoflistsofnames <- c('names_all', namesoflistsofnames)
  for (vl in unique(map_headernames$varlist)) {
    if (nchar(vl) > 0 && !exists(vl)) {warning(paste0(vl, " was not created as a data object."))}
  }
  ############################################################################## #
  
  # create other missing documentation ####
  
  vlists =  sapply(ls(pattern = "^names_"), c)
  
  for (i in seq_along(vlists)) {
    if (!file.exists(paste0("./R/data_", vlists[i], ".R"))) {
      cat(paste0("Creating documentation placeholder: data_", vlists[i], ".R \n"))
      dataset_documenter(vlists[i], title = "a list of variable names for internal use in EJAM")
    }
  }
  ############################################################################## #
  
  ## check if usastats and statestats colnames match names_these ####
  
  if (exists("usastats") && exists("statestats")) {
    cat("checking new names_these vs colnames of whatever versions of usastats and statestats are attached or just created or else lazy loaded from installed pkg\n")
    notfound    = setdiff(names_these, names(usastats))   # uses attached possibly new version if different than installed version. fails if pkg not attached and new usastats not just made
    notfound_st = setdiff(names_these, names(statestats)) # ditto
    if (length(notfound   ) > 0) {warning('some of names_these are not column names found in usastats  ... ',        paste0(notfound,    collapse = ', '), '\n')} else {print('ok')}
    if (length(notfound_st) > 0) {warning('some of names_these are not column names found in statestats  ... ',      paste0(notfound_st, collapse = ', '), '\n')} else {print('ok')}
    rm(notfound, notfound_st)
  } else {
    warning("did not check if all names_these are in names(statestats) and names(usastats) because usastats or statestats is missing")
  }
  if (exists("blockgroupstats")) {
    cat("checking new names_these vs colnames of whatever versions of blockgroupstats is attached or just created or else lazy loaded from installed pkg\n")
    notfound_bg = setdiff(names_these, names(blockgroupstats))   # ditto
    if (length(notfound_bg) > 0) {warning('some of names_these are not column names found in blockgroupstats  ... ', paste0(notfound_bg, collapse = ', '), '\n')} else {print('ok')}
    rm(notfound_bg)
  }
  ############################################################################## #
  
  return(names_these) 
  
}

# USE THE FUNCTION #### 

datacreate_names_of_indicators()    # this does metadata and use_data inside the function

rm(datacreate_names_of_indicators)

cat("FINISHED A SCRIPT\n")
cat("\n In globalenv() so far: \n\n")
print(ls())
