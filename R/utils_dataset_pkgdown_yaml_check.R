# actually now checks more than just what is in yml file 
# and note yaml vs yml is inconsistent

dataset_pkgdown_yaml_check = function(fname = "_pkgdown.yml") {
  
  cat("Doing dataset_pkgdown_yaml_check()\n")
  cat('also see draft func_has_keywords_internal_tag() \n')
  cat("Checking yml file for reference metadata as in pkgdown_sitrep() ...\n")
  #pkgdown:::error_to_sitrep("Reference metadata", pkgdown:::data_reference_index(as_pkgdown('.')))
  pkgdown::pkgdown_sitrep('.') 
  # pkgdown_sitrep() reports status of all checks, while check_pkgdown() would do same checks but error at 1st problem.
  cat("\n")
  cat("Checking ", fname, "\n")
  
  # 1  in _pkgdown.yml 
  inyaml  = readLines(fname)
  inyaml = unlist(strsplit(inyaml, "\n"))
  inyaml = gsub("  - \\'.*", '', inyaml)
  inyaml = gsub("  - ", "", inyaml)
  inyaml = gsub(".*:.*", "", inyaml)
  inyaml = gsub("^  .*", "", inyaml)
  inyaml = inyaml[inyaml != ""]
  inyaml = unique(inyaml)
  
  # 2 IN SOURCE PACKAGE /data/xyz.rda FILENAMES
  cat("Checking /data/*.rda \n")
  inrda = list.files("./data", pattern = ".rda$")
  inrda = gsub("\\.rda$", "", inrda)
  
  # 3 DOCUMENTED IN SOURCE AS ITS OWN /R/data_XYZ.R FILE
  cat("Checking /R/data_*.R \n")
  inrfiles <- list.files("./R/", pattern = "^data_.*\\.R$")
  inrfiles <- gsub("^data_|\\.R$", "", inrfiles)
  inrfiles <- inrfiles[!grepl("aaaaa", inrfiles)]
  inrfiles <- inrfiles[!grepl("xxxxx", inrfiles)]
  
  # 4  DATASETS IN INSTALLED PACKAGE
  cat("Checking data() via datapack() \n")
  inpkg = datapack("EJAM")
  inpkg = inpkg$Item
  ## also includes things like .onAttach if you do it this way:
  # inpkg2 = functions_in_pkg(pkg = 'EJAM', internal_included = T, data_included = T, exportedfuncs_included = F)
  # inpkg2 = inpkg2$object

  cat('\n')
  ############# #  ############# #  ############# #  ############# #  ############# #
  
  cat("\n ------------------- NOT YET IN _pkgdown.yml ------------------- \n\n")
  
  ############# #
  # 2,1
  # setdiff(inrda, inyaml)
  cat("\n IN *SOURCE* PACKAGE /data/xyz.rda FILENAMES, \n BUT NOT YET IN _pkgdown.yml \n\n")
  miss <- setdiff(inrda, inyaml)
  if (length(miss) > 0) {
  print(data.frame(item = miss))
  } else {
    cat("    (NA / none missing)\n")
  }
  
  # 1,2
  # setdiff(inyaml, inrda) 
  #  not useful since   ############# # functions in yaml of course dont have .rda files, and also shows bgej etc.
  # cat("\n NO SOURCE PACKAGE /data/xyz.rda FILE BUT STILL IS IN _pkgdown.yml \n\n")
  # print(data.frame(setdiff(inyaml, inrda)))

  ############# #
  # 3,1
  # setdiff(inrfiles, inyaml)
  # # rfiles not yaml   also shows bgej etc.  
   cat("\n IN *SOURCE* PACKAGE DOCUMENTED via its own /R/data_XYZ.R FILE, \n BUT NOT YET IN _pkgdown.yml \n\n")
   miss <- setdiff(inrfiles, inyaml)
   missing_info_to_return <- miss
   if (length(miss) > 0) {
     print(data.frame(item = miss))
   } else {
     cat("    (NA / none missing)\n")
   }
   
  # 1,3
  # setdiff(inyaml, inrfiles)
  # yaml not rfiles  not useful since all the functions are in yaml etc

  ############# #
  # 4,1
  # setdiff(inpkg, inyaml)
  cat("\n IN INSTALLED PACKAGE AS A DATASET found via data(), \n BUT NOT YET IN _pkgdown.yml\n")
  miss = setdiff(inpkg, inyaml) # as.vector(unlist(sapply(inpkg, function(z) if (!any(grepl(z, inyaml))) (z)  )))
  if (length(miss) > 0) {
  cat("
  e.g., in the 'internal' section of the _pkgdown.yml file, one could add the following lines:\n
- title: internal
  contents:\n")
  cat(paste0(paste0("  - ", miss), collapse = "\n"), '\n\n')
  } else {
    cat("    (NA / none missing)\n")
  }
  
  # 1,4
  # setdiff(inyaml, inpkg)
  # yaml not pkg  not useful since all the functions are in yaml, and also shows bgej etc.
  
  ############# #  ############# #  ############# #  ############# #  ############# #
  
  cat("\n ------------------- NOT DOCUMENTED via its own /R/data_XYZ.R FILE ------------------- \n")
  
  ############# #
  # 2,3
  # setdiff(inrda, inrfiles) 
  # check for source pkg data undocumented in src
  cat("\n IN *SOURCE* PACKAGE /data/xyz.rda FILENAMES, \n BUT NOT DOCUMENTED via its own /R/data_XYZ.R FILE
      (which is OK if its alias is documented or a few objects are documented in one .R file)\n\n")
  miss <- setdiff(inrda, inrfiles)
  if (length(miss) > 0) {
    print(data.frame(item = miss))
  } else {
    cat("    (NA / none missing)\n")
  }
  
  ###### ####### #
  
  # 4,3
  # setdiff(inpkg, inrfiles)
  # check for installed pkg data undocumented in src
  cat("\n IN INSTALLED PACKAGE AS A DATASET found via data(), \n BUT NOT DOCUMENTED via its own /R/data_XYZ.R FILE
      (which is OK if its alias is documented or a few objects are documented in one .R file)\n\n")
  miss <- setdiff(inpkg, inrfiles)
  if (length(miss) > 0) {
    print(data.frame(item = miss))
  } else {
    cat("    (NA / none missing)\n")
  }
  ############# #  ############# #  ############# #  ############# #  ############# #
  
  cat("\n ------------------- NOT IN *SOURCE* PACKAGE /data/xyz.rda ------------------- \n") 
  
  ############# #
  # 4,2
  #  setdiff(inpkg, inrda)
  cat("\n DATASETS IN INSTALLED PACKAGE, \n BUT NOT IN *SOURCE* PACKAGE /data/xyz.rda FILENAMES,
      (e.g., a file within the data folder is a .txt not .rda file) \n\n")
  miss <- setdiff(inpkg, inrda)
  if (length(miss) > 0) {
    print(data.frame(item = miss))
  } else {
    cat("    (NA / none missing)\n")
  }
  
  ############# #
  # 3,2
  # setdiff(inrfiles, inrda)
  cat("\n DOCUMENTED IN *SOURCE* via its own /R/data_XYZ.R FILE,\n BUT NOT IN *SOURCE* PACKAGE /data/xyz.rda FILENAMES,
    Some large datasets are documented even though they are not installed with the package\n    (they get downloaded as needed)\n\n")
  miss <- setdiff(inrfiles, inrda)
  if (length(miss) > 0) {
    print(data.frame(item = miss))
  } else {
    cat("    (NA / none missing)\n")
  }
  ############# #  ############# #  ############# #  ############# #  ############# #
  
  cat("\n ------------------- NOT IN INSTALLED PACKAGE AS A DATASET found via data() ------------------- \n") 
  
  ### ###### #
  # 3,4
  # setdiff(inrfiles, inpkg)
  cat("\n DOCUMENTED IN *SOURCE* via its own /R/data_XYZ.R FILE, \n BUT NOT IN INSTALLED PACKAGE AS A DATASET found via data(),
    Some large datasets are documented even though they are not installed with the package\n    (they get downloaded as needed)\n\n")
  miss <- setdiff(inrfiles, inpkg)
  if (length(miss) > 0) {
    print(data.frame(item = miss))
  } else {
    cat("    (NA / none missing)\n")
  }
  ############# #
  # 2,4
  # setdiff(inrda, inpkg)
  cat("\n IN *SOURCE* PACKAGE /data/xyz.rda FILENAMES,\n BUT NOT IN INSTALLED PACKAGE AS A DATASET found via data()\n\n")
  miss <- setdiff(inrda, inpkg)
  if (length(miss) > 0) {
    print(data.frame(item = miss))
  } else {
    cat("    (NA / none missing)\n")
  }
  ############# #  ############# #  ############# #  ############# #  ############# #
  
  cat("---------------------------------------------------------- \n\n")
  
  invisible(missing_info_to_return)
}
