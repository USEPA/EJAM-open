# actually now checks more than just what is in yml file 
# and note yaml vs yml is inconsistent

dataset_pkgdown_yaml_check = function(fname = "_pkgdown.yml") { 
  
  # pkgdown::pkgdown_sitrep()
  cat("Checking yml file for reference metadata as in pkgdown_sitrep() ...\n")
  pkgdown:::error_to_sitrep("Reference metadata", pkgdown:::data_reference_index(as_pkgdown('.')))
  cat("\n\n")
  
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
  inrda = list.files("./data", pattern = ".rda$")
  inrda = gsub("\\.rda$", "", inrda)
  
  # 3 DOCUMENTED IN SOURCE AS ITS OWN /R/data_XYZ.R FILE
  inrfiles <- list.files("./R/", pattern = "^data_.*\\.R$")
  inrfiles <- gsub("^data_|\\.R$", "", inrfiles)
  inrfiles <- inrfiles[!grepl("aaaaa", inrfiles)]
  inrfiles <- inrfiles[!grepl("xxxxx", inrfiles)]
  
  # 4  DATASETS IN INSTALLED PACKAGE
  inpkg = datapack("EJAM")
  inpkg = inpkg$Item
  ## also includes things like .onAttach if you do it this way:
  # inpkg2 = functions_in_pkg(pkg = 'EJAM', internal_included = T, data_included = T, exportedfuncs_included = F)
  # inpkg2 = inpkg2$object
  ############# #  ############# #  ############# #  ############# #  ############# #
  
  ############# #
  # 2,1
  # setdiff(inrda, inyaml)
  cat("\n---------- IN SOURCE PACKAGE /data/xyz.rda FILENAMES BUT NOT YET IN _pkgdown.yml \n\n")
  print(cbind(setdiff(inrda, inyaml)))
  
  # 1,2
  # setdiff(inyaml, inrda) 
  #  not useful since   ############# # functions in yaml of course dont have .rda files, and also shows bgej etc.
  # cat("\n---------- NO SOURCE PACKAGE /data/xyz.rda FILE BUT STILL IS IN _pkgdown.yml \n\n")
  # print(cbind(setdiff(inyaml, inrda)))

  ############# #
  # 3,1
  # setdiff(inrfiles, inyaml)
  # # rfiles not yaml   also shows bgej etc.  
   cat("\n\n---------- DOCUMENTED IN SOURCE AS ITS OWN /R/data_XYZ.R FILE but not in _pkgdown.yml \n\n")
   print(cbind(setdiff(inrfiles, inyaml)))
  
  # 1,3
  # setdiff(inyaml, inrfiles)
  # yaml not rfiles  not useful since all the functions are in yaml etc

  ############# #
  # 4,1
  # setdiff(inpkg, inyaml)
  cat("\n\n---------- OBJECTS THAT ARE DATASETS IN INSTALLED PACKAGE BUT NOT YET IN  _pkgdown.yml 
  e.g., in the 'internal' section of the _pkgdown.yml file, one could add the following lines:\n
- title: internal
  contents:\n")
  mssng = setdiff(inpkg, inyaml) # as.vector(unlist(sapply(inpkg, function(z) if (!any(grepl(z, inyaml))) (z)  )))
  cat(paste0(paste0("  - ", mssng), collapse = "\n"), '\n\n')
  
  # 1,4
  # setdiff(inyaml, inpkg)
  # yaml not pkg  not useful since all the functions are in yaml, and also shows bgej etc.
  
  ############# #
  # 2,3
  # setdiff(inrda, inrfiles) 
  # check for source pkg data undocumented in src
  cat("\n---------- IN SOURCE PACKAGE /data/xyz.rda FILENAMES BUT NOT DOCUMENTED IN SOURCE AS ITS OWN /R/data_XYZ.R FILE \n\n")
  print(cbind(setdiff(inrda, inrfiles)))
  
  # 3,2
  # setdiff(inrfiles, inrda)
  cat("\n---------- DOCUMENTED IN SOURCE AS ITS OWN /R/data_XYZ.R FILE BUT NOT IN SOURCE PACKAGE /data/xyz.rda FILENAMES \n\n")
  print(cbind(setdiff(inrfiles, inrda)))
  
  ############# #
  # 2,4
  # setdiff(inrda, inpkg)
  cat("\n IN SOURCE PACKAGE /data/xyz.rda FILENAMES but not DATASETS IN INSTALLED PACKAGE\n\n")
  print(cbind(setdiff(inrda, inpkg)))
  
  # 4,2
  #  setdiff(inpkg, inrda)
  cat("\n DATASETS IN INSTALLED PACKAGE but not IN SOURCE PACKAGE /data/xyz.rda FILENAMES\n\n")
  print(cbind(setdiff(inpkg, inrda)))
  
  ############# #
  # 3,4
  # setdiff(inrfiles, inpkg)
  cat("\n DOCUMENTED IN SOURCE AS ITS OWN /R/data_XYZ.R FILE BUT NOT IN INSTALLED PACKAGE data()\n\n")
  print(cbind(setdiff(inrfiles, inpkg)))
  
  # 4,3
  # setdiff(inpkg, inrfiles)
  # check for installed pkg data undocumented in src
  cat("\n  IN INSTALLED PACKAGE data() BUT NOT DOCUMENTED IN SOURCE AS ITS OWN /R/data_XYZ.R FILE \n\n")
  print(cbind(setdiff(inpkg, inrfiles)))

  
  invisible(mssng)
}

