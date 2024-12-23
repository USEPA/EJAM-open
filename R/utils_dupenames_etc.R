
# Functions here 

# nacounts
# setdiff_yx
# setdiff2
# unshared
#
# dupeRfiles
# dupenames
# all_equal_functions
# functions_that_use()
# functions_in_pkg
# dependencies_of_ejam

##################################################################### #
nacounts = function(x, showall=FALSE) {
  
  if (NCOL(x) == 1) {
    return(
      data.frame(nas = sum(is.na(x)),
                 other = sum(!is.na(x)))
    ) # ignores showall
  } else {
    if (showall) {
      shown <- rep(TRUE, NCOL(x))
    } else {
      shown <- sapply(x, anyNA)
    }
    z = data.frame(
      nas = sapply(x, function(y) sum(is.na(y))), 
      other = sapply(x, function(y) sum(!is.na(y)))
    )
    z = z[shown,]
    return(z)
  }
}
### e.g.,  nacounts(data.frame(a = 1:3, b= c(NA, NA, 0), d = c(NA,1,1)))
##################################################################### #


#' UTILITY - see what is in y not x
#' 
#' utility just like setdiff except for y,x instead of x,y
#' @examples
#'   setdiff(   1:4, 3:8)
#'   setdiff_yx(1:4, 3:8) # makes it easy to check both without 
#' @keywords internal
#' 
setdiff_yx = function(x,y) setdiff(y,x)
##################################################################### #
# setdiff() extensions ####

#' UTILITY - see what is only in x or y but not both - just like setdiff except for y,x and also x,y
#' 
#' setdiff2 aka unshared just shows which elements are in one and only one of the sets x and y
#' @examples setdiff2(1:4, 3:8)
#' 
#' @keywords internal
#' 
setdiff2 <- function(x,y) {setdiff(union(x,y),intersect(x,y))}
##################################################################### #


#' UTILITY - see what is only in x or y but not both - just like setdiff except for y,x and also x,y
#'  
#' setdiff2 aka unshared just shows which elements are in one and only one of the sets x and y
#' @examples unshared(1:4, 3:8)
#' 
#' @keywords internal
#' 
unshared <- function(x,y) {setdiff(union(x,y), intersect(x,y))}
##################################################################### #
# conflicting sourcefile names ####

#' UTILITY - check conflicting sourcefile names
#' 
#' @description See what same-named .R files are in 2 sourcecode folders
#' @details 
#'   See [dupeRfiles()] for files supporting a shiny app that is not a package, e.g.
#'   
#'   See [dupenames()] for objects that are in R packages.
#'   
#'   See [functions_in_pkg()]
#'   
#'   See [datapack()]
#'   
#' @param folder1 path to other folder with R source files
#' @param folder2 path to a folder with R source files, defaults to "./R"
#' @keywords internal
#' 
dupeRfiles <- function(folder1 = '../EJAM/R', folder2 = './R') {
  if (!dir.exists(folder1)) {
    #try to interpret as name of source package without path
    # assuming wd is one pkg and other is in parallel place
    folder1 <- paste0("../", folder1, "/R")
    if (!dir.exists(folder1)) {stop('folder1 does not exist nor does ', folder1)}
  }
  cat("Comparing .R files in ", folder1, ", to the files in ", folder2, "\n\n")
  docs1 <- list.files(folder1)
  docs2 <- list.files(folder2)
  both <- intersect(docs1, docs2)
  x <- list()
  for (fname in both) {
    x[[fname]] <-  ifelse(identical(
      readLines(file.path(folder1, fname)), 
      readLines(file.path(folder2, fname))
    ) , "identical", "differ") 
  }
  cat('\n\n')
  out <- data.frame(filename = both, identical = unlist(as.vector(x)))
  out <- out[order(out$identical), ]
  rownames(out) <- NULL
  return(out)
}
##################################################################### #
# conflicting exported functions or data ####

#' UTILITY - check conflicting exported function or data names
#' 
#' @description See what same-named objects (functions or data) are exported by some (installed) packages
#' @details utility to find same-named exported objects (functions or datasets) within source code 
#'   of 2+ packages, and see what is on search path, for dev renaming / moving functions/ packages
#'   
#'   See [dupeRfiles()] for files supporting a shiny app that is not a package, e.g.
#'   
#'   See [dupenames()] for objects that are in R packages.
#'   
#'   See [functions_in_pkg()]
#'   
#'   See [datapack()]
#'   
#' @param pkg one or more package names as vector of strings. 
#'   If "all" it checks all installed pkgs, but takes very very long potentially.
#' @param sortbypkg If TRUE, just returns same thing but sorted by package name
#' @param compare.functions If TRUE, sends to console inf about whether body and formals
#'   of the functions are identical between functions of same name from different packages.
#'   Only checks the first 2 copies, not any additional ones (where 3+ pkgs use same name)
#' @return data.frame with columns Package, Object name (or NA if no dupes)
#' 
#' @keywords internal
#' 
dupenames <- function(pkg = EJAM::ejampackages, sortbypkg=FALSE, compare.functions=TRUE) {
  
  # Get list of exported names in package1, then look in package1 to
  #   obs <- getNamespaceExports(pkg)
  # find those appearing in source code .R files without package1:: specified,
  # since code using those functions and code defining those have to both be in the same pkg,
  #  (or need to add xyz:: specified)
  # and maybe want to do global search replace within files, like this:
  #   xfun::gsub_file()
  
  if ("all" %in% pkg) {
    pkg <- as.vector(installed.packages()[,"Package"])
  } else {
    
    # THIS COULD/SHOULD BE REPLACED USING ::: and/or getFromNamespace(), etc.
    
    findPkgAll <- function(pkg) { # finds path to each installed package of those specified
      unlist(lapply(.libPaths(), function(lib)
        find.package(pkg, lib, quiet = TRUE, verbose = FALSE)))
    }
    installed.packages.among <- function(pkg) {
      fff <- findPkgAll(pkg) # ok if a pkg is not installed. finds path to installed not source location
      if (length(fff) == 0) {warning("none of those packages are installed")
        return(NA)
      }
      gsub(".*/", "", fff) # get just names of pkgs not full paths
    }
    pkg <- installed.packages.among(pkg)
  }
  # getNamespaceExports will get exported object names, but fails if any pkg is not installed, hence code above
  
  xnames <-  sapply(pkg, function(x) {
    
    # DO WE WANT TO CHECK EVEN NON-EXPORTED OBJECTS? see getFromNamespace() and :::
    
    y <- try(getNamespaceExports(x))
    if (class(y) == "try-error") {return(paste0("nothing_exported_by_", x))} else {return(y)}
  } ) # extremely slow if "all" packages checked
  
  counts <- sapply(xnames, length)
  xnames <- unlist(xnames)
  xnames_pkgs <- rep(names(counts), counts)
  names(xnames) <- xnames_pkgs
  ddd <-  data.frame(variable = xnames, package = names(xnames))
  duplicatednameslistedonceeach <- names(table(  xnames ))[(table(  xnames ) > 1)]
  if (length(duplicatednameslistedonceeach) > 0) {
    ddd <- ddd[ddd$variable %in% duplicatednameslistedonceeach, ]
    ddd <- ddd[order(ddd$variable), ]
    rownames(ddd) <- NULL
  } else {
    ddd <- NA
    return(ddd)
  }
  if (sortbypkg) ddd <- ddd[order(ddd$package), ]
  
  if (compare.functions) {
    ddd$problem = "ok"
    #  use all_equal_functions() here to compare all pairs (but ignores more 2d copy of a function, so misses check of trios)
    #  to see if identical names are actually identical functions 
    # ddd <- dupenames()
    for (var in unique(ddd$variable)) {
      ok <- all_equal_functions(
        fun = var,
        package1 = ddd$package[ddd$variable == var][1],
        package2 = ddd$package[ddd$variable == var][2]
      )
      cat(var, " identical? ", ok, " \n")
      if (any(!ok)) {ddd$problem[ddd$variable == var] <- "Copies of this function differ"}
    }
    cat(" \n\n") 
  } else {
    ddd$problem = "not checked"
  }
  
  return(ddd)
}
##################################################################### #
## (used by dupenames ####

#' UTILITY - check different versions of function with same name in 2 packages
#' obsolete since EJAMejscreenapi phased out? was used by dupenames() to check different versions of function with same name in 2 packages
#' @param fun quoted name of function, like "latlon_infer"
#' @param package1 quoted name of package, like "EJAM"
#' @param package2 quoted name of other package, like "EJAMejscreenapi"
#'
#' @return TRUE or FALSE
#' @seealso [dupenames()] [all.equal.function()]
#' 
#' @keywords internal
#' 
all_equal_functions <- function(fun="latlon_infer", package1="EJAM", package2="EJAMejscreenapi") {
  
  # not the same as base R all.equal.function() see  ?all.equal.function
  
  # strange quirks did not bother to debug:
  
  # 1) Normally it checks the first two cases of dupe named functions from 2 packages,
  # and answers with FALSE or TRUE (1 value).
  # But it returns FALSE 3 times only in the case of run_app (but not latlon_is.valid) 
  # dupenames(ejampackages) # or just dupenames() 
  
  # 2) ### error when checking a package that is loaded but not attached. 
  # eg doing this:
  # all_equal_functions("get.distance.all", "proxistat", "EJAM") # something odd about proxistat pkg
  #   and note there is now a function called proxistat()
  ### or 
  # dupenames(c("proxistat", "EJAMejscreenapi"), compare.functions = T)
  # Error in all_equal_functions(fun = var, package1 = ddd$package[ddd$variable ==  : 
  #                                                                   distances.all not found in proxistat
  #                                                                Called from: all_equal_functions(fun = var, package1 = ddd$package[ddd$variable == 
  
  if (!(is.character(fun) & is.character(package1) & is.character(package2))) {
    warning("all params must be quoted ")
    return(NA)
  }
  # we could attach
  f1 = try(
    silent = TRUE, 
    expr = getFromNamespace(fun, ns = package1) 
    # get((fun), envir = as.environment(paste0("package:", (package1)) ) ) # this would not work if the package were not already loaded, on search path. see ?getFromNamespace
  )
  if (class(f1) == "try-error"  ) {
    # warning("fails when checking a package that is loaded but not attached - library func allows it to work. ")
    warning(fun, " not found in ", package1 )
    return(NA)
  }
  if (!(is.function(f1))) {warning(package1, "::", fun, " is not a function");return(NA)}
  
  f2 = try(
    silent = TRUE, 
    expr = getFromNamespace(fun, ns = package2) 
    # get((fun), envir = as.environment(paste0("package:", (package2)) ) )
  )
  
  if (class(f2) == "try-error") {
    # warning("fails when checking a package that is loaded but not attached - library func allows it to work. ")
    warning(fun, " not found in ",  package2)
    return(NA)
  }
  if (!(is.function(f2))) {warning(package2, "::", fun, " is not a function");return(NA)}
  
  x <- (TRUE == all.equal(body(f1), body(f2))) & (TRUE == all.equal(formals(f1), formals(f2)))
  return(x)
}
##################################################################################### #
# find text in functions or source files ####

#' utility for developing package - searches for text in each function exported by pkg (or each .R source file in pkg/R)
#' 
#' @details Searches the body and parameter defaults of exported functions.  
#' @param text something like "EJAM::" or "stop\\(" or "library\\(" or "***"
#' @param pkg name of package or path to source package root folder - this 
#'   
#'   checks only the exported functions of an installed package, 
#'   if pkg = some installed package as character string like "EJAM"
#'   
#'   checks each .R source FILE NOT each actual function, 
#'   if pkg = root folder of source package with subfolder called R with .R source files
#'   
#' @param ignore_comments logical, 
#'   ignore_comments is ignored and treated as if it were TRUE when pkg = some installed package
#'   
#'   ignore_comments is used only if pkg = a folder that contains .R files
#'   
#'    Note it will fail to ignore comments in .R files that are at the end of the line of actual code like  print(1) # that prints 1
#'
#' @return vector of names of functions or paths to .R files
#' 
#' @keywords internal
#' 
functions_that_use <- function(text = "stop\\(", pkg = "EJAM", ignore_comments = TRUE) {
  
  
  if (grepl("\\(", text) & !grepl("\\\\\\(", text)) {warning('to look for uses of stop(), for example, use two slashes before the open parens, etc. as when using grepl()')}
  
  stops <- NULL
  if (inherits(try(find.package(pkg), silent = TRUE), "try-error")) {
    # not an installed pkg. 
    if (dir.exists(file.path(pkg, "R"))) {
      # it should be a folder that is root of source package with subfolder called R with .R files so search in those
      
      for (this in list.files(file.path(pkg, 'R'), pattern = '.R', full.names = TRUE)) {
        text_lines_of_function_body <- readLines(this)
        # each row is an element of the vector here
        if (ignore_comments) {
          dropcommentedlines <- function(mytext) {gsub("^[ ]*#.*$", "", mytext)} # presumes each line is an element of mytext vector
          text_lines_of_function_body <- dropcommentedlines(text_lines_of_function_body)
        }
        text_of_function_body <- paste0(text_lines_of_function_body, collapse = '\n')
        if (grepl(text, text_of_function_body)) {
          stops <- c(stops, this)}
      }
      
    } else {
      if (shiny::isRunning()) {
        warning('pkg must be the name of an installed package or a path to root of source package with R subfolder that has .R files')
        return(NULL)
      } else {
        stop('pkg must be the name of an installed package or a path to root of source package with R subfolder that has .R files')
      }
    }
  } else {
    # it is an installed package
    if (ignore_comments == FALSE) {warning('always ignores commented lines when checking exported functions of an installed package')}
    for (this in getNamespaceExports(pkg)) {
      
      text_lines_of_function_body <- as.character(functionBody(get(this)))
      # or is that the same as just  as.character(body(this))  ??
      # each row is an element of the vector now
      
      # also check the function parameter default values
      text_lines_of_function_body <- c(text_lines_of_function_body, paste0(formals(this), collapse = " "))
      
      if (ignore_comments) {
        dropcommentedlines <- function(mytext) {gsub("^[ ]*#.*$", "", mytext)} # presumes each line is an element of mytext vector
        # however that will fail to ignore comments that are at the end of the line of actual code like  print(1) # that prints 1
        text_lines_of_function_body <- dropcommentedlines(text_lines_of_function_body)
      }
      text_of_function_body <- paste0(text_lines_of_function_body, collapse = '\n')
      if (grepl(text, text_of_function_body)) {
        stops <- c(stops, this)}
    }
  }
  return(sort(stops))
}
##################################################################### #
# see pkg data + functions, exported & internal ####

#' utility to see which objects in a loaded/attached package are exported functions, internal (unexported) objects, or datasets
#' @details
#'   See [dupeRfiles()] for files supporting a shiny app that is not a package, e.g.
#'   
#'   See [dupenames()] for objects that are in R packages.
#'   
#'   See [functions_in_pkg()]
#'   
#'   See [datapack()]
#'   
#' @param pkg name of package as character like "EJAM"
#' @param alphasort_table default is FALSE, to show internal first as a group, then exported funcs, then datasets
#' @param internal_included default TRUE includes internal (unexported) objects in the list
#' @param exportedfuncs_included default TRUE includes exported functions (non-datasets, actually) in the list
#' @param data_included default TRUE includes datasets in the list, as would be seen via data(package=pkg)
#' @param vectoronly set to TRUE to just get a character vector of object names instead of the data.frame table output
#' @seealso [ls()] [getNamespace()] [getNamespaceExports()] [loadedNamespaces()]
#' 
#' @return data.table with colnames object, exported, data  where exported and data are 1 or 0 for T/F,
#'   unless vectoronly = TRUE in which case it returns a character vector
#' @examples  # functions_in_pkg("datasets")
#' 
#' @keywords internal
#'
functions_in_pkg <- function(pkg, alphasort_table=FALSE, internal_included=TRUE, exportedfuncs_included=TRUE, data_included=TRUE, vectoronly=FALSE) {
  
  # helper functions inside this function ####
  
  dataonly <- function(pkg) {EJAM:::datapack(pkg = pkg, simple = TRUE)$Item}
  
  exported_plus_internal_withdata <- function(pkg) {sort(union(dataonly(pkg), ls(getNamespace(pkg), all.names = TRUE)))} # all.names filters those starting with "."
  exported_only_withdata          <- function(pkg) {ls(paste0("package:", pkg))} 
  # same as ls(envir = as.environment(x = paste0("package:", pkg)))
  # same as  getNamespaceExports() except sorted 
  
  exported_plus_internal_nodata <- function(pkg) {sort(setdiff(
    exported_plus_internal_withdata(pkg = pkg), 
    dataonly(pkg = pkg)))}
  exported_only_nodata <- function(pkg) {sort(setdiff(
    exported_only_withdata(pkg = pkg), 
    dataonly(pkg = pkg)))}
  
  internal_only_withdata <- function(pkg) {sort(setdiff(
    exported_plus_internal_withdata(pkg = pkg), 
    exported_only_nodata(pkg = pkg)))}
  internal_only_nodata <- function(pkg) {sort(setdiff(
    internal_only_withdata(pkg = pkg), 
    dataonly(pkg = pkg)))}
  
  # # double-checks, obsolete now since phased out use of EJAMejscreenapi pkg
  # 
  # setequal(      exported_plus_internal_withdata("EJAMejscreenapi"), 
  #          union(exported_plus_internal_nodata(  "EJAMejscreenapi"), dataonly("EJAMejscreenapi")))
  # 
  # setequal(      exported_only_withdata(         "EJAMejscreenapi"), 
  #          union(exported_only_nodata(           "EJAMejscreenapi"), dataonly("EJAMejscreenapi")))
  # 
  # setequal(      internal_only_withdata(         "EJAMejscreenapi"), 
  #          union(internal_only_nodata(           "EJAMejscreenapi"), dataonly("EJAMejscreenapi")))
  
  # table format output 
  
  omni <- exported_plus_internal_withdata(pkg)
  y <- data.frame(
    object = omni,
    exported = ifelse(omni %in% exported_only_withdata(pkg), 1, 0),
    data = ifelse(omni %in% dataonly(pkg), 1, 0)
  )
  if (!data_included) {
    y <- y[y$data == 0, ]
  }
  if (!internal_included) {
    y <- y[!(y$exported == 0), ]
  }
  if (!exportedfuncs_included) {
    y <- y[!(y$exported == 1 & y$data == 0), ]
  }
  
  if (!vectoronly) {
    if (alphasort_table) {
      # already done by default
    } else {
      y <- y[order(y$exported, y$data, y$object), ]
    }
    return(y)
  }
  
  # vector format output  
  
  if (vectoronly) {
    # cat('\n\n')
    # cat(pkg)
    # cat('\n\n')
    # print(y)
    # cat('\n\n')
    
    return(y$object) 
    
    ################# #
    # if (internal_included & data_included) {
    #   x <- exported_plus_internal_withdata(pkg)
    # }
    # if (internal_included & !data_included) {
    #   x <- exported_plus_internal_nodata(pkg)
    # }
    # if (!internal_included & data_included) {
    #   x <- exported_only_withdata(pkg)
    # }
    # if (!internal_included & !data_included) {
    #   x <- exported_only_nodata(pkg)
    # }
    #   return(x)
    ################# #
    
  }
}
################# #    ################# #    ################# #    ################# #    ################# #


# recursive dependencies of a package ####

#' utility for developing package, see what pkgs it depends on, recursively (i.e., downstream ones too)
#' Reminder of ways to check this is printed to console.
#' 
#' @param localpkg "EJAM" or another installed package
#' 
#' @param depth would be used if using the deepdep package and function
#' @param ignores_grep would be used if using the deepdep package and function
#' @return NULL
#' 
#' @keywords internal
#' 
dependencies_of_ejam <- function(localpkg = "EJAM", depth = 6, ignores_grep = "0912873410239478") {
  
  #################### #  
  
  cat(paste0("
  
  # This may be useful to see dependencies of a package like EJAM:
  
x = sort(packrat", ":::", "recursivePackageDependencies('", 
             localpkg,
"', lib.loc = .libPaths(), ignores = NULL))

x
      "))
  
  #################### #
   
#   cat(paste0("
#   
#   # or if you have the deepdep package installed (it is not required by EJAM)... 
#   
# y = sort(
#   unique( 
#     grep(
#       '", ignores_grep, "', 
#       deepdep", "::", "deepdep(
#         '", localpkg, "', 
#         local = TRUE, 
#         downloads = FALSE, 
#         depth = ", depth, "
#       )$name, 
#       value = TRUE, 
#       invert = TRUE)
#   )
# )  
#   
#       "))
#   
#   #################### #
#   
#   cat("
# 
# setdiff(x,y)
# setdiff(y,x)
#       ")
  #################### #
  
  ## report all dependencies and downstream ones etc.
  ## requires that packrat and deepdep packages be attached 1st:
  # x <- grep("asdfasdfasfasdfasdf", deepdep('EJAM', local = TRUE, downloads = FALSE, depth = 6)$name, value = TRUE, invert = TRUE)
  # y <- sort( packrat:::recursivePackageDependencies('EJAM', lib.loc = .libPaths(), ignores = NULL))
  # setdiff(y, x)
  # ## [1] "snow"  
  ## for some reason this 1 package is identified as a dependency one way but not the other way
  
  invisible()
}
##################################################################################### #

