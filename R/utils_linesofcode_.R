
# also SEE https://pharmar.github.io/riskmetric/  too for assess_size_codebase()
########################################################## #

#' utility - quick way to do dir() but for query pattern like  *.xlsx
#' useful if you forget [glob2rx()] is available
#' @param query_glob optional, something like "*.r*" or "*.xlsx" or "myfile.*" or "a*.*",
#'   so it is like `dir(pattern = glob2rx(query_glob))`
#' @param ignore.case passed to [dir()]
#' @param recursive passed to [dir()], often useful to set TRUE
#' @param silent whether to avoid printing to console
#' @param ... passed to [dir()]
#' @return vector of paths
#' @examples
#'  #  dir2()
#' dontrun{
#' dir2()
#' dir2("*.zip", recursive = T)
#' dir2("*.y*",  recursive = T)
#'
#' # if recursive=T, left-aligned view of paths is shown
#' dir2("*datacreate*.*", recursive = T)
#' # for right-aligned view:
#' data.frame(hit=dir2("*address*.*", recursive = T))
#'
#' dir2("*.csv*", path = testdatafolder(installed = FALSE), recursive = T)
#' }
#'
#' @keywords internal
#'
dir2 <- function(query_glob = '*.*', ignore.case = TRUE, recursive = FALSE, silent = FALSE, ...) {

  vector_of_paths <- dir(
    pattern = glob2rx(query_glob),
    ignore.case = ignore.case,
    recursive = recursive,
    ...
  )

  table_view <- as.data.frame(cbind(
    `Matching files found` = vector_of_paths
  ))
  cat("\n")
  if (!silent) {
    if (recursive) {
      print(as.matrix(table_view))
    } else {
      print(table_view)
    }
  }
  invisible(vector_of_paths)
}
########################################################## #

#' UTILITY - count lines of source code per .R file (not per function)
#' check how big each source code file is to help decide how to break it up into pieces
#'
#' @param folder path to folder with .R files
#' @param packages optional vector of names of packages of source code
#' @param recursive logical, look in subfolders
#' @param sums logical, if TRUE, returns sums info, otherwise just prints that to console and returns more info
#' @param rfolderonly logical
#' @param cropfilename number of character to truncate filename to for display in console
#' @param croppath limit path for display
#' @param showrows optional
#'
#' @return data.frame of info about files
#'
#' @keywords internal
#'
#' @noRd
#'
linesofcode2 <- function(folder='.', packages, recursive=TRUE, sums=FALSE, rfolderonly=FALSE, cropfilename=40, croppath=20, showrows=NULL) {

  # if packages is specified:   if (!missing(packages))
  #   if both specified: if (!missing(folder)) {warn("packages and folder both specified, so ignoring folder param")}
  #   (if only packages names specified, quietly ignore default folder)
  #   find each of packages wherever, using find.package()
  # if packages not specified:  if (missing(packages))
  #   (if neither folder nor packages param specified, use default folder.)
  #   (if only folder specified, use specified folder.)
  #   identify any pkgs in folder.

  if (missing(packages)) {
    # identify any pkgs in folder
    # only if the folder contains all the source package dirs, this works:

    dir_is_pkg = function(mydir) {
      devtools::is.package(devtools::as.package(mydir))
    }
    if (length(folder) > 1) {
      stop('folder must be a single folder')
    }
    if (file.exists(file.path(folder, "DESCRIPTION")) && dir_is_pkg(folder)) {
      message("folder ", folder, " seems to be a package, but you might have meant to use the parent folder containing one or more source or installed packages")
      packages <- normalizePath(dirname(folder))
      # handle it as if they had provided packages param not folder param
      return(
        linesofcode2(packages = packages, recursive = recursive, sums = sums, rfolderonly = rfolderonly, cropfilename = cropfilename, showrows = showrows)
      )
    } else {
      pkg_dir = dir(folder, include.dirs = T, full.names = T)
      pkg_dir = pkg_dir[dir.exists(pkg_dir)]
      # full path right now
      pkgname.allfound = basename(pkg_dir)
    }
  }

  if (!missing(packages)) {
    packages = basename(packages)
    if (!missing(folder)) {warn("packages and folder both specified, so ignoring folder param")}
    # find each of packages wherever, using find.package()
    # find each package - finds installed version unless load_all() has been done
    pkg_dir = vector()
    parent_dir = vector()
    for (i in seq_along(packages)) {
      x = try(find.package(package = packages[i]), silent = TRUE)
      if (inherits(x, "try-error")) {
        warning("Package ", packages[i], " not found")
        x <- NA
      }
      pkg_dir[i] = x
    }
    parent_dir = dirname(pkg_dir[!is.na(pkg_dir)])
    pkgname.allfound <- packages[!is.na(pkg_dir)]
    pkg_dir = pkg_dir[!is.na(pkg_dir)]
    # > cbind(parent_dir, pkgname.allfound, pkg_dir)
    # parent_dir                      pkgname.allfound   pkg_dir
    # [1,] "C:/Users/x/R/mysource"    "EJAM"             "C:/Users/x/R/mysource/EJAM"
    # [2,] "C:/Users/x/R/myinstalled" "data.table"        "C:/Users/x/R/myinstalled/data.table"
  }

  Rfilenames <- NULL
  pnames <- NULL
  for (i in seq_along(pkg_dir)) {
    rfiles_1path = dir2('*.R', path = pkg_dir[i], recursive = recursive, silent = TRUE)
    pnames <- c(pnames, rep(pkgname.allfound[i], length(rfiles_1path)))
    Rfilenames <- c(Rfilenames,  rfiles_1path)
  }
  justfilename  <- basename(Rfilenames)

  # *** get vector of package names as long as Rfilenames

  pkgname <- pnames # pkgname.allfound[pkgname.allfound %in% packages ]

  n <- length(Rfilenames)

  if (n == 0) {
    cat('No .R files found  \n')
  }
  if (n > 0) {
    out <- matrix(nrow = n, ncol = 6)
    out <- as.data.frame(out)
    names(out) <- c('lines', 'comments', 'code', 'package', 'where', 'filename')

    for (i in 1:n) {

      filetext <- suppressWarnings(  try(
        readLines(file.path(folder, Rfilenames[i])),
        silent = TRUE ))
      if (inherits(filetext, "try-error")) {
        linecount <- NA
        commentcount <- NA
      } else {
        linecount <- length(filetext)
        commentcount <-  sum( grepl(pattern = '^#', x =  filetext, ignore.case = TRUE) )
      }
      codecount <- linecount - commentcount

      out[i, 'lines'] <- linecount
      out[i, 'comments'] <- commentcount
      out[i, 'code'] <- codecount
      out[i, 'package'] <- pkgname[i]
      out[i, 'where'] <- gsub(justfilename[i], "", gsub(pkgname[i], "", Rfilenames[i]))
      out[i, 'filename'] <- justfilename[i]
      #out[i, ''] <- x
      #cat('Lines: ', linecount, ' in', Rfilenames[i], '(', codecount, 'code +', commentcount, 'comments)', '\n')
    }
    out <- out[order(out$lines, decreasing = T), ]
    rownames(out) <- NULL

    if (rfolderonly) {out <- out[out$where == "/R/", ]}

    mysums <- cbind(
      #   summarize(out$lines,    by = out$package, FUN = sum), # was from the Hmisc pkg
      #   summarize(out$filename, by = out$package, FUN = length) # was from the Hmisc pkg  #  c(out$package , out$lines  ,   out$package,  out$filename) from Hmisc # Group.1     x         Group.1   x  from aggregate
      aggregate(out$lines,    by = list(out$package), FUN = sum),
      aggregate(out$filename, by = list(out$package), FUN = length)
    )
    names(mysums)    <- c(    "package",     "filename",  "p2",   "lines")
    mysums <- mysums[ , c("package", "filename", "lines")]
    mysums <- mysums[order(mysums$lines, decreasing = T), ]
    rownames(mysums) <- NULL

    if (sums) {
      return(mysums)
    } else {
      cat('\n'); print( mysums ); cat('\n')

      cropped <- out
      cropit <- function(x, n) {x[nchar(x) > n + 3] <- paste0(substr(x[nchar(x) > n + 3], 1, n), "..." ); return(x)}
      cropped$filename  <- cropit(cropped$filename, cropfilename)
      cropped$where    <- cropit(cropped$where,     croppath)

      if (is.null(showrows)) {
        showrows <- 1 + findInterval(sum(cropped$lines) / 2, cumsum(cropped$lines))
        cat("\nMost of the code is in these files: \n\n")}
      print(cropped[1:(min(NROW(cropped), showrows )),])
      cat("\n Full list is returned invisibly \n")
      invisible(out)
    }
  }
}
########################################################## #

# linesofcode_per_section <- function(folder, ...) {
#
#
#   x <- linesofcode2(folder, ...)
#
#   # to be continued
#
#
# }
#
########################################################## #
