# check how big each section of source code is to help decide how to break it up into pieces


# BUT SEE https://pharmar.github.io/riskmetric/  too for assess_size_codebase()

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





#' linesofcode2 - UTILITY - count lines of source code per .R file (not per function) - NOT TESTED
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
linesofcode2 <- function(folder='.', packages, recursive=TRUE, sums=FALSE, rfolderonly=FALSE, cropfilename=40, croppath=20, showrows=NULL) {

  dir2 <- function(x, ignore.case = TRUE, ...) {
    if (missing(x)) {x <- '*.*'}
    return(cbind(dir(pattern = glob2rx(x), ignore.case = ignore.case, ...)))
  }
  
Rfilenames <- dir2('*.R', path = folder, recursive = recursive)
  justfilename.allfound     <- gsub('[[:print:]]*/', '', Rfilenames)
  pkgname.allfound          <- gsub('/[[:print:]]*', '', Rfilenames)
  
  if (!missing(packages)) {
    # remove files not within packages trees
    if (!any(packages %in% pkgname.allfound)) {warning(   'None of specified packages were found under specified folder')
      return(NULL)}
    if (!all(packages %in% pkgname.allfound)) {warning('Not all specified packages were found under specified folder')}
    packages <- packages[packages %in% pkgname.allfound]
  } else {
    packages <- pkgname.allfound
  }
  Rfilenames   <- Rfilenames[pkgname.allfound %in% packages]
  justfilename <- justfilename.allfound[ pkgname.allfound %in% packages ]
  pkgname      <- pkgname.allfound[pkgname.allfound %in% packages ]
  n <- length(Rfilenames)
  if (n > 0) {
    out <- matrix(nrow = n, ncol = 6)
    out <- as.data.frame(out)
    names(out) <- c('lines', 'comments', 'code', 'package', 'where', 'filename')
    
    for (i in 1:n) {
      
      filetext <- suppressWarnings(  readLines(file.path(folder, Rfilenames[i])) )
      linecount <- length(filetext)
      commentcount <-  sum( grepl(pattern = '^#', x =  filetext, ignore.case = TRUE) )
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
  } else {
    cat('No .R files found in', folder, '\n')
  }
}
