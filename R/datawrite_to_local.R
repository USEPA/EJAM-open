
#' Save datasets during package development
#' 
#' Utility to write large object(s) like EJAM datasets to local disk for convenience during app/pkg development, 
#' formatted as .arrow or .rda
#' 
#' @param varnames vector of object names
#' @param fnames optional vector of file names including extension ".rda" or ".arrow", but without path.
#'   Default is .arrow 
#' @param justchecking set this to FALSE to actually save instead of 
#'   just seeing in console info or the commands to be used, to test/check this
#' @param localfolder path to local folder without slash at end
#' @param overwrite Set to TRUE to overwrite file if it exists already, with new copy.
#' @return the paths of the objects as requested to be saved whether or not actually done
#' @seealso [dataload_from_local()] 
#' @examples 
#'   x = 1
#'   datawrite_to_local("x", localfolder = ".", justchecking = T) 
#'   
#' @export
#'
datawrite_to_local <- function(
    
    varnames = c(
      'blockwts', 'blockpoints', 'blockid2fips', "quaddata",
      'bgej', 'bgid2fips',
      'frs', 'frs_by_programid', 'frs_by_naics', "frs_by_sic", "frs_by_mact"),
    
    fnames = paste0(varnames, ".arrow"),
    
    localfolder = "~/../Downloads", 
    
    justchecking = F, overwrite = FALSE) {
  
  if (!all(is.character(varnames))) {
    ok = FALSE
    varnames = deparse(substitute(varnames))
    if (all(sapply(varnames, exists))) {
      if (interactive()) {
      ok <- askYesNo(paste0("looks like you provided unquoted object names ... do you mean '", varnames[1],"' etc.?"))
      if (is.na(ok)) {ok <- FALSE} # clicked cancel
      warning("varnames must be a character vector of quoted names of objects like c('x', 'y') \n")
      if (!ok) {return(NULL)}
      }}
    if (!ok) {
    stop("varnames must be a character vector of quoted names of objects like c('x', 'y') ")
    }}
  ####################################################### #
ext <- paste0(".", tools::file_ext(fnames))
  
  cat("\n\n")
  if (justchecking) {
    cat("Just checking, so nothing is being saved.\n\n")
  }
  
  if (interactive() && missing(localfolder)) {
    localfolder <- rstudioapi::selectDirectory(
      "Select Directory where you want to save local copies", 
      path = "~/../Downloads/EJAMbigfiles"
      # To update a particular local folder, e.g.:
      # locdir = ("./../EJAM-opensource/data")
      # locdir = ("~/../Downloads/EJAMbigfiles")
    )
    if (is.na(localfolder)) {stop('must specify a folder')}
  }
  
  if (!dir.exists(localfolder)) {
    cat(localfolder, ' does not exist.\n\n')
    if (!justchecking) {
      cat("Nothing could be saved.\n\n")
      warning( 'Nothing could be saved.')
      return()
    }
  }
  
  # Ask to confirm each of the defaults 
  if (interactive() && missing(varnames)) {
    confirmed <- rep(TRUE, length(varnames))
    for (i in seq_along(varnames)) {
      confirmed[i] <- askYesNo(paste0("Save ", varnames[i], " (as ", fnames[i],")?"))
    }
    varnames <- varnames[!is.na(confirmed) & confirmed]
  }
  
  localpaths  <- paste0(localfolder, '/', fnames)
  # if (justchecking) {cat("The folder " , localfolder, "exists? ", file.exists(localfolder), '\n\n')}
  if (!justchecking && interactive() && missing(overwrite) && any(file.exists(localpaths))) {
    overwrite <- askYesNo("overwrite files that already exist locally?")
    if (is.na(overwrite)) {stop('stopping')}
  }
  
  for (i in seq_along(varnames)) {
    
    this <- varnames[i] 
    
    if (justchecking) {
      cat(this, "is in memory? ", exists(x = this ), '... ') # looks in default environment!!
      if (dir.exists(localfolder)) {
        cat('The file is already saved in folder? ',  file.exists(localpaths[i]))
      }
      cat('\n')
    } else {
      if (file.exists(localpaths[i]) & !overwrite) {
        cat(localpaths[i], "already exists.\n  Set overwrite=TRUE if you want to replace it.\n")}
      if (!file.exists(localpaths[i]) | overwrite) {
        
        if (ext[i] == ".rda") {
          
          cat("saving", localpaths[i] , "\n")
          save(list = this, file = localpaths[i])
          
        } else {
          
          #     .arrow files   ####
          cat("saving", localpaths[i], "\n");
          
          text_to_do <- paste0("x <- arrow::write_ipc_file(", 
                               varnames[i],", ",  
                               "sink = '", localpaths[i], "')"  
          )
          cat(" ", text_to_do, '\n')
          x <- eval(parse(text = text_to_do)) # executes the command
          rm(x)
          # arrow::write_ipc_file(bgej,         file.path(locdir, "bgej.arrow"))
          # arrow::write_ipc_file( UNQUOTED OBJECT NAME!***, sink = localpaths[i])
        }
        if (file.exists(localpaths[i])) {cat("  saved ",  "\n\n")} else {cat("  Failed to save ",   '\n\n')}
      }
      
    }
  }   # end loop
  cat('\n\n')
  if (interactive() & !justchecking) {
    browseURL(localfolder)
  }
  invisible(localpaths)
}
##############################################################
