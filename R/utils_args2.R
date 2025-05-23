

##  utility that tries to print a view of a function's
##  parameters and their default values
##  with one per row of text

# examples: 


#  EJAM:::args2(ejamit)

#  EJAM:::args2(latlon_from_anything)

#  EJAM:::args2(ejam2report)


#  EJAM:::args2(ejam2excel)
# 
#
## ejam2excel(
##   ejamitout,
##   fname = NULL,
##   save_now = TRUE,
##   overwrite = TRUE,
##   launchexcel = FALSE,
##   interactive_console = TRUE,
##   ok2plot = TRUE,
##   in.testing = FALSE,
##   in.analysis_title = EJAM analysis,
##   react.v1_summary_plot = NULL,
##   radius_or_buffer_in_miles = NULL,
##   buffer_desc = Selected Locations,
##   radius_or_buffer_description = Miles radius of circular buffer (or distance used if buffering around polygons),
##   hyperlink_colnames = ECHO Report,
##   site_method = "",
##   mapadd = FALSE,
##   ...
## )


args2 <- function(funcname) {
  
  if (length(funcname) > 1) {stop("must provide a single function name")}
  if (!(is.character(funcname) || is.function(funcname))) {stop("must provide function or character string that is name of function")}
  # primitives do not show params the way other functions would
  if (is.character(funcname)) {
    func_obj <- get(funcname)  
  } else {
    func_obj <- funcname
  }
  if (is.primitive(func_obj)) {
    cat("This is a primitive function:\n")
    print(args(func_obj))
    invisible()
  }
  
  # convert to string that has function name
  if (is.function(funcname)) {funcname <- as.character(substitute(funcname))}
  
  # get the arguments and their default values as a list
  y <- formals(funcname)
  
  if (length(y) == 0) {
    cat("No parameters found for function ", funcname, "\n")
  } else {
    param_names <- names(y)
    
    # try to identify the ones with no default specified
    hasdefault <- sapply(y, function(z) {is.character(z) | is.numeric(z) | is.logical(z) | is.null(z) | any(nchar(z) > 0)})
    nodefault <- !hasdefault
    
    ischar <- sapply(y, is.character)
    param_defaults_txt <- as.character(y)
    
    # try to fix at least the ones that are "" or '' but it may miss some defaults this way so print(str()) later as backup
    # param_defaults_txt[hasdefault & param_defaults_txt == ""] <- '""'
    param_defaults_txt[hasdefault & ischar ] <- paste0('"', param_defaults_txt[hasdefault & ischar ], '"')
# & !param_defaults_txt == ""
    x = paste0(
      paste0(param_names, ifelse(nodefault, "", " = "), param_defaults_txt), 
      collapse = ",\n  "
    )
    x <- paste0(
      funcname, "(\n  ", 
      x,
      "\n)\n"
    )
    cat("\n\nAn attempt to print a view of function defaults with one per row of text: \n\n")
    cat(x)
    cat("\n")
  }
  if (!is.primitive(func_obj)) {
    cat("\nWhat str() shows (minus the attributes)  \n\n")
    attributes(func_obj) <- NULL
    print(str(func_obj))
  }
  
  invisible(formals(funcname))
}

