#' See ejamit()$results_bysite in interactive table in RStudio viewer pane
#'
#' @param out output of ejamit(), or one table like ejamit()$results_overall,
#'   or subset like ejamit()$results_bysite[7,]
#' @param fname optional. path and filename of the html file to save the table to, 
#'   or it uses tempdir() if not specified. Set it to NULL to prevent saving a file.
#' @param maxrows only load/ try to show this many rows max.
#' @param launch_browser set TRUE to have it launch browser and show report.
#'   Ignored if not interactive() or if fname is set to NULL.
#' @return a datatable object using [DT::datatable()] 
#'   that can be printed to the console or shown in the RStudio viewer pane
#' @examples ejam2tableviewer(testoutput_ejamit_10pts_1miles)
#'   
#' @export
#'
ejam2tableviewer = function(out, fname = 'automatic', maxrows = 1000, launch_browser = TRUE) {
  
  if (!interactive()) {launch_browser <- FALSE} # but that means other functions cannot override this while not interactive.
  
  if ("results_bysite" %in% names(out)) {
    x <- out$results_bysite  
  } else {
    x <- out
  }
  if (!is.data.frame(x)) { # data.table is ok too
    stop("Input must be a data frame")
  }
  
  x <- x[1:min(nrow(x), maxrows), ]
  
  x <- table_signif_round_x100(x)
  # x <- table_x100(x, cnames = names_pct_as_fraction_ejamit)
  # x <- table_round(x)
  
  dt <-    DT::datatable(x,
                         # colnames = out$longnames, 
                         colnames = fixcolnames(names(x), 'r', 'long'),
                         rownames = FALSE,  
                         escape = FALSE,  # related to showing URLs correctly but note security implication
                         caption = paste0(nrow(x),  ' SITES \\"' , ' ', '\\"'), 
                         filter = "top")
  ################################# #
  
  # if fname was missing, then create a name and save it in temp dir.
  # if fname was set to some path by user, then save it there not in temp dir
  # if fname was set to NULL by user, then do not save, and cannot see in browser
  
  # For now at least, do not try to save file if in shiny app!
  if (!shiny::isRunning()) {
  
    # save/try save if (fname missing) or (fname  provided and not null) 
    if (missing(fname)) {trysave <- TRUE} else  {if (!is.null(fname)) {trysave <- TRUE}}
    
    if (trysave)  {
    # (NULL would mean do not save and do not browse)
    
    # Validate folder and or file 
      
    validfoldernotfile = function(x) {x = file.info(x)$isdir; x[is.na(x)] <- FALSE; return(x)}
      # BAD folder or missing param  
      if (missing(fname) || 
          (  !validfoldernotfile(dirname(fname))) ) {
        if ( !validfoldernotfile(dirname(fname))) {
          warning("ignoring filename because path was invalid")
        }
        fname <- create_filename(ext = ".html", file_desc = "results_bysite", buffer_dist = x$radius.miles[1])
        fname <- file.path(tempdir(), fname)
      } else {
        # good folder NOT WITH a filename, define a filename fname in that folder
        if (validfoldernotfile(fname)) {
          mydir = fname
          fname <- create_filename(ext = ".html", file_desc = "results_bysite", buffer_dist = x$radius.miles[1])
          fname = file.path(mydir, fname)
        } else {
          # good folder, WITH a filename w good extension, that may not yet exist?
          if (validfoldernotfile(dirname(fname)) & tools::file_ext(fname) == "html") {
            # all set
          } else {
            # good folder, WITH BAD extension
            if (validfoldernotfile(dirname(fname)) & !tools::file_ext(fname) == "html") {
              warning("wrong extension, so adding .html")
              fname = paste0(fname, ".html")
            }
          }
        }
      }
      # save file:
      htmlwidgets::saveWidget(dt, fname)    
      
      cat("\n")
      cat("Interactive table of sites is saved here: ", fname, '\n')
      cat(paste0("To open that folder: browseURL('", dirname(fname), "')\n"))
      cat(paste0("To view that report in a browser: browseURL('", fname, "')\n"))  
      
      # maybe launch external browser to view table:
      if (!shiny::isRunning() && launch_browser) {
        browseURL(fname)
      }
    }
  }
  
  ## shows table in the RStudio viewer pane by returning it (even if running shiny app locally - not sure what happens if on server)
  return(dt)
}
################################################# #
