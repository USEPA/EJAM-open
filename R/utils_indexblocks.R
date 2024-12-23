
#' Create localtree (a quadtree index of all US block centroids) in global environment
#' 
#' @details Note this is duplicated code in .onAttach() and also in global.R  
#' 
#'    .onAttach() can be edited to create this when the package loads, 
#'   but then it takes time each time a developer rebuilds/installs the package or others that load EJAM.
#'   
#' It also has to happen in global.R if it has not already.
#' @return Returns TRUE when done. Side effect is it creates the index in memory.
#' 
#' @export
#'
indexblocks <- function() {
  
  cat("Checking for index of Census blocks called 'localtree' ...")
  if (!exists("localtree")) {
    cat('not found...')
    if (!exists("quaddata")) {
      cat(    "\n index cannot be created until quaddata is loaded ... Trying dataload_from_pins() ... \n")
      message("The index of Census block groups (localtree) cannot be created until quaddata is loaded. Trying dataload_from_pins()")
      dataload_from_pins("quaddata")
    } else {
      cat("Building index...")
      assign(
        "localtree",
        SearchTrees::createTree(quaddata, treeType = "quad", dataType = "point"),
        envir = globalenv()
        # need to test, but seems to work.
        # But takes a couple seconds at every reload of pkg.
      )
      if (exists("localtree")) {
        cat("  Done building index.\n")        
      } else {
        cat("  Failed to build index.\n")
        warning("indexblocks() failed to build index of Census blocks, 'localtree' ")
      }
    }
  } else {
    cat('localtree already exists.\n')
  }
  return(TRUE)
}
