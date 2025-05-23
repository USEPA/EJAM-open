
#' Utility checking values of input$ that appear in this code
#' 
#' See appsilon pkg shiny.info now
#' @param file  path to source file to search in 
#'
#' @return character vector of ids of inputs like x,y,z if it found input$x input$y input$z
#' 
#' @keywords internal
#' 
input_names_listing <- function(file="./R/app_server.R") {
  
  x = readLines(file)
  
  cat("Found these input$____  values in that source file ",file,"  input$____ :\n\n")
  # this is not perfect, but roughly gets the list of inputs:
  sort(
    gsub("input\\$", "", unique(grep("input\\$", gsub("(.*)(input\\$[A-Za-z0-9_-]*)(.*)","\\2", x), value = T)))
  )
}
