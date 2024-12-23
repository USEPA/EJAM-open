############################################# #
# pins available? (to build vignettes) ####
dataload_pin_available <- function(boardfolder = "Mark",
                                   auth = "auto",
                                   server = "https://rstudio-connect.dmap-stage.aws.epa.gov", 
                                   silent = FALSE) {
  
  offline_warning()
  board <- tryCatch(pins::board_connect(server = server, auth = auth),
                    error = function(e) e)
  if (inherits(board, "error")) {
    board_available <- FALSE
    if (!silent) {cat("Failed trying to connect to pins board server.\n\n")}
  } else {
    board_available <- TRUE
    if (!silent) {cat("Successfully connected to Posit Connect pins board.\n\n")}
  }
  return(board_available)
}
############################################# #
