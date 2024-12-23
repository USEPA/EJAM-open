#' utility - interactive prompt in RStudio to ask user to specify number like radius
#' 
#' same as askradius()
#' @aliases askradius
#' @param default default value for the number to be provided and returned
#' @param title title of popup dialog box, like "Radius"
#' @param message question, like, "Within how many miles of each point?"
#' @seealso askYesNo()
#' @return a single number
#' 
#' @keywords internal
#'
ask_number <- function(default = 3, title = "Radius", message = "Within how many miles of each point?") {
  radius <- NA
  while (is.na(radius)) {
    radius <- as.numeric(rstudioapi::showPrompt(title = title, message = message, default = default))
    if (length(radius) == 0) {radius <- NA}
  }
  if (title == "Radius" & missing(message)) {units_text <- " miles."} else {units_text <- ""}
  cat("Specified value is", radius, units_text, '\n')
  return(radius)
}


askradius <- ask_number
