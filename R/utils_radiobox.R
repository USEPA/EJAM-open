

#' DRAFT - addin/ gadget dialog box so RStudio user can pick a radio button
#' Interactive dialog box of choices (RStudio addin that wraps a Shiny Gadget)
#'
#' @param choiceNames vector of options displayed, e.g., c("Points", "Shapes", "FIPS")
#' @param choiceValues vector of corresponding values as returned by the function, e.g., c("latlon", "shp", "fips")
#' @param title Appears just above the list of choices, e.g., "Select One"
#' @param label Appears at top of dialog box and between cancel and done, e.g., "Choose one:"
#' @param height height of box in pixels, e.g., 250
#' @param width width of box in pixels, e.g., 100
#' @return one of the choiceValues (if not cancelled/ error), once Done is clicked.
#'
#' @details uses [shiny::runGadget()]
#' 
#'   *** WARNING: AS DRAFTED, CANNOT use within nontrivial scripts or functions 
#'   because the [stopApp()] seems to interrupt other processes and cause problems -
#'   and seems related to a quirk seen if a script or function calls radiobox() twice - 
#'   it will work the first time but show a blank popup window the 2d time...
#'   e.g., if you  try to do this:
#'   radius1 <- radiobox()
#'   radius2 <- radiobox()
#'   May all be related to this issue: https://github.com/rstudio/rstudio/issues/13394
#'   
#'   Note this function could be defined as an RStudio addin and assigned a keyboard shortcut, if that is useful.
#'   
#' @examples
#' # chosen <- radiobox()
#' # cat("you chose", chosen, '\n')
#' \dontrun{
#'  junk = function() {
#'   z =  radiobox()
#'   # print(z)
#'   return(z)
#'  }
#'  # (note this works after load_all or if it is an exported function)
#'  radius <- radiobox(
#'   c("Far (3 miles)", "Medium (2 miles)", "Near (1 mile)"),
#'   c(3,2,1),
#'   label = "Radius"
#'  )
#'  cat("The radius will be", radius, "miles. \n")
#' 
#' }
#' 
#' @keywords internal
#'
radiobox <- function(choiceNames  = c("Points", "Shapes", "FIPS"), # what is shown
                     choiceValues = c("latlon", "shp", "fips"),    # what is returned
                     label = "Choose one:",  #
                     title = "", # 
                     height = 250, width = 100) {
  
  if (!missing(choiceNames) & missing(choiceValues)) {
    # this way, the user can specify only the 1st parameter, like  x = radiobox(1:3)
    choiceValues <- choiceNames
  }
  ######################################## #
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(title),
    miniUI::miniContentPanel(
      shiny::radioButtons('radiobuttons', 
                          label = label, 
                          choiceNames  = choiceNames, 
                          choiceValues = choiceValues) 
    )
  )
  ######################################## #
  
  server <- function(input, output, session) {
    
    shiny::observeEvent(input$done, {
      value_chosen <- input$radiobuttons
      # return(value_chosen) # this would not stop the app
      shiny::stopApp(returnValue = value_chosen)
    })
    shiny::observeEvent(input$cancel, {
      shiny::stopApp() # returns NULL upon cancel button press, and no error
      # shiny::stopApp(returnValue = NULL) # returns NULL upon cancel button press, and no error
      # but clicking the x to close the window returns nothing at all, and no error code.
    })
  }
  ########################################## #
  
  dviewer <- shiny::dialogViewer(dialogName = title, height = height, width = width)
  shiny::runGadget(app = ui, server = server,
                             stopOnCancel = FALSE, # To handle input$cancel via our own observeEvent 
                             viewer = dviewer
  )
}
