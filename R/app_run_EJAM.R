#' Launch EJAM as shiny web app
#' @description Launches shiny web app from RStudio - [app_run_EJAM()] and [run_app()] are the same
#' @seealso [run_app()]
#' @inheritParams run_app
#' 
#' @export
#' 
app_run_EJAM <- function(
    onStart = NULL,
    options = list(),
    
    enableBookmarking = 'server',    ################################ #
    # this and the bookmarkButton() in ui let user save any uploaded files plus state of all  input$  settings, saved on server.
    # also see onBookmark() onBookmarked() onRestore() onRestored()
    # see https://mastering-shiny.org/action-bookmark.html or https://rdrr.io/cran/shiny/man/bookmarkButton.html
    
    uiPattern = "/",
    ...
) {
  run_app(
    onStart = onStart,
    options = options,
    enableBookmarking = enableBookmarking,
    uiPattern = uiPattern,
    ...
  )
}
