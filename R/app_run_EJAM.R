#' @rdname run_app
#' @aliases run_app

#' @export
#' @keywords internal
#' 
app_run_EJAM <- function(    # SHOULD BE IDENTICAL TO run_app() now
    onStart = NULL,
    options = list(),
    
    enableBookmarking = 'url',
    ################################ #
    # this and the bookmarkButton() in ui let user save any uploaded files plus state of all  input$  settings, saved on server.
    # also see onBookmark() onBookmarked() onRestore() onRestored()
    # see https://mastering-shiny.org/action-bookmark.html or https://rdrr.io/cran/shiny/man/bookmarkButton.html
    # see  default_setBookmarkExclude
    uiPattern = "/",
    ...
) {
  run_app(
    onStart = onStart,
    options = options,
    enableBookmarking = enableBookmarking,
    uiPattern = uiPattern, 
    ... = ...
  )
}
## or 
# app_run_EJAM <- run_app
