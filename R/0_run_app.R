
#' Launch EJAM as Shiny web app
#' @description Launches shiny web app from RStudio - [app_run_EJAM()] and [run_app()] are the same
#' @param ... arguments to pass to golem_opts.
#'   Note that many defaults are defined in global_defaults_*.R. They can be changed there
#'   or passed here, to override the defaults for the duration of the app
#'   ```
#'   run_app(
#'     default_default_miles=3.1, 
#'     default_max_miles=31, 
#'     default_max_mb_upload=100, 
#'     default_default_miles_shapefile = 0.5, 
#'     advanced=TRUE, 
#'     testing=TRUE, 
#'     shiny.testmode=TRUE, 
#'     # and this will be implemented at some point: 
#'     sitepoints = mytable such as testpoints_100
#'     ## or maybe
#'     ## sitepoints = 'latlondata.xlsx'
#'   )
#'   ```
#'   This uses an internal function global_or_param()
#' @details
#' run_app( isPublic = TRUE ) will launch the public-facing version.
#' 
#' @inheritParams shiny::shinyApp
#' @return An object that represents the app. Printing the object or passing it to [runApp()] will run the app.
#' @seealso [app_run_EJAM()]
#' @aliases app_run_EJAM
#' 
#' @export
#'
#'
run_app      <- function(
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
  # This with_golem_options()  just does  shinyApp(ui = app_ui, server = app_server)
  #   which mean app_ui and app_server() need to have been loaded/attached via loadall
  #   (but it can show maintenance_page if that option is set.)
  global_defaults_or_user_options <- EJAM:::get_global_defaults_or_user_options(
    user_specified_options = list(...),
    bookmarking_allowed = enableBookmarking
  )
  
  golem::with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = global_defaults_or_user_options
  )

  # Normally R Shiny apps are not R packages -
  # The server just sources all .R files found in the /R/ folder,
  # and then runs what is found in app.R (if that is found / it is a one-file Shiny app).
  # This R Shiny app, however, is shared as an R package,
  # via the golem package approach,
  # which provides the useful features of a package and
  # useful features that the golem package enables.
  #
  # There is still an app.R script in the package root
  # -- note there is no function called app()  --
  # which lets RStudio Connect source the app.R script
  # to launch this shiny app.
  #
  # The way this works is that there is a file called
  #
  #  _disable_autoload.R in the /R/ folder
  #
  # to tell the server to not source all the source .R files,
  # since they are already in the installed package.
  # Then they get loaded from the package because
  # the app.R script here says this:
  #
  #   pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
  #
  # with the shinyApp() call wrapped in shiny::runApp() rather than in app()
  #
  # Also, app_runYYYY() is the same as YYYY::run_app() in case that is useful.
  #
  # See https://thinkr-open.github.io/golem/
}
