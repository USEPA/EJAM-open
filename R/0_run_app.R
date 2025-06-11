
#' Launch EJAM as Shiny web app in RStudio.
#' @description Launch app. [app_run_EJAM()] & [run_app()] are the same.
#' @param ... arguments to pass to golem_opts.
#'   Note that many defaults are defined in files like global_defaults_*.R.
#'   They can be changed there, but also
#'   can be passed here to override those defaults for the duration of the app.
#'
#'   For example:
#'   ```
#'   run_app(
#'     default_default_miles=3.1,
#'     default_max_miles=31,
#'     default_max_mb_upload=100
#'   )
#'
#'   ```
#' @details
#' `run_app( isPublic = TRUE )` will launch a simpler version of the web app
#'  (e.g., for more general public use rather than the full set of complicated
#'  features that are used less often).
#'
#' Technical Notes:
#'
#' Examples of other parameters that you could pass to run_app()
#' but these are not fully tested:
#'  ```
#' run_app(shiny.testmode = TRUE, testing = TRUE,
#'         default_default_miles = 3.1,
#'         default_max_miles = 31,
#'         default_max_mb_upload = 100,
#'         default_default_miles_shapefile = 0.1,
#'
#'         default_hide_plot_histo_tab = FALSE,
#'         default_hide_advanced_settings = FALSE,
#'         default_testing = TRUE,
#'         default_shiny.testmode = TRUE,
#'
#'         default_standard_analysis_title = "CUSTOM REPORT",
#'
#'         default.an_threshgroup1 = "Envt-US-or-ST",
#'         default.an_thresh_comp1 = 95,
#'         default.an_threshnames1 = EJAM::names_e_state_pctile,
#'
#'         # to have shapefiles, MACT, Cities as initial (default) selections:
#'         ## default_upload_dropdown = "dropdown", #may not be implemented yet
#'         default_choices_for_type_of_site_upload =c('Shapefile upload'='SHP'),
#'         default_choices_for_type_of_site_category=c('by MACT subpart'='MACT'),
#'         fipspicker_fips_type2pick_default = "Cities or Places",
#'
#'         # shorter reorganized list of extra indicators:
#'         default_extratable_list_of_sections = list(
#'           Health = c("pctdisability", "lowlifex",
#'             "rateheartdisease", "rateasthma", "ratecancer", "lifexyears"),
#'           Poverty_Income = c("pctpoor",  "percapincome"),
#'           `Feature Counts` = c("count.NPL", "count.TSDF",
#'             "num_waterdis", "num_airpoll", "num_brownfield", "num_tri",
#'             "num_school", "num_hospital", "num_church")
#'         )
#' )
#'
#' # untested:    sitepoints = testpoints_10 # or 'latlondata.xlsx'
#' ```
#'
#' The `enableBookmarking` param lets a user, via the [bookmarkButton()] in ui,
#' save any uploaded files plus state of all  input$  settings.
#' if enableBookmarking="url" that is all saved on the server.
#' See [shinyApp()] [onBookmark()] [onBookmarked()] [onRestore()] [onRestored()]
#' See https://mastering-shiny.org/action-bookmark.html
#' or https://rdrr.io/cran/shiny/man/bookmarkButton.html
#'
#' Typically R Shiny apps are not distributed as R packages and
#' launching a shiny app will just source() all .R files found in /R/ folder,
#' and then run what is found in `app.R` (assuming it is a one-file Shiny app).
#'
#' This R Shiny app, however, is shared as an R package,
#' via the [golem package](https://thinkr-open.github.io/golem/) approach,
#' which provides the useful features of a package and
#' useful features that the golem package enables.
#'
#' There is a file `app.R` in the package root,
#' used when the shiny app
#' is started locally via RStudio's Run button or on posit connect server.
#' The `app.R` script does `library(EJAM)` if the pkg is not already loaded,
#' and then does [EJAM::run_app()] which
#'
#' There is a file called `_disable_autoload.R` in the /R/ folder
#' used when the shiny app is started,
#' to tell the server to NOT source all the .R files,
#' since they are already being loaded from the package by [run_app()]
#'
#' @inheritParams shiny::shinyApp
#' @return An object that represents the app. Printing the object or
#'   passing it to [runApp()] will run the app.
#' @seealso [app_run_EJAM()]
#' @aliases app_run_EJAM
#'
#' @export
#'
run_app      <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = 'url',
    uiPattern = "/",
    ...
) {

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
}
