
#' DRAFT - Launch EJAM ejscreenapi web app in RStudio
#' @description Launch EJAM ejscreenapi web app in RStudio
#' @md
#' @details 
#' 
#' app_run_EJAM()                 is like run_app() for the EJAM shiny app run from the EJAM package
#' 
#' app_run_EJAMejscreenapi()      is like run_app() for the EJScreen API shiny app run from the EJAM package
#' 
#' @param ... arguments to pass to golem_opts
#'   See `?golem::get_golem_options` for more details
#' @inheritParams shiny::shinyApp
#' 
#' @keywords internal
#'  
app_run_EJAMejscreenapi <- function(
    
  onStart = NULL,
  options = list(),
  uiPattern = "/",
  enableBookmarking = NULL,  # or "server"
  ...
) {
  
  # temporary workaround, see https://github.com/ThinkR-open/golem/issues/6
  #
  source(system.file("global_defaults_ejscreenapi.R", package = "EJAM"))

  # # clean up global env when done with app ? ####
  # # if the user had any of these on purpose before launching app, this might be a problem to remove them?
  # #
  # on.exit({ # leftovers could be created by the shiny app at launch or as the pkg is attached
  #   rm(list = intersect(ls(), c("apptitle", "asExcel", "base_color_default", "circleweight", 
  #                               "cluster_color_default", "echo_message", "echo_url", "highlight_color_default", 
  #                               "ips", "maxmaxpts", "maxpts_map", "maxradius", "minradius", "opacitymax", 
  #                               "opacitymin", "opacityratio", "opacitystart", "opacitystep", 
  #                               "perhourfast", "perhourguess", "perhourslow", "stepradius", "tabletips_message", "whichip")))
  # })
    
  golem::with_golem_options(
    app = shinyApp(
      ui      = app_ui_EJAMejscreenapi,
      server  = app_server_EJAMejscreenapi,
      onStart = onStart,
      options = options,
      uiPattern = uiPattern,
      enableBookmarking = enableBookmarking
    ),
    golem_opts = list(...)
  )
}
####################################################### # 

