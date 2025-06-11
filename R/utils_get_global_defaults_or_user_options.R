
#' utility that reconciles/consolidates user-defined params passed via run_app() and default settings from global_defaults_ files
#'
#' @param user_specified_options passed as arguments to `run_app()`
#' @param bookmarking_allowed same as `?shinyApp` enableBookmarking param
#'
#' @returns a list of global defaults or user options that run_app()
#'   uses as the golem_opts parameter in `golem::with_golem_options()`
#'   and that later can be retrieved by server or ui via `golem::get_golem_options()`
#'   or via `global_or_param()` (which both do almost the same thing).
#' @details
#' This function, called by `run_app()`,
#' collects the shiny-app-related default settings that are defined in these places:
#'   
#' 1. parameters passed to `run_app()` by a user
#' 
#' 2. settings defined in file `global_defaults_shiny_public.R`
#' 
#' 3. settings defined in file `global_defaults_shiny.R`
#' 
#' 4. settings defined in file `global_defaults_package.R`
#' 
#' and consolidates them all as a list, to be available to server/ui.
#' 
#' @keywords internal
#'
get_global_defaults_or_user_options <- function(user_specified_options = NULL, bookmarking_allowed = 'url') {

  ############ #  
  # 1. parameters passed to run_app() by a user ####
  # 
  # Save whatever options user specifies
  # Options unspecified may have default values in global_defaults_shiny_public.R
  # The final list of shiny options gets passed to golem_opts
  
  global_defaults_or_user_options <- user_specified_options
  global_defaults_or_user_options$bookmarking_allowed <- bookmarking_allowed
  
  ############ # 
  # helper func
  update_global_defaults_or_user_options <- function(app_defaults) {
    
    # Function to consolidate/update shiny defaults, 
    # prioritizing user-specified settings (in ...)
    # over pre-specified ones in global_defaults_*.R files
    
    for (o in names(app_defaults)) {
      if (!o %in% names(global_defaults_or_user_options)) {
        global_defaults_or_user_options[[o]] <- app_defaults[[o]]
      }
    }
    return(global_defaults_or_user_options)
  }

  ############ #  
  # 2. settings defined in file global_defaults_shiny_public.R ####
  # 
  # sets switches controlling what is displayed in public version based on passed isPublic parameter
  
  source(system.file("global_defaults_shiny_public.R", package = "EJAM"), local = TRUE)
  global_defaults_or_user_options <- update_global_defaults_or_user_options(global_defaults_shiny_public)
  
  ############ #  
  # 3. settings defined in file global_defaults_shiny.R ####
  # 
  # temporary workaround, see https://github.com/ThinkR-open/golem/issues/6
  
  source(system.file("global_defaults_shiny.R", package = "EJAM"), local = TRUE)  # uses latest source version of that file if devtools::load_all() has been done.
  global_defaults_or_user_options <- update_global_defaults_or_user_options(global_defaults_shiny)
  global_defaults_or_user_options <- update_global_defaults_or_user_options(help_texts)
  global_defaults_or_user_options <- update_global_defaults_or_user_options(html_fmts)
  global_defaults_or_user_options <- update_global_defaults_or_user_options(sanitize_functions)
  global_defaults_or_user_options <- update_global_defaults_or_user_options(extratable_stuff)
  
  ############ #  
  # 4. settings defined in file global_defaults_package.R ####
  
  source(system.file("global_defaults_package.R", package = "EJAM"), local = TRUE) # # uses latest source version of that file if devtools::load_all() has been done.
  global_defaults_or_user_options <- update_global_defaults_or_user_options(global_defaults_package)
  
  ############ #  
  rm(global_defaults_shiny_public)
  rm(global_defaults_shiny)
  rm(global_defaults_package)
  
  return(global_defaults_or_user_options)
}
