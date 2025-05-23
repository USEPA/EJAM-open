
get_global_defaults_or_user_options <- function(user_specified_options, bookmarking_allowed) {
  
  # Save whatever options user specifies
  # Options unspecified may have default values in global_defaults_shiny_public.R
  # The final list of shiny options gets passed to golem_opts
  global_defaults_or_user_options <- user_specified_options
  global_defaults_or_user_options$bookmarking_allowed <- bookmarking_allowed
  
  # Function to update shiny defaults, prioritizing user-specified (in ...)
  # over pre-specified
  update_global_defaults_or_user_options <- function(app_defaults) {
    for (o in names(app_defaults)) {
      if (!o %in% names(global_defaults_or_user_options)) {
        global_defaults_or_user_options[[o]] <- app_defaults[[o]]
      }
    }
    return(global_defaults_or_user_options)
  }
  # sets switches controlling what is displayed in public version based on passed isPublic parameter
  source(system.file("global_defaults_shiny_public.R", package = "EJAM"), local = TRUE)
  global_defaults_or_user_options <- update_global_defaults_or_user_options(global_defaults_shiny_public)
  
  # temporary workaround, see https://github.com/ThinkR-open/golem/issues/6
  source(system.file("global_defaults_shiny.R", package = "EJAM"), local = TRUE)  # uses latest source version if devtools::load_all() has been done.
  global_defaults_or_user_options <- update_global_defaults_or_user_options(global_defaults_shiny)
  global_defaults_or_user_options <- update_global_defaults_or_user_options(help_texts)
  global_defaults_or_user_options <- update_global_defaults_or_user_options(html_fmts)
  global_defaults_or_user_options <- update_global_defaults_or_user_options(sanitize_functions)
  global_defaults_or_user_options <- update_global_defaults_or_user_options(extratable_stuff)
  
  source(system.file("global_defaults_package.R", package = "EJAM"), local = TRUE) # # uses latest source version if devtools::load_all() has been done.
  global_defaults_or_user_options <- update_global_defaults_or_user_options(global_defaults_package)
  
  rm(global_defaults_shiny_public)
  rm(global_defaults_shiny)
  rm(global_defaults_package)
  
  return(global_defaults_or_user_options)
}
