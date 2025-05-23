#' Access files from app code
#'
#' Utility from golem pkg to help code refer to files, in correct folder
#' 
#' @details 
#'   Since R will during installation move all source/EJAM/inst/xyz to
#'   installed/EJAM/xyz, we use golem app_sys() to ensure it points to 
#'   the right folder, which in source pkg is 
#'   EJAM/inst/app/www/ejam_styling.css but in
#'   installed pkg is EJAM/app/www/ejam_styling.css
#'   
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use `[golem::set_golem_name()]`
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
#' 
app_sys <- function(...) {
 
  system.file(..., package = "EJAM")  
  
# system.file() refers to the installed version of the package.
# Since R during installation moves source/EJAM/inst/xyz to installed/EJAM/xyz,
# we use golem app_sys() to ensure it points to the right folder,
# which in source pkg is EJAM/inst/app/www/ejam_styling.css
# but in installed pkg is EJAM/app/www/ejam_styling.css
}
###################################################################### #


#' Read App Config
#' 
#' Utility from golem package. Checks golem-config.yml
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE.
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the config file
#'
#' @noRd
#' 
get_golem_config <- function(
  value,
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    Sys.getenv(
      "R_CONFIG_ACTIVE",
      "default"
    )
  ),
  use_parent = TRUE,
  # Modify this if your config file is somewhere else
  file = app_sys("golem-config.yml") 
  #  source/EJAM/inst/filename = installed/EJAM/filename
) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}
# 
#  f config file to use, like "golem-config.yml"   
# can be found in EJAM/inst/ referred to as if in root 
# can use  config :: get  to read whatever value you asked about 
 # you can do  config::get("app_prod", file="golem-config.yaml") 
