stop("
 this script needs to be fixed - not currently working since 
 changes in use of get_global_defaults_or_user_options() and global_or_param()
")

## load installed version of EJAM package + data
library(EJAM) # probably redundant if doing load_all next
## load all EJAM functions, even if not-exported
devtools::load_all()

## load some other packages # probably redundant if loading EJAM pkg
library(shiny)
library(data.table)
library(tidyverse)
library(leaflet)
library(leaflet.extras2)
library(sf)

# quaddata and localtree seem to sometimes not get set after install 
# which causes the app to crash upon running the analysis
if (!exists("quaddata")) {
  dataload_from_local(varnames = "quaddata")
}
localtree <- SearchTrees::createTree( quaddata, treeType = "quad", dataType = "point")

## if you make any changes to a function, can re-run it with source
#source('R/example_function_that_I_changed.R')

## source app-related scripts
source('R/app_config.R')
source('R/app_ui.R')
source('R/app_server.R')

global_defaults_or_user_options <- get_global_defaults_or_user_options(
  user_specified_options = list()
)

## launch local version of Shiny app

shinyApp(app_ui, app_server)
