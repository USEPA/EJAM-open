
## load EJAM package + data
library(EJAM)

## load some other packages
library(shiny)
library(data.table)
library(tidyverse)
library(leaflet)
library(leaflet.extras2)
library(sf)

## load all EJAM functions, even if not-exported
devtools::load_all()

# quaddata and localtree seem to sometimes not get set after install 
# which causes the app to crash upon running the analysis
if (!exists("quaddata")) {
  EJAM:::dataload_from_local(varnames = "quaddata")
}
localtree <- SearchTrees::createTree( quaddata, treeType = "quad", dataType = "point")

## if you make any changes to a function, can re-run it with source
#source('R/example_function_that_I_changed.R')

## source app-related scripts
source('R/app_config.R')
source('R/app_ui.R')
source('R/app_server.R')

# This might be somewhat obsolete considering get_global_defaults_or_user_options() etc.:
global_defaults_or_user_options <- list()
source('inst/global_defaults_package.R')
source('inst/global_defaults_shiny.R')
source('inst/global_defaults_shiny_public.R')

## launch local version of Shiny app
shinyApp(app_ui, app_server)
