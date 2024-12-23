#' PLACEHOLDER FOR POSSIBLE MODULE - specify_sites UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_specify_sites_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' specify_sites Server Functions
#'
#' @noRd 
mod_specify_sites_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_specify_sites_ui("specify_sites_1")
    
## To be copied in the server
# mod_specify_sites_server("specify_sites_1")
