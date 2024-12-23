#' PLACEHOLDER FOR POSSIBLE MODULE - view_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_view_results_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' view_results Server Functions
#'
#' @noRd
mod_view_results_server <- function(id) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_view_results_ui("view_results_1")

## To be copied in the server
# mod_view_results_server("view_results_1")
