#' rules UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rules_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' rules Server Functions
#'
#' @noRd
mod_rules_server <- function(id, ledger){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_rules_ui("rules_1")

## To be copied in the server
# mod_rules_server("rules_1")
