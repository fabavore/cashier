#' cash_flow UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cash_flow_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' cash_flow Server Functions
#'
#' @noRd 
mod_cash_flow_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_cash_flow_ui("cash_flow_1")
    
## To be copied in the server
# mod_cash_flow_server("cash_flow_1")
