#' accounts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bs4Dash bs4Card
#' @importFrom DT DTOutput
mod_accounts_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Card(
      title = "Accounts",
      DTOutput(ns("account_table")),
      status = "primary",
      solidHeader = TRUE,
      width = 12
    )
  )
}

#' accounts Server Functions
#'
#' @noRd
#'
#' @importFrom DT renderDT
#' @importFrom dplyr select
mod_accounts_server <- function(id, ledger){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$account_table <- renderDT({
      gargoyle::watch("accounts")
      ledger$accounts$get_data() |>
        select(
          `Account Name` = `account_name`,
          `Account IBAN` = `account_iban`,
          `Opening Date` = `opening_date`,
          `Opening Amount` = `opening_amount`,
          `Currency` = `currency`
        )
    },
    rownames = FALSE)
  })
}

## To be copied in the UI
# mod_accounts_ui("accounts_1")

## To be copied in the server
# mod_accounts_server("accounts_1")
