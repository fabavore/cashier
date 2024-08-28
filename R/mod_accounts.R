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
#' @importFrom shinyjs useShinyjs disabled toggleState
mod_accounts_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    bs4Card(
      title = "Accounts",
      disabled(actionButton(ns("edit"), "Edit", icon = icon("edit"))),
      br(), br(),
      DT::DTOutput(ns("account_table")),
      status = "primary",
      solidHeader = TRUE,
      width = 12
    )
  )
}

#' accounts Server Functions
#'
#' @noRd
mod_accounts_server <- function(id, ledger){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    data <- reactive({
      gargoyle::watch("accounts")
      ledger$accounts$get_data()
    })

    observe({
      toggleState("edit", condition = not_null(input$account_table_rows_selected))
    })

    observe({
      showModal(create_edit_account_modal(
        ns,
        values = data() |> slice(input$account_table_rows_selected)
      ))
    }) |> bindEvent(input$edit)

    observe({
      ledger$accounts$rows_update(data.frame(
        `id` = data() |> slice(input$account_table_rows_selected) |> pull(`id`),
        account_iban = input$account_iban,
        account_name = input$account_name,
        opening_date = input$opening_date,
        opening_amount = input$opening_amount,
        currency = input$currency
      ))

      gargoyle::trigger("accounts")
      removeModal()
    }) |> bindEvent(input$confirm_edit)

    output$account_table <- DT::renderDT({
      data() |>
        select(
          `Account IBAN` = `account_iban`,
          `Account Name` = `account_name`,
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
