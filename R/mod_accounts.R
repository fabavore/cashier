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
      actionButton(ns("add"), "Add account", icon = icon("add")),
      disabled(actionButton(ns("edit"), "Edit account", icon = icon("edit"))),
      disabled(actionButton(ns("delete"), "Delete account", icon = icon("trash"))),
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

    observe({
      toggleState("edit", condition = not_null(input$account_table_rows_selected))
      toggleState("delete", condition = not_null(input$account_table_rows_selected))
    })

    # Add account --------------------------------------------------------------
    observe({
      showModal(create_add_account_modal(ns))
    }) |> bindEvent(input$add)

    observe({
      removeModal()
    }) |> bindEvent(input$confirm_add)

    # Edit account -------------------------------------------------------------
    observe({
      showModal(create_edit_account_modal(ns))
    }) |> bindEvent(input$edit)

    observe({
      removeModal()
    }) |> bindEvent(input$confirm_edit)

    # Delete account -----------------------------------------------------------
    observe({
      showModal(create_delete_account_modal(ns))
    }) |> bindEvent(input$delete)

    observe({
      removeModal()
    }) |> bindEvent(input$confirm_delete)

    output$account_table <- DT::renderDT({
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
