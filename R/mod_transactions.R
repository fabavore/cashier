#' transactions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyjs useShinyjs disabled toggleState
mod_transactions_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    bs4Dash::bs4Card(
      title = "Transactions",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      actionButton(ns("open_import_modal"), "Import CSV", icon = icon("upload")),
      disabled(actionButton(ns("open_rule_modal"), "Create rule from this transaction", icon = icon("add"))),
      br(), br(),
      DT::DTOutput(ns("transaction_table"))
    )
  )
}

#' transactions Server Functions
#'
#' @importFrom dplyr select slice pull
#' @importFrom DT renderDT
#' @noRd
mod_transactions_server <- function(id, ledger){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    data <- reactive({
      gargoyle::watch("postings")
      gargoyle::watch("rules")
      ledger$get_transactions()
    })

    # Display transactions data in a table
    output$transaction_table <- renderDT({
      data() |>
        select(
          `Account` = `account_name`,
          `Date` = `booking_date`,
          `Counterparty` = `payee_name`,
          `Description` = `description`,
          `Amount` = `amount`,
          `Currency` = `currency`,
          `Category` = `category`,
          `Tags` = `tags`
        )
    },
    rownames = FALSE,
    selection = "single"
    )

    # Open the import modal when the "Import CSV" button is clicked
    observe({
      showModal(create_import_modal(ns))
    }) |> bindEvent(input$open_import_modal)

    # Update the transactions data when the import button is pressed
    observe({
      req(input$csv_file)

      input$csv_file$datapath |>
        process_posting_csv() |>
        import_postings(ledger)

      gargoyle::trigger("accounts")
      gargoyle::trigger("postings")
      removeModal()
    }) |> bindEvent(input$import)

    observe({
      toggleState("open_rule_modal", condition = not_null(input$transaction_table_rows_selected))
    })

    # Open the rule modal when the according button is pressed
    observe({
      req(input$transaction_table_rows_selected)

      showModal(create_rule_modal(
        ns,
        values = data() |> slice(input$transaction_table_rows_selected),
        choices = list(
          category = ledger$rules$data |> pull(category) |> unique(),
          tags = ledger$rules$data |> pull(tags) |> unique()
        )
      ))
    }) |> bindEvent(input$open_rule_modal)

    observe({
      new_rule <- data.frame(
        payee_name = input$payee_name,
        description = input$description,
        category = input$category,
        tags = paste(input$tags, collapse = ", ")
      )
      gargoyle::trigger("rules")
      ledger$rules$rows_append(new_rule)
      removeModal()
    }) |> bindEvent(input$create_rule)
  })
}

## To be copied in the UI
# mod_transactions_ui("transactions_1")

## To be copied in the server
# mod_transactions_server("transactions_1")
