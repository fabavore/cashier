#' transactions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_transactions_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::bs4Card(
      title = "Transactions",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      actionButton(ns("open_import_modal"), "Import CSV", icon = icon("upload")),
      br(), br(),
      DT::DTOutput(ns("transaction_table"))
    )
  )
}

#' transactions Server Functions
#'
#' @importFrom dplyr select
#' @noRd
mod_transactions_server <- function(id, ledger){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Reactive expression to handle file import
    import_data <- reactive({
      req(input$csv_file)

      # Process the uploaded file
      new_data <- process_posting_csv(input$csv_file$datapath)
      new_data
    }) |> bindEvent(input$import)

    # Open the import modal when the "Import CSV" button is clicked
    observe({
      showModal(create_import_modal(ns))
    }) |> bindEvent(input$open_import_modal)

    # Update the transactions data when the import button is pressed
    observe({
      ledger$postings$rows_append(import_data())
      gargoyle::trigger("postings")
      removeModal()
    }) |> bindEvent(import_data())

    # Display transactions data in a table
    output$transaction_table <- DT::renderDT({
      gargoyle::watch("postings")
      ledger$postings$get_data() |>
        select(
          `Booking Date` = `booking_date`,
          `Value Date` = `value_date`,
          `Counterparty` = `payee_name`,
          `Purpose` = `purpose`,
          `Amount` = `amount`,
          `Currency` = `currency`
        )
    },
    rownames = FALSE
    )
  })
}

## To be copied in the UI
# mod_transactions_ui("transactions_1")

## To be copied in the server
# mod_transactions_server("transactions_1")
