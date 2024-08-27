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
#' @noRd
mod_transactions_server <- function(id, transaction_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Display transactions data in a table
    output$transaction_table <- DT::renderDT({
      transaction_data()
    })

    # Reactive expression to handle file import
    import_data <- reactive({
      req(input$transaction_file)

      # Process the uploaded file
      new_data <- process_posting_csv(input$transaction_file$datapath)
      new_data
    }) |> bindEvent(input$import)

    # Open the import modal when the "Import CSV" button is clicked
    observe({
      showModal(create_import_modal(ns))
    }) |> bindEvent(input$open_import_modal)

    # Update the transactions data when the import button is pressed
    observe({
      transaction_data(import_data())
      removeModal()
    }) |> bindEvent(import_data())
  })
}

## To be copied in the UI
# mod_transactions_ui("transactions_1")

## To be copied in the server
# mod_transactions_server("transactions_1")
