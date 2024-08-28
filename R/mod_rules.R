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
    bs4Dash::bs4Card(
      title = "Transactions",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      actionButton(ns("open_import_modal"), "Import CSV", icon = icon("upload")),
      br(), br(),
      DT::DTOutput(ns("rule_table"))
    )
  )
}

#' rules Server Functions
#'
#' @noRd
mod_rules_server <- function(id, ledger){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Reactive expression to handle file import
    import_data <- reactive({
      req(input$csv_file)

      # Process the uploaded file
      new_data <- process_csv(input$csv_file$datapath)
      new_data
    }) |> bindEvent(input$import)

    # Open the import modal when the "Import CSV" button is clicked
    observe({
      showModal(create_import_modal(ns))
    }) |> bindEvent(input$open_import_modal)

    # Update the transactions data when the import button is pressed
    observe({
      ledger$rules$rows_append(import_data())
      gargoyle::trigger("rules")
      removeModal()
    }) |> bindEvent(import_data())

    # Display rules data in a table
    output$rule_table <- DT::renderDT({
      gargoyle::watch("rules")
      ledger$rules$get_data() |>
        select(
          `Counterparty Regex` = `payee_name`,
          `Description Regex` = `description`,
          `Category` = `category`,
          `Tags` = `tags`
        )
    },
    rownames = FALSE)
  })
}

## To be copied in the UI
# mod_rules_ui("rules_1")

## To be copied in the server
# mod_rules_server("rules_1")
