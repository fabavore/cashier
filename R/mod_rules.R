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
      title = "Matching Rules",
      actionButton(ns("open_import_modal"), "Import CSV", icon = icon("upload")),
      disabled(actionButton(ns("edit"), "Edit", icon = icon("edit"))),
      br(), br(),
      DT::DTOutput(ns("rule_table")),
      status = "primary",
      solidHeader = TRUE,
      width = 12
    )
  )
}

#' rules Server Functions
#'
#' @importFrom dplyr slice pull
#' @importFrom tidyr separate_longer_delim
#' @noRd
mod_rules_server <- function(id, ledger){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    data <- reactive({
      gargoyle::watch("rules")
      ledger$rules$get_data()
    })

    observe({
      toggleState("edit", condition = not_null(input$rule_table_rows_selected))
    })

    observe({
      values <- data() |> slice(input$rule_table_rows_selected)

      showModal(create_edit_rule_modal(ns, values))
      updateSelectizeInput(
        session, "category",
        choices = data() |> pull(category) |> unique(),
        selected = values |> pull(category)
      )
      updateSelectizeInput(
        session, "tags",
        choices = data() |> separate_longer_delim(tags, ", ") |> pull(tags) |> unique(),
        selected = values |> separate_longer_delim(tags, ", ") |> pull(tags)
      )
    }) |> bindEvent(input$edit)

    observe({
      ledger$rules$rows_update(data.frame(
        `id` = data() |> slice(input$rule_table_rows_selected) |> pull(`id`),
        payee_name = input$payee_name,
        description = input$description,
        category = input$category,
        tags = input$tags |> paste(collapse = ", ")
      ))

      gargoyle::trigger("rules")
      removeModal()
    }) |> bindEvent(input$confirm_edit)

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
      data() |>
        select(
          `Counterparty Regex` = `payee_name`,
          `Description Regex` = `description`,
          `Category` = `category`,
          `Tags` = `tags`
        )
    },
    select = "single",
    rownames = FALSE)
  })
}

## To be copied in the UI
# mod_rules_ui("rules_1")

## To be copied in the server
# mod_rules_server("rules_1")
