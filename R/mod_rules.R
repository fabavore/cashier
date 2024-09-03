#' rules UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyjs useShinyjs disabled
mod_rules_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::bs4Card(
      title = "Matching Rules",
      actionButton(ns("open_import_modal"), "Import CSV", icon = icon("upload")),
      actionButton(ns("add"), "Add", icon = icon("add")),
      disabled(actionButton(ns("edit"), "Edit", icon = icon("edit"))),
      disabled(actionButton(ns("delete"), "Delete", icon = icon("trash"))),
      downloadButton(ns("download"), "Download CSV", icon = icon("download")),
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
#' @importFrom shinyjs toggleState
#' @importFrom dplyr slice pull
#' @importFrom tidyr separate_longer_delim
#' @importFrom readr write_csv2
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
      toggleState("delete", condition = not_null(input$rule_table_rows_selected))
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

    # Open the import modal when the "Import CSV" button is clicked
    observe({
      showModal(create_import_modal(ns))
    }) |> bindEvent(input$open_import_modal)

    # Update the rule data when the import button is pressed
    observe({
      req(input$csv_file)

      lapply(
        input$csv_file$datapath,
        function(file_name) {
          ledger$import_rules(readr::read_csv2(
            file_name, col_types = cols(.default = col_character())
          ))
        }
      )

      gargoyle::trigger("rules")
      removeModal()
    }) |> bindEvent(input$import)

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

    output$download <- downloadHandler(
      filename = function() {
        paste0("rules_", Sys.Date(), ".csv")
      },
      content = function(file) {
        data() |>
          select(
            `Counterparty Regex` = `payee_name`,
            `Description Regex` = `description`,
            `Category` = `category`,
            `Tags` = `tags`
          ) |>
          write_csv2(file)
      }
    )
  })
}

## To be copied in the UI
# mod_rules_ui("rules_1")

## To be copied in the server
# mod_rules_server("rules_1")
