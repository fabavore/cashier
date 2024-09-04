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
      fluidRow(
        column(
          width = 6,
          actionButton(ns("append"), "Add", icon = icon("add")),
          disabled(actionButton(ns("update"), "Edit", icon = icon("edit"))),
          disabled(actionButton(ns("delete"), "Delete", icon = icon("trash"))),
        ),
        column(
          width = 6,
          class = "text-right",
          disabled(actionButton(ns("move_up"), "Move up", icon = icon("caret-up"))),
          disabled(actionButton(ns("move_down"), "Move down", icon = icon("caret-down")))
        )
      ),
      br(),
      DT::DTOutput(ns("rule_table")),
      br(),
      fluidRow(
        column(
          width = 6,
          actionButton(ns("open_import_modal"), "Import CSV", icon = icon("upload"))
        ),
        column(
          width = 6,
          class = "text-right",
          downloadButton(ns("download"), "Download CSV", icon = icon("download"))
        )
      ),
      status = "primary",
      solidHeader = TRUE,
      width = 12
    )
  )
}

#' rules Server Functions
#'
#' @importFrom shinyjs toggleState
#' @importFrom DT selectRows
#' @importFrom dplyr slice pull mutate arrange select across
#' @importFrom tidyr separate_longer_delim
#' @importFrom stringr str_split_1
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
      toggleState("update", condition = not_null(input$rule_table_rows_selected))
      toggleState("delete", condition = not_null(input$rule_table_rows_selected))
      toggleState(
        "move_up",
        condition = not_null(input$rule_table_rows_selected) && input$rule_table_rows_selected > 1
      )
      toggleState(
        "move_down",
        condition = not_null(input$rule_table_rows_selected) && input$rule_table_rows_selected < nrow(data())
      )
    })

    edit_modal_choices <- reactive({
      list(
        category = data() |> pull(category) |> unique(),
        tags = data() |> separate_longer_delim(tags, ", ") |> pull(tags) |> unique()
      )
    })

    observe({
      showModal(create_rule_edit_modal(ns, edit_type = "append", choices = edit_modal_choices()))
    }) |> bindEvent(input$append)

    observe({
      ledger$rules$rows_append(data.frame(
        payee_name = input$payee_name,
        description = input$description,
        category = input$category,
        tags = input$tags |> paste(collapse = ", ")
      ))

      gargoyle::trigger("rules")
      removeModal()
    }) |> bindEvent(input$confirm_rule_append)

    observe({
      values <- data() |> slice(input$rule_table_rows_selected) |> as.list()
      values$tags <- values$tags |> str_split_1(", ")

      showModal(create_rule_edit_modal(
        ns, edit_type = "update",
        choices = edit_modal_choices(),
        values = values
      ))
    }) |> bindEvent(input$update)

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
    }) |> bindEvent(input$confirm_rule_update)

    observe({
      showModal(create_rule_edit_modal(ns, edit_type = "delete"))
    }) |> bindEvent(input$delete)

    observe({
      ledger$rules$rows_delete(data.frame(
        `id` = data() |> slice(input$rule_table_rows_selected) |> pull(`id`)
      ))

      gargoyle::trigger("rules")
      removeModal()
    }) |> bindEvent(input$confirm_rule_delete)

    observe({
      selected <- input$rule_table_rows_selected
      data_swapped <- data() |>
        slice(selected, selected - 1) |>
        mutate(`id` = rev(`id`))

      ledger$rules$rows_update(data_swapped |> mutate(across(-`id`, ~ `id` |> as.character())))
      ledger$rules$rows_update(data_swapped)
      gargoyle::trigger("rules")
    }) |> bindEvent(input$move_up)

    observe({
      selected <- input$rule_table_rows_selected
      data_swapped <- data() |>
        slice(selected, selected + 1) |>
        mutate(`id` = rev(`id`))

      ledger$rules$rows_update(data_swapped |> mutate(across(-`id`, ~ `id` |> as.character())))
      ledger$rules$rows_update(data_swapped)
      gargoyle::trigger("rules")
    }) |> bindEvent(input$move_down)

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
          `Precedence` = `id`,
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
