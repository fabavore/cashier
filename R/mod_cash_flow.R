#' cash_flow UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cash_flow_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Dash::box(
        title = "Cash Flow Over Time",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        plotly::plotlyOutput(ns("cash_flow_plot"))
      )
    )
  )
}

#' cash_flow Server Functions
#'
#' @noRd
#'
#' @importFrom stringr str_starts str_remove
mod_cash_flow_server <- function(id, transactions, type = c("Income", "Expenses")){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    cash_flow <- reactive({
      cash_flow <- calculate_cash_flow(transactions()) |>
        filter(category |> str_starts(paste0(type, ": "))) |>
        mutate(category = category |> str_remove(paste0("^", type, ": ")))

      switch (type,
        Income = cash_flow,
        Expenses = cash_flow |> mutate(amount = -amount)
      )
    })

    output$cash_flow_plot <- renderPlotly({
      cash_flow() |>
        plot_ly(x = ~ month, y = ~ amount, color = ~ category, type = "bar") |>
        layout(xaxis = list(title = "Month"), yaxis = list(title = 'Amount'), barmode = 'stack')
    })
  })
}

## To be copied in the UI
# mod_cash_flow_ui("cash_flow_1")

## To be copied in the server
# mod_cash_flow_server("cash_flow_1")
