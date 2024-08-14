#' dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dashboard_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::dashboardPage(
      bs4Dash::dashboardHeader(
        title = bs4Dash::dashboardBrand("CashieR Finance Manager")
      ),
      bs4Dash::dashboardSidebar(
        bs4Dash::sidebarMenu(
          bs4Dash::menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
          bs4Dash::menuItem("Expenses", tabName = "expenses", icon = icon("file-invoice-dollar")),
          bs4Dash::menuItem("Revenues", tabName = "revenues", icon = icon("money-bill-alt"))
        )
      ),
      bs4Dash::dashboardBody(
        bs4Dash::tabItems(
          bs4Dash::tabItem(
            tabName = "overview",
            fluidRow(
              bs4Dash::box(
                title = "Net Worth Over Time",
                width = 6,
                solidHeader = TRUE,
                status = "primary",
                plotly::plotlyOutput(ns("net_worth_plot"))
              ),
              bs4Dash::box(
                title = "Cash Flow Over Time",
                width = 6,
                solidHeader = TRUE,
                status = "primary",
                plotly::plotlyOutput(ns("cash_flow_plot"))
              )
            ),
            fluidRow(
              bs4Dash::box(
                title = "Expenses vs Revenues",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                plotly::plotlyOutput(ns("expenses_revenues_plot"))
              )
            )
          ),
          bs4Dash::tabItem(
            tabName = "expenses",
            h2("Expenses"),
            p("This is the expenses tab.")
          ),
          bs4Dash::tabItem(
            tabName = "revenues",
            h2("Revenues"),
            p("This is the revenues tab.")
          )
        )
      ),
      controlbar = bs4Dash::dashboardControlbar(
        bs4Dash::controlbarMenu(
          bs4Dash::controlbarItem(
            title = "Settings",
            icon = icon("cogs"),
            p("This is the settings section.")
          ),
          bs4Dash::controlbarItem(
            title = "Help",
            icon = icon("question-circle"),
            p("This is the help section.")
          )
        )
      ),
      footer = bs4Dash::dashboardFooter(
        p("CashieR Personal Finance Manager © 2024")
      )
    )
  )
}

#' dashboard Server Functions
#'
#' @noRd
mod_dashboard_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Mock data for the net worth plot
    net_worth_data <- data.frame(
      date = seq(as.Date("2024-01-01"), by = "month", length.out = 12),
      net_worth = cumsum(rnorm(12, mean = 1000, sd = 200))
    )

    # Mock data for the cash flow plot
    cash_flow_data <- data.frame(
      date = seq(as.Date("2024-01-01"), by = "month", length.out = 12),
      cash_inflow = cumsum(rnorm(12, mean = 2000, sd = 500)),
      cash_outflow = cumsum(rnorm(12, mean = 1500, sd = 400))
    )
    cash_flow_data$net_cash_flow <- cash_flow_data$cash_inflow - cash_flow_data$cash_outflow

    # Mock data for the expenses vs revenues plot
    expenses_revenues_data <- data.frame(
      month = factor(month.abb, levels = month.abb),
      expenses = rnorm(12, mean = 1500, sd = 300),
      revenues = rnorm(12, mean = 2000, sd = 400)
    )

    # Render the net worth plot
    output$net_worth_plot <- plotly::renderPlotly({
      plotly::plot_ly(
        net_worth_data, x = ~date, y = ~net_worth, type = 'scatter', mode = 'lines+markers',
        line = list(color = 'blue'), marker = list(color = 'red')
      ) |>
        plotly::layout(
          xaxis = list(title = "Date"),
          yaxis = list(title = "Net Worth ($)"),
          hovermode = "x unified"
        )
    })

    # Render the cash flow plot with Plotly
    output$cash_flow_plot <- plotly::renderPlotly({
      plotly::plot_ly(cash_flow_data, x = ~date) |>
        plotly::add_trace(y = ~cash_inflow, type = 'scatter', mode = 'lines', name = 'Cash Inflow',
                  fill = 'tozeroy', line = list(color = 'green')) |>
        plotly::add_trace(y = ~cash_outflow, type = 'scatter', mode = 'lines', name = 'Cash Outflow',
                  fill = 'tozeroy', line = list(color = 'red')) |>
        plotly::layout(
               xaxis = list(title = "Date"),
               yaxis = list(title = "Amount ($)"),
               hovermode = "x unified",
               legend = list(x = 0, y = 1))
    })

    # Render the expenses vs revenues plot with Plotly
    output$expenses_revenues_plot <- plotly::renderPlotly({
      plotly::plot_ly(expenses_revenues_data, x = ~month)  |>
        plotly::add_trace(y = ~expenses, type = 'bar', name = 'Expenses',
                  marker = list(color = 'red')) |>
        plotly::add_trace(y = ~revenues, type = 'bar', name = 'Revenues',
                  marker = list(color = 'green')) |>
        plotly::layout(
               xaxis = list(title = "Month"),
               yaxis = list(title = "Amount ($)"),
               barmode = 'group',
               hovermode = "x unified",
               legend = list(x = 0, y = 1))
    })
  })
}

## To be copied in the UI
# mod_dashboard_ui("dashboard_1")

## To be copied in the server
# mod_dashboard_server("dashboard_1")
