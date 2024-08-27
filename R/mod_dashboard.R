#' dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bs4Dash sidebarMenu menuItem tabItems tabItem
mod_dashboard_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::dashboardPage(
      bs4Dash::dashboardHeader(
        title = bs4Dash::dashboardBrand("CashieR Finance Manager")
      ),
      bs4Dash::dashboardSidebar(
        sidebarMenu(
          menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
          menuItem("Transactions", tabName = "transactions", icon = icon("money-bill-transfer"), selected = T),
          menuItem("Automation", tabName = "rules", icon = icon("microchip")),
          menuItem("Expenses", tabName = "expenses", icon = icon("file-invoice-dollar")),
          menuItem("Revenues", tabName = "revenues", icon = icon("money-bill-alt"))
        )
      ),
      bs4Dash::dashboardBody(
        tabItems(
          tabItem(
            tabName = "overview",
            fluidRow(
              bs4Dash::box(
                title = "Net Worth Over Time",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                plotly::plotlyOutput(ns("net_worth_plot"))
              )
            ),
            fluidRow(
              bs4Dash::box(
                title = "Cash Flow Over Time",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                plotly::plotlyOutput(ns("cash_flow_plot"))
              )
            ),
            fluidRow(
              bs4Dash::box(
                title = "Expenses vs Income",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                plotly::plotlyOutput(ns("expenses_vs_income_plot"))
              )
            )
          ),
          tabItem(
            tabName = "transactions",

            # Include the transactions module UI here
            mod_transactions_ui(ns("transactions_1"))
          ),
          tabItem(
            tabName = "rules",

            # Include the rules module UI
            mod_rules_ui(ns("rules_1"))
          ),
          tabItem(
            tabName = "expenses",
            h2("Expenses"),
            p("This is the expenses tab.")
          ),
          tabItem(
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
#' @importFrom plotly renderPlotly plot_ly layout add_trace
#' @importFrom dplyr mutate group_by summarize if_else
#' @importFrom tidyr pivot_wider
#'
#' @noRd
mod_dashboard_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ledger <- Ledger$new("ledger.db")

    gargoyle::init("postings")
    gargoyle::init("rules")

    # Call the transactions module server
    mod_transactions_server("transactions_1", ledger)

    # Call the rules module server
    mod_rules_server("rules_1", ledger)

    # Generate the Net Worth chart using Plotly
    output$net_worth_plot <- renderPlotly({
      gargoyle::watch("postings")
      data <- ledger$postings$get_data()
      req(nrow(data) > 0)

      data |> calculate_net_worth() |> plot_net_worth()
    })

    # Generate the Cash Flow chart using Plotly
    output$cash_flow_plot <- renderPlotly({
      gargoyle::watch("postings")
      data <- ledger$postings$get_data()
      req(nrow(data) > 0)

      cash_flow <- data |>
        mutate(
          Month = zoo::as.yearmon(booking_date) |> zoo::as.Date(frac = 1),
          Type = ifelse(amount >= 0, "Income", "Expense")
        ) |>
        group_by(Month, Type) |>
        summarize(Cash_Flow = sum(amount, na.rm = TRUE), .groups = 'drop') |>
        pivot_wider(names_from = Type, values_from = Cash_Flow, values_fill = list(Cash_Flow = 0))

      plot_ly(cash_flow, x = ~Month) |>
        add_trace(y = ~Income, name = 'Income', type = 'scatter', mode = 'lines+markers', fill = 'tozeroy', line = list(color = 'green')) |>
        add_trace(y = ~-Expense, name = 'Expense', type = 'scatter', mode = 'lines+markers', fill = 'tozeroy', line = list(color = 'red')) |>
        layout(
          title = "Cash Flow Over Time",
          xaxis = list(title = "Month"),
          yaxis = list(title = "Cash Flow"),
          legend = list(title = list(text = "Type")),
          hovermode = "x"
        )
    })

    # Generate the Expenses vs Revenues chart using Plotly
    output$expenses_vs_income_plot <- renderPlotly({
      gargoyle::watch("postings")
      data <- ledger$postings$get_data()
      req(nrow(data) > 0)

      cash_flow <- data |>
        mutate(
          Month = zoo::as.yearmon(booking_date) |> zoo::as.Date(frac = 1),
          Type = ifelse(amount >= 0, "Income", "Expense")
        ) |>
        group_by(Month, Type) |>
        summarize(Cash_Flow = sum(amount, na.rm = TRUE), .groups = 'drop') |>
        pivot_wider(names_from = Type, values_from = Cash_Flow, values_fill = list(Cash_Flow = 0))

      plotly::plot_ly(cash_flow, x = ~Month)  |>
        plotly::add_trace(y = ~-Expense, type = 'bar', name = 'Expenses',
                          marker = list(color = 'red')) |>
        plotly::add_trace(y = ~Income, type = 'bar', name = 'Income',
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
