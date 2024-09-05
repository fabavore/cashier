#' dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bs4Dash sidebarMenu menuItem tabItems tabItem bs4Card box boxSidebar descriptionBlock
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
          menuItem("Accounts", tabName = "accounts", icon = icon("bank")),
          menuItem("Transactions", tabName = "transactions", icon = icon("money-bill-transfer")),
          menuItem("Automation", tabName = "rules", icon = icon("microchip")),
          menuItem("Income", tabName = "income", icon = icon("money-bill-alt")),
          menuItem("Expenses", tabName = "expenses", icon = icon("file-invoice-dollar"))
        )
      ),
      bs4Dash::dashboardBody(
        tabItems(
          tabItem(
            tabName = "overview",
            fluidRow(
              box(
                title = "Current Net Worth",
                width = 4,
                status = "primary",
                collapsible = FALSE,
                descriptionBlock(
                  number = textOutput(ns("current_net_worth")),
                  numberColor = "primary",
                  header = "Total Net Worth",
                  rightBorder = FALSE
                )
              ),
              box(
                title = "Total Cash Flow",
                width = 4,
                status = "primary",
                collapsible = FALSE,
                fluidRow(
                  column(
                    width = 6,
                    descriptionBlock(
                      number = textOutput(ns("total_income")),
                      numberColor = "success",
                      numberIcon = icon("caret-up"),
                      header = "Total Income",
                      rightBorder = TRUE
                    )
                  ),
                  column(
                    width = 6,
                    descriptionBlock(
                      number = textOutput(ns("total_expenses")),
                      numberColor = "danger",
                      numberIcon = icon("caret-down"),
                      header = "Total Expenses",
                      rightBorder = FALSE
                    )
                  )
                )
              )
            ),
            fluidRow(
              box(
                title = "Net Worth Over Time",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                sidebar = boxSidebar(
                  id = ns("net_worth_plot_settings"),
                  width = 25,
                  checkboxInput(ns("net_worth_include_balances"), "Include balances")
                ),
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
            )
          ),
          tabItem(
            tabName = "accounts",
            mod_accounts_ui(ns("accounts_1"))
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
            tabName = "income",
            mod_cash_flow_ui(ns("cash_flow_1"))
          ),
          tabItem(
            tabName = "expenses",
            mod_cash_flow_ui(ns("cash_flow_2"))
          )
        )
      ),
      controlbar = bs4Dash::dashboardControlbar(
        bs4Dash::controlbarMenu(
          bs4Dash::controlbarItem(
            title = "Settings",
            icon = icon("cogs"),
              tableOutput(ns("tbl"))

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
#' @importFrom dplyr mutate group_by summarize if_else filter pull
#' @importFrom tidyr pivot_wider
#'
#' @noRd
mod_dashboard_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    Sys.setenv("GOLEM_CONFIG_ACTIVE" = "dev")

    ledger <- Ledger$new(db_path = get_golem_config("db_path"))

    gargoyle::init("accounts")
    gargoyle::init("postings")
    gargoyle::init("rules")

    mod_accounts_server("accounts_1", ledger)

    # Call the transactions module server
    mod_transactions_server("transactions_1", ledger)

    # Call the rules module server
    mod_rules_server("rules_1", ledger)

    transactions <- reactive({
      gargoyle::watch("postings")
      gargoyle::watch("rules")
      ledger$get_transactions()
    })

    mod_cash_flow_server("cash_flow_1", transactions, type = "Income")
    mod_cash_flow_server("cash_flow_2", transactions, type = "Expenses")

    net_worth <- reactive({
      data <- transactions()
      req(nrow(data) > 0)

      calculate_net_worth(data)
    })

    output$current_net_worth <- renderText({
      net_worth() |>
        filter(account_name == "Net Worth") |>
        tail(1) |>
        pull(balance) |>
        paste("€")
    })

    # Generate the Net Worth chart using Plotly
    output$net_worth_plot <- renderPlotly({
      data <- net_worth()

      if (input$net_worth_include_balances == FALSE) {
        data <- data |> filter(account_name == "Net Worth")
      }

      data |> plot_net_worth()
    })

    # Generate the Cash Flow chart using Plotly
    output$cash_flow_plot <- renderPlotly({
      data <- transactions()
      req(nrow(data) > 0)

      data |> plot_cash_flow()
    })
  })
}

## To be copied in the UI
# mod_dashboard_ui("dashboard_1")

## To be copied in the server
# mod_dashboard_server("dashboard_1")
