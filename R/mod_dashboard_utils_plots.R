#' Plot Net Worth Over Time
#'
#' This function generates a line plot of net worth over time using Plotly.
#' Optionally, it can include balances of individual accounts in the plot.
#'
#' @param net_worth A data frame containing the net worth data, with columns `date`, `balance`, and `account_name`.
#' @param include_balances Logical, indicating whether to include the balances of individual accounts in the plot.
#' Default is `FALSE`.
#'
#' @return A Plotly object representing the net worth over time plot.
#'
#' @details The function uses Plotly to create an interactive line plot of net worth.
#' The plot displays the net worth balance over time with formatted hover information showing the balance in Euros.
#'
#' @examples
#' # Assuming you have a net_worth data frame
#' # plot_net_worth(net_worth)
#'
#' @importFrom plotly plot_ly layout style
#'
#' @noRd
plot_net_worth <- function(net_worth, include_balances = FALSE) {
  net_worth |>
    plot_ly(
      x = ~date, y = ~balance, color = ~account_name,
      type = 'scatter', mode = 'lines'
    ) |>
    layout(
      xaxis = list(title = 'Date'),
      yaxis = list(title = 'Balance (€)'),
      legend = list(title = list(text = 'Account')),
      hovermode = "x unified"
    ) |>
    style(
      hovertemplate = '%{y:,.2f} €'
    )
}

#' Plot Monthly Cash Flow
#'
#' This function generates a line chart of monthly cash flow using Plotly. It visualizes income and expenses over time, optionally excluding transactions classified as transfers. Income and expenses are plotted as separate traces with areas filled under the curves.
#'
#' @param transactions A data frame or tibble containing transaction data. The data frame must include at least the following columns:
#'   - `amount`: Numeric, the amount of the transaction. Positive values indicate income, and negative values indicate expenses.
#'   - `booking_date`: Date, the date the transaction was booked.
#'   - `category`: Character, the category of the transaction (optional, used to filter out transfers).
#' @param exclude_transfer Logical, if `TRUE` (default), transactions categorized as transfers (identified by `category` starting with "Transfer") will be excluded from the cash flow calculation.
#'
#' @return A Plotly plot object visualizing the monthly cash flow with income and expenses as separate traces.
#'
#' @importFrom dplyr mutate filter group_by summarise
#' @importFrom zoo as.yearmon as.Date
#' @importFrom plotly plot_ly add_trace layout config style
#' @importFrom stringr str_starts
#' @importFrom tidyr replace_na pivot_wider
#' @examples
#' # Example usage:
#' transactions <- data.frame(
#'   booking_date = seq.Date(from = as.Date("2023-01-01"), to = as.Date("2023-12-31"), by = "month"),
#'   amount = c(1000, -500, 1200, -600, 1300, -700, 1100, -400, 1250, -800, 1350, -900),
#'   category = c("Income", "Expense", "Income", "Transfer", "Income", "Expense", "Income", "Transfer", "Income", "Expense", "Income", "Expense")
#' )
#' plot_cash_flow(transactions)
#'
#' @noRd
plot_cash_flow <- function(transactions, exclude_transfer = TRUE) {
  if (exclude_transfer) {
    transactions <- transactions |>
      replace_na(list(category = "")) |>
      filter(category |> str_starts("Transfer", negate = TRUE))
  }

  cash_flow <- transactions |>
    mutate(
      month = zoo::as.yearmon(booking_date) |> zoo::as.Date(frac = 0),
      type = if_else(amount >= 0, "Income", "Expenses")
    ) |>
    group_by(month, type) |>
    summarise(cash_flow = sum(amount, na.rm = TRUE), .groups = 'drop') |>
    pivot_wider(
      names_from = type, values_from = cash_flow, values_fill = list(cash_flow = 0)
    )

  fig <- plot_ly(cash_flow, x = ~ month) |>
    add_trace(
      y = ~ Income, name = 'Income',
      type = 'scatter', mode = 'lines+markers',
      fill = 'tozeroy', line = list(color = 'green')
    ) |>
    add_trace(
      y = ~ -Expenses, name = 'Expenses',
      type = 'scatter', mode = 'lines+markers',
      fill = 'tozeroy', line = list(color = 'red')
    ) |>
    layout(
      xaxis = list(title = "Month"),
      yaxis = list(title = "Income and Expenses (€)"),
      legend = list(title = list(text = "Type")),
      hovermode = "x unified"
    ) |>
    style(
      hovertemplate = "%{y:,.2f} €"
    )

  return(fig)
}

