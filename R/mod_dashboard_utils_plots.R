#' Plot Net Worth Over Time
#'
#' @description
#' This function creates a `plotly` line plot that visualizes the net worth over time for multiple accounts. It handles missing balances by filling them down and calculating the total net worth by summing balances across accounts.
#'
#' @param transactions A data frame or tibble containing transaction data. The data frame must include at least the following columns:
#'   - `account_name`: Character, the name of the account.
#'   - `booking_date`: Date, the date the transaction was booked.
#'   - `balance`: Numeric, the balance of the account at the time of the transaction.
#'
#' @return A `plotly` plot object that displays the balances of individual accounts as well as the total net worth over time.
#'
#' @examples
#' # Example usage:
#' transactions <- data.frame(
#'   account_name = rep(c("Account A", "Account B"), each = 10),
#'   booking_date = rep(seq.Date(as.Date("2023-01-01"), by = "month", length.out = 10), 2),
#'   balance = c(1000, 1050, 1100, 1200, 1250, 1300, 1400, 1450, 1500, 1600,
#'               500, 550, 600, 650, 700, 750, 800, 850, 900, 950)
#' )
#' plot_net_worth(transactions)
#'
#' @importFrom dplyr mutate group_by summarise first bind_rows rename
#' @importFrom tidyr complete fill replace_na
#' @importFrom plotly plot_ly add_lines config layout
#' @export
plot_net_worth <- function(transactions) {
  # Prepare data
  balance <- transactions |>
    rename(date = booking_date) |>
    group_by(account_name, date) |>
    summarise(balance = first(balance), .groups = "drop") |>
    complete(account_name, date = seq(min(date), max(date), by = "day")) |>
    group_by(account_name) |>
    fill(balance, .direction = "down") |>
    replace_na(list(balance = 0))

  # Calculate net worth
  net_worth <- balance |>
    group_by(date) |>
    summarise(balance = sum(balance), .groups = "drop") |>
    mutate(account_name = "Net Worth")

  # Create the plot
  fig <- bind_rows(balance, net_worth) |>
    plot_ly(
      x = ~date, y = ~balance, color = ~account_name,
      type = 'scatter', mode = 'lines'
    ) |>
    layout(
      xaxis = list(title = 'Date'),
      yaxis = list(title = 'Balance (€)'),
      legend = list(title = list(text = 'Account'))
    )

  return(fig)
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
#' @importFrom plotly plot_ly add_trace layout config
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
#' @export
plot_cash_flow <- function(transactions, exclude_transfer = TRUE) {
  if (exclude_transfer) {
    transactions <- transactions |>
      replace_na(list(category = "")) |>
      filter(category |> str_starts("Transfer", negate = TRUE))
  }

  cash_flow <- transactions |>
    mutate(
      month = zoo::as.yearmon(booking_date) |> zoo::as.Date(frac = 0),
      type = if_else(amount >= 0, "Income", "Expense")
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
      y = ~ -Expense, name = 'Expense',
      type = 'scatter', mode = 'lines+markers',
      fill = 'tozeroy', line = list(color = 'red')
    ) |>
    layout(
      xaxis = list(title = "Month"),
      yaxis = list(title = "Income and Expenses (€)"),
      legend = list(title = list(text = "Type")),
      hovermode = "x"
    )

  return(fig)
}

