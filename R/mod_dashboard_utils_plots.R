#' Plot Net Worth by Day and by Month
#'
#' @description
#' This function creates a combined line chart of net worth over time, showing both daily net worth and monthly aggregated net worth. The chart is created using the `plotly` package, with separate traces for daily and monthly data.
#'
#' @param net_worth A data frame containing the net worth data. The data frame should have at least two columns:
#' \itemize{
#'   \item \code{date}: The date of each net worth calculation (as a Date object).
#'   \item \code{net_worth}: The cumulative net worth at each date.
#' }
#'
#' @return A `plotly` object representing the combined net worth line chart, with both daily and monthly data.
#'
#' @details
#' The function creates an interactive line chart using `plot_ly`, displaying the trend of net worth over time. One trace shows the daily net worth, and another shows the monthly aggregated net worth. The chart is configured with titles for both axes.
#'
#' @examples
#' # Example net worth data
#' net_worth <- data.frame(
#'   date = seq.Date(from = as.Date("2024-01-01"), to = as.Date("2024-12-31"), by = "day"),
#'   net_worth = cumsum(rnorm(365, mean = 0, sd = 100))
#' )
#'
#' @importFrom dplyr mutate group_by summarise first last bind_rows rename
#' @importFrom tidyr complete fill replace_na
#' @importFrom zoo as.yearmon
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
    summarise(balance = sum(balance), .groups = "drop_last") |>
    mutate(account_name = "Net Worth")

  # Calculate net worth by month
  net_worth_monthly <- net_worth |>
    mutate(date = zoo::as.yearmon(date) |> zoo::as.Date(frac = 1)) |>
    group_by(date) |>
    summarise(balance = last(balance)) |>
    mutate(account_name = "Net Worth Monthly")

  # Create the plot
  fig <- bind_rows(balance, net_worth, net_worth_monthly) |>
    plot_ly(x = ~date, y = ~balance, color = ~account_name, type = 'scatter', mode = 'lines') |>
    layout(title = 'Balance Over Time',
           xaxis = list(title = 'Date'),
           yaxis = list(title = 'Balance'),
           legend = list(title = list(text = 'Account')))

  return(fig)
}

