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
#' @importFrom dplyr mutate group_by summarize last
#' @importFrom zoo as.yearmon
#' @importFrom plotly plot_ly add_lines config layout
#' @export
plot_net_worth <- function(net_worth) {
  # Calculate net worth by month
  net_worth_monthly <- net_worth |>
    mutate(month = zoo::as.yearmon(date) |> zoo::as.Date(frac = 1)) |>
    group_by(month) |>
    summarize(net_worth = last(net_worth))

  # Create the plot
  fig <- plot_ly() |>
    # Add daily net worth line
    add_lines(data = net_worth, x = ~date, y = ~net_worth, name = "Daily Net Worth") |>
    # Add monthly net worth line
    add_lines(
      data = net_worth_monthly, x = ~month, y = ~net_worth,
      name = "Monthly Net Worth", line = list(shape = "spline")
    ) |>
    config(

    ) |>
    layout(
      title = "Net Worth by Day and by Month",
      xaxis = list(title = "Date"),
      yaxis = list(title = "Net Worth (EUR)")
    )

  return(fig)
}

