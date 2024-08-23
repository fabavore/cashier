#' Calculate Net Worth from Financial Transactions
#'
#' @description
#' This function calculates the net worth over time based on financial transactions. It sums up the cumulative transaction amounts (inflows and outflows) to calculate the net worth at each transaction date.
#'
#' @param transactions A data frame containing the transaction data. The data frame should have at least two columns:
#' \itemize{
#'   \item \code{BookingDate}: The date of the transaction (as a Date object).
#'   \item \code{Amount}: The transaction amount (positive for inflows, negative for outflows).
#' }
#'
#' @return A data frame with two columns:
#' \itemize{
#'   \item \code{Date}: The date of each transaction.
#'   \item \code{NetWorth}: The cumulative net worth at each transaction date.
#' }
#'
#' @details
#' This function assumes that the `transactions` data frame is sorted by date. The net worth is calculated by cumulatively summing the `amount` column.
#'
#' @examples
#' # Example transactions data
#' transactions <- data.frame(
#'   booking_date = as.Date(c("2024-01-01", "2024-01-05", "2024-01-10")),
#'   amount = c(1000, -200, 500)
#' )
#'
#' # Calculate net worth
#' net_worth <- calculate_net_worth(transactions)
#' print(net_worth)
#'
#' @importFrom dplyr arrange group_by summarise mutate rename
#' @export
calculate_net_worth <- function(transactions) {
  net_worth <- transactions |>
    arrange(BookingDate) |>
    group_by(BookingDate) |>
    summarise(Amount = sum(Amount), .groups = "drop_last") |>
    mutate(NetWorth = cumsum(Amount)) |>
    rename(Date = BookingDate)

  return(net_worth)
}
