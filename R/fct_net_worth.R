#' Calculate Net Worth
#'
#' This function calculates the net worth over time for multiple accounts.
#' It fills in missing dates and balances, and then aggregates the balances to compute the total net worth for each day.
#'
#' @param transactions A data frame containing transaction data with at least two columns: `account_name` and `booking_date`.
#' @return A list containing two data frames:
#'   \describe{
#'     \item{balance}{A data frame with the balance for each account and date.}
#'     \item{net_worth}{A data frame with the aggregated net worth for each date.}
#'   }
#'
#' @importFrom dplyr mutate group_by summarise first bind_rows rename
#' @importFrom tidyr complete fill replace_na
#'
#' @noRd
calculate_net_worth <- function(transactions) {
  # Prepare data by renaming the date column and calculating daily balances for each account
  balance <- transactions |>
    rename(date = booking_date) |>
    group_by(account_name, date) |>
    summarise(balance = first(balance), .groups = "drop") |>
    complete(account_name, date = seq(min(date), max(date), by = "day")) |>
    group_by(account_name) |>
    fill(balance, .direction = "down") |>
    replace_na(list(balance = 0))

  # Calculate net worth by summing up balances across accounts for each date
  net_worth <- balance |>
    group_by(date) |>
    summarise(balance = sum(balance), .groups = "drop") |>
    mutate(account_name = "Net Worth")

  # Return the balance and net worth data frames
  bind_rows(balance, net_worth)
}
