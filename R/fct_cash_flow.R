calculate_cash_flow <- function(transactions) {
  mutate(
    month = zoo::as.yearmon(booking_date) |> zoo::as.Date(frac = 0),
    category = category |>
      coalesce(if_else(amount > 0, "Income: Unknown", "Expenses: Unknown"))
  ) |>
    group_by(month, category) |>
    summarise(amount = sum(amount, na.rm = TRUE), .groups = 'drop')
}
