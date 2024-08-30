#' Import Postings into Ledger
#'
#' Imports postings from a processed CSV file into the ledger system.
#' The function updates or appends account and posting information in the ledger.
#'
#' @param ledger A list representing the ledger system, containing the `accounts` and `postings` tables.
#' @param postings A tibble containing processed postings data from the CSV file.
#'
#' @details
#' The function selects relevant columns from the postings data, extracts account information,
#' and either adds a new account or updates an existing account in the ledger. It then appends
#' the postings data to the `postings` table in the ledger.
#'
#' - If the account does not exist in the ledger, it is added as a new entry.
#' - If the account exists but has an earlier opening date, the existing account is updated.
#' - The postings are appended to the ledger's `postings` table.
#' - Missing values in character columns are replaced with empty strings.
#'
#' @importFrom dplyr select filter mutate pull across where
#' @importFrom tibble tibble
#' @importFrom tidyr replace_na
#'
#' @examples
#' \dontrun{
#' ledger <- list(
#'   accounts = your_account_table_function(),
#'   postings = your_posting_table_function()
#' )
#' postings <- process_posting_csv("path/to/csvfile.csv")
#' import_postings(ledger, postings)
#' }
import_postings <- function(postings, ledger) {
  # Select relevant columns
  postings <- postings |>
    select(
      account_name = `Bezeichnung Auftragskonto`,
      account_iban = `IBAN Auftragskonto`,
      booking_date = `Buchungstag`,
      value_date = `Valutadatum`,
      payee_name = `Name Zahlungsbeteiligter`,
      payee_iban = `IBAN Zahlungsbeteiligter`,
      description = `Verwendungszweck`,
      amount = `Betrag`,
      balance = `Saldo nach Buchung`,
      currency = `Waehrung`
    )

  # Extract account information
  opening <- postings |> tail(1)
  account <- ledger$accounts$data |> filter(account_iban == opening$account_iban)

  account_update <- tibble(
    account_name = opening$account_name,
    account_iban = opening$account_iban,
    opening_date = opening$booking_date,
    opening_amount = opening$balance - opening$amount,
    currency = opening$currency
  )

  # Append or update account
  if (length(account |> pull(opening_date)) == 0) {
    # Add a new account
    ledger$accounts$rows_append(account_update)
  } else if (account_update$opening_date < account |> pull(opening_date)) {
    # Update the values of the existing account entry
    ledger$accounts$rows_update(account_update |> mutate(id = account |> pull(id)))
  }

  # Append postings
  postings <- postings |>
    mutate(account_id = account |> pull(id)) |>
    select(-account_name, -account_iban) |>
    mutate(across(where(is.character), ~ replace_na(.x, "")))

  ledger$postings$rows_append(postings)
}
