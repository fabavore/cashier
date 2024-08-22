#' Process Transactions CSV
#'
#' This function imports a CSV file containing transaction data, selects and renames only the relevant columns,
#' and processes date and numeric fields. The function is designed to handle CSV files with specific formats
#' and delimiters commonly used for financial transaction records.
#'
#' @param file_path A character string specifying the path to the CSV file to be processed.
#' The CSV file must use a semicolon (`;`) as the delimiter and a comma (`,`) as the decimal mark.
#' The date format in the CSV should be `day.month.year` (e.g., `24.01.2024`).
#'
#' @return A tibble (data frame) with the following columns:
#' \describe{
#'   \item{account_iban}{The IBAN of the account where the transaction originated.}
#'   \item{booking_date}{The date the transaction was booked, in `Date` format (YYYY-MM-DD).}
#'   \item{value_date}{The date value of the transaction.}
#'   \item{payer_name}{The name of the payee or payer.}
#'   \item{payer_iban}{The IBAN of the payee or payer.}
#'   \item{type}{The type of the transaction, previously referred to as `Buchungstext`.}
#'   \item{purpose}{The purpose of the transaction.}
#'   \item{amount}{The amount of the transaction. Positive values indicate income, negative values indicate expenses.}
#'   \item{currency}{The currency of the transaction amount.}
#' }
#'
#' @details
#' The function reads the CSV file with the specified encoding, delimiter, and column types. It selects and renames
#' the relevant columns to shorter English names for ease of use in further processing. The `booking_date` column
#' is converted to `Date` format for accurate date handling. Only the specified columns are read from the CSV,
#' ignoring any additional columns present in the file.
#'
#' @importFrom readr read_csv2 locale cols_only col_character col_date col_double
#' @importFrom dplyr select
#' @param file_path The path to the CSV file to read.
#' @return A tibble with the processed data.
#' @examples
#' # Example usage:
#' # Assuming 'transactions.csv' is in the current working directory
#' processed_data <- process_transactions("transactions.csv")
#' head(processed_data)
#'
#' @noRd
process_transactions <- function(file_path) {
  file_path |>
    read_csv2(
      locale = locale(encoding = "latin1"),
      col_types = cols_only(
        `IBAN Auftragskonto` = col_character(),
        `Buchungstag` = col_date(format = "%d.%m.%Y"),
        `Valutadatum` = col_date(format = "%d.%m.%Y"),
        `Name Zahlungsbeteiligter` = col_character(),
        `IBAN Zahlungsbeteiligter` = col_character(),
        `Buchungstext` = col_character(),
        `Verwendungszweck` = col_character(),
        `Betrag` = col_double(),
        `Waehrung` = col_character()
      )
    ) |>
    select(
      account_iban = `IBAN Auftragskonto`,
      booking_date = `Buchungstag`,
      value_date = `Valutadatum`,
      payer_name = `Name Zahlungsbeteiligter`,
      payer_iban = `IBAN Zahlungsbeteiligter`,
      type = `Buchungstext`,
      purpose = `Verwendungszweck`,
      Amount = `Betrag`,
      currency = `Waehrung`
    )
}
