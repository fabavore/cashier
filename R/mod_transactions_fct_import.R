#' Process Financial Transactions from CSV File
#'
#' @description
#' This function reads financial transaction data from a CSV file, processes it, and returns a clean data frame. The CSV file is expected to use `;` as a separator, `,` as a decimal mark, and dates in `dd.mm.yyyy` format. The function renames columns to shorter, intuitive names, and converts dates and amounts into appropriate formats.
#'
#' @param file_path A string specifying the file path to the CSV file containing the transaction data.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{Account_IBAN}}: The user's IBAN.
#'   \item \code{Booking_Date}: The transaction booking date (as a Date object).
#'   \item \code{Value_Date}: The date the transaction is effective (as a Date object).
#'   \item \code{Payer_Name}: The name of the counterparty.
#'   \item \code{Payer_IBAN}: The counterparty's IBAN.
#'   \item \code{Transactions_Type}: The type of transaction (e.g., Direct Debit, Transfer, Salary).
#'   \item \code{Purpose}: The purpose of the transaction (e.g., Rent payment, Groceries).
#'   \item \code{Amount}: The transaction amount (positive for inflows, negative for outflows).
#'   \item \code{Currency}: The currency of the transaction (e.g., EUR).
#' }
#'
#' @details
#' The function automatically handles locale-specific settings, such as decimal marks and date formats, using the `readr` and `dplyr` packages. It expects specific columns in the CSV file and will only import those relevant to transaction processing.
#'
#' @examples
#' \dontrun{
#' # Assuming you have a CSV file named "transactions.csv"
#' file_path <- "path/to/transactions.csv"
#' transactions <- process_csv(file_path)
#' head(transactions)
#' }
#'
#' @importFrom readr read_csv2 cols col_character col_date col_double
#' @importFrom dplyr select
#' @export
process_csv <- function(file_path) {
  transactions <- read_csv2(
    file_path,
    col_types = cols(
      .default = col_character(),
      Buchungstag = col_date(format = "%d.%m.%Y"),
      Valutadatum = col_date(format = "%d.%m.%Y"),
      Betrag = col_double()
    )
  ) |>
    select(
      Account_IBAN = `IBAN Auftragskonto`,
      Booking_Date = `Buchungstag`,
      Value_Date = `Valutadatum`,
      Payer_Name = `Name Zahlungsbeteiligter`,
      Payer_IBAN = `IBAN Zahlungsbeteiligter`,
      Transactions_Type = `Buchungstext`,
      Purpose = `Verwendungszweck`,
      Amount = `Betrag`,
      Currency = `Waehrung`
    )

  return(transactions)
}
