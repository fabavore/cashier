#' Create Import CSV Modal Dialog
#'
#' This function creates a modal dialog for uploading CSV files. The modal includes a file input for selecting
#' a CSV file and buttons for importing or canceling the operation.
#'
#' @param ns A namespace function for module compatibility. This is used to prefix the IDs of the elements in the modal.
#' @return A `modalDialog` object that can be used in the UI definition.
#'
#' @noRd
create_import_modal <- function(ns) {
  modalDialog(
    title = "Import CSV File",
    size = "s",
    footer = tagList(
      modalButton("Cancel"),
      bs4Dash::actionButton(ns("import"), "Import", status = "primary")
    ),
    fluidRow(
      column(width = 12,
             fileInput(ns("csv_file"), "Choose CSV File",
                       accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
      )
    ),
    easyClose = TRUE
  )
}

#' Process Financial Transactions from CSV File
#'
#' @description
#' This function reads financial transaction data from a CSV file, processes it, and returns a clean data frame. The CSV file is expected to use `;` as a separator, `,` as a decimal mark, and dates in `dd.mm.yyyy` format. The function renames columns to shorter, intuitive names, and converts dates and amounts into appropriate formats.
#'
#' @param file_path A string specifying the file path to the CSV file containing the transaction data.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{account_iban}: The user's IBAN.
#'   \item \code{booking_date}: The transaction booking date (as a Date object).
#'   \item \code{value_date}: The date the transaction is effective (as a Date object).
#'   \item \code{payee_name}: The name of the counterparty.
#'   \item \code{payee_iban}: The counterparty's IBAN.
#'   \item \code{description}: The purpose of the transaction (e.g., Rent payment, Groceries).
#'   \item \code{amount}: The transaction amount (positive for inflows, negative for outflows).
#'   \item \code{currency}: The currency of the transaction (e.g., EUR).
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
#' @importFrom readr read_delim cols col_character col_date col_double locale
#' @importFrom dplyr select mutate across where
#' @importFrom tidyr replace_na
#' @export
process_posting_csv <- function(file_path) {
  transactions <- read_delim(
    file_path,
    delim = ";",
    col_types = cols(
      .default = col_character(),
      Buchungstag = col_date(),
      Valutadatum = col_date(),
      Betrag = col_double()
    ),
    locale = locale(
      date_format = "%d.%m.%Y",
      decimal_mark = ",",
      grouping_mark = "."
    )
  ) |>
    select(
      account_iban = `IBAN Auftragskonto`,
      booking_date = `Buchungstag`,
      value_date = `Valutadatum`,
      payee_name = `Name Zahlungsbeteiligter`,
      payee_iban = `IBAN Zahlungsbeteiligter`,
      description = `Verwendungszweck`,
      amount = `Betrag`,
      currency = `Waehrung`
    ) |>
    mutate(across(where(is.character), ~ replace_na(.x, "")))

  return(transactions)
}

process_rule_csv <- function(file_path) {
  rules <- read_csv2(
    file_path,
    col_types = cols(
      .default = col_character()
    )
  ) |>
    select(
      payee_name = `Counterparty Regex`,
      description = `Description Regex`,
      category = `Category`,
      tags = `Tags`
    ) |>
    mutate(across(where(is.character), ~ replace_na(.x, "")))

  return(rules)
}
