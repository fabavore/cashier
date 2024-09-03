#' Process Posting CSV File
#'
#' Reads a CSV file containing financial postings and processes it into a data frame.
#' The function expects specific column names and formats.
#'
#' @param file_path A string representing the path to the CSV file.
#'
#' @return A tibble containing the processed data from the CSV file.
#'
#' @details
#' This function reads a CSV file with a semicolon delimiter (`;`), a decimal mark
#' of comma (`,`), and date format `"%d.%m.%Y"`. The columns `Buchungstag` and
#' `Valutadatum` are parsed as dates, while `Betrag` and `Saldo nach Buchung`
#' are parsed as numeric values. Other columns are treated as characters by default.
#'
#' @importFrom readr read_delim cols col_character col_date col_double locale
#'
#' @examples
#' \dontrun{
#' file_path <- "path/to/your/csvfile.csv"
#' postings <- process_posting_csv(file_path)
#' }
process_posting_csv <- function(file_path) {
  read_delim(
    file_path,
    delim = ";",
    col_types = cols(
      .default = col_character(),
      Buchungstag = col_date(),
      Valutadatum = col_date(),
      Betrag = col_double(),
      `Saldo nach Buchung` = col_double()
    ),
    locale = locale(
      date_format = "%d.%m.%Y",
      decimal_mark = ","
    )
  )
}
