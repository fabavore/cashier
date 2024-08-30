#' FinanceData Class for Managing Financial Data Tables
#'
#' The `FinanceData` class is designed to manage individual financial data tables (e.g., accounts, postings, rules)
#' in a SQLite database. This class provides methods to append, update, and retrieve data from these tables.
FinanceData <- R6::R6Class(
  "FinanceData",

  public = list(
    #' @field data Reference to the table in the database.
    data = NULL,

    #' @description
    #' Initialize the `FinanceData` object by connecting to a specific table in the database.
    #'
    #' @param src The source database connection (e.g., a DBI connection object).
    #' @param from The name of the table in the database that this object will manage.
    #'
    #' @importFrom dplyr tbl
    initialize = function(src, from) {
      self$data <- tbl(src, from)
    },

    #' @description
    #' Append new rows to the database table managed by the `FinanceData` object.
    #'
    #' @param y A tibble containing the rows to append.
    #' @param ... Additional arguments passed to `dplyr::rows_append`.
    #'
    #' @details
    #' The function converts any columns that end with "date" to character format to ensure proper insertion into the database.
    #' It then appends the rows to the table in-place.
    #'
    #' @importFrom dplyr mutate across ends_with rows_append
    rows_append = function(y, ...) {
      y <- y |>
        mutate(across(ends_with("date"), as.character))
      self$data |>
        rows_append(y, ..., copy = TRUE, in_place = TRUE)
    },

    #' @description
    #' Update existing rows in the database table managed by the `FinanceData` object.
    #'
    #' @param y A tibble containing the rows to update.
    #' @param ... Additional arguments passed to `dplyr::rows_update`.
    #'
    #' @details
    #' The function converts any columns that end with "date" to character format to ensure proper insertion into the database.
    #' It then updates the rows in the table, matching rows by their `id` column.
    #' Unmatched rows are ignored, and the operation will be performed in-place.
    #'
    #' @importFrom dplyr mutate across ends_with collect rows_update
    rows_update = function(y, ...) {
      y <- y |>
        mutate(across(ends_with("date"), as.character))
      self$data |>
        rows_update(y, by = "id", ..., unmatched = "ignore", copy = TRUE, in_place = TRUE)
    },

    #' @description
    #' Retrieve all data from the table managed by the `FinanceData` object.
    #'
    #' @return A tibble containing all rows from the table with date columns as `Date` objects.
    #'
    #' @details
    #' The function collects the data from the database and converts any columns that end with "date" from character
    #' format back to `Date` objects, using the format `%Y-%m-%d`.
    #'
    #' @importFrom dplyr mutate across ends_with collect
    get_data = function() {
      self$data |>
        collect() |>
        mutate(across(ends_with("date"), ~ as.Date(.x, format = "%Y-%m-%d")))
    }
  )
)
