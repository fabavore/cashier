FinanceData <- R6::R6Class(
  "FinanceData",

  public = list(
    data = NULL,

    initialize = function(src, from) {
      self$data <- dplyr::tbl(src, from)
    },

    rows_append = function(y, ...) {
      y <- y |>
        dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.character))
      self$data |>
        dplyr::rows_append(y, ..., copy = TRUE, in_place = TRUE)
    },

    rows_update = function(y, ...) {
      y <- y |>
        dplyr::mutate(dplyr::across(dplyr::ends_with("date"), as.character))
      self$data |>
        dplyr::rows_update(y, by = "id", ..., unmatched = "ignore", copy = TRUE, in_place = TRUE, )
    },

    get_data = function() {
      self$data |>
        dplyr::collect() |>
        dplyr::mutate(dplyr::across(
          dplyr::ends_with("date"),
          ~ as.Date(.x, format = "%Y-%m-%d")
        ))
    }
  )
)

Ledger <- R6::R6Class(
  "Ledger",

  public = list(
    con = NULL,

    accounts = NULL,

    postings = NULL,

    rules = NULL,

    initialize = function(db_path = "ledger.db") {
      self$con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

      # Create the account table if it doesn't exist
      DBI::dbExecute(self$con, "
        CREATE TABLE IF NOT EXISTS account (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          account_iban TEXT,
          account_name TEXT,
          opening_date TEXT,
          opening_amount REAL,
          currency TEXT,
          UNIQUE(account_iban) ON CONFLICT IGNORE
        )
      ")

      # Create the posting table if it doesn't exist
      DBI::dbExecute(self$con, "
        CREATE TABLE IF NOT EXISTS posting (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          account_iban TEXT,
          booking_date TEXT,
          value_date TEXT,
          payee_name TEXT,
          payee_iban TEXT,
          description TEXT,
          amount REAL,
          currency TEXT,
          UNIQUE(account_iban, booking_date, value_date, payee_name, payee_iban, description, amount, currency) ON CONFLICT IGNORE
        )
      ")

      # Create the rule table if it doesn't exist
      DBI::dbExecute(self$con, "
        CREATE TABLE IF NOT EXISTS rule (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          payee_name TEXT,
          description TEXT,
          category TEXT,
          tags TEXT,
          UNIQUE(payee_name, description) ON CONFLICT IGNORE
        )
      ")

      self$accounts <- FinanceData$new(self$con, from = "account")

      self$postings <- FinanceData$new(self$con, from = "posting")

      self$rules <- FinanceData$new(self$con, from = "rule")
    },

    #' @importFrom dplyr mutate across ends_with left_join coalesce bind_rows
    #' if_else collect transmute
    get_transactions = function() {
      opening_transactions <- self$accounts$data |>
        transmute(
          account_iban, account_name,
          booking_date = opening_date,
          value_date = opening_date,
          amount = opening_amount,
          currency,
          description = "Opening",
          category = "Equity"
        ) |>
        collect() |>
        mutate(across(ends_with("date"), ~ as.Date(.x, format = "%Y-%m-%d")))

      transactions <- self$postings$data |>
        left_join(
          self$accounts$data |> select(account_iban, account_name),
          by = "account_iban"
        ) |>
        left_like_join(
          self$rules$data,
          by = c("payee_name", "description"), suffix = c("", "_rule")
        ) |>
        mutate(
          category = coalesce(
            category,
            if_else(amount > 0, "Income: Unknown", "Expenses: Unknown")
          )
        ) |>
        collect() |>
        mutate(across(ends_with("date"), ~ as.Date(.x, format = "%Y-%m-%d"))) |>
        bind_rows(opening_transactions)

      return(transactions)
    },

    finalize = function() {
      DBI::dbDisconnect(self$con)
    }
  )
)
