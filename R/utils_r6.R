FinanceData <- R6::R6Class(
  "FinanceData",

  public = list(
    data = NULL,

    initialize = function(src, from) {
      self$data <- tbl(src, from)
    }
  )
)

Ledger <- R6::R6Class(
  "Ledger",

  public = list(
    con = NULL,

    postings = NULL,

    rules = NULL,

    initialize = function(db_path = "ledger.db") {
      self$con <- dbConnect(SQLite(), db_path)

      # Create the posting table if it doesn't exist
      dbExecute(self$con, "
        CREATE TABLE IF NOT EXISTS posting (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          account_iban TEXT,
          booking_date TEXT,
          value_date TEXT,
          payee_name TEXT,
          payee_iban TEXT,
          purpose TEXT,
          amount REAL,
          currency TEXT,
          UNIQUE(account_iban, booking_date, value_date, payee_name, payee_iban, purpose, amount, currency) ON CONFLICT IGNORE
        )
      ")

      # Create the rule table if it doesn't exist
      dbExecute(self$con, "
        CREATE TABLE IF NOT EXISTS rule (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          payee_name TEXT,
          payee_iban TEXT,
          purpose TEXT,
          category TEXT,
          tags TEXT,
          UNIQUE(payee_name, payee_iban, purpose, category, tags) ON CONFLICT IGNORE
        )
      ")

      self$postings <- FinanceData$new(self$con, from = "posting")

      self$rules <- FinanceData$new(self$con, from = "rule")
    },

    finalize = function() {
      dbDisconnect(self$con)
    }
  )
)
