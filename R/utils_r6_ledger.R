#' Ledger Class for Managing Finance Data
#'
#' The `Ledger` class is designed to manage financial transactions, accounts, and rules
#' using a SQLite database. This class provides methods to initialize the database,
#' handle account and transaction data, and apply rules for categorizing transactions.
Ledger <- R6::R6Class(
  "Ledger",

  public = list(
    #' @field con Database connection object
    con = NULL,

    #' @field accounts Holds account data
    accounts = NULL,

    #' @field postings Holds transaction data (postings)
    postings = NULL,

    #' @field rules Holds rules for categorizing transactions
    rules = NULL,

    #' @description
    #' Initialize the `Ledger` object by connecting to a SQLite database
    #' and creating the necessary tables if they don't exist.
    #'
    #' @param db_path The path to the SQLite database file. Defaults to "ledger.sql".
    initialize = function(db_path = "~/ledger.sql") {
      self$con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

      # Create the accounts table if it doesn't exist
      DBI::dbExecute(self$con, "
        CREATE TABLE IF NOT EXISTS account (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          account_name TEXT,
          account_iban TEXT,
          opening_date TEXT,
          opening_balance REAL,
          currency TEXT,
          UNIQUE(account_iban) ON CONFLICT IGNORE
        )
      ")

      # Create the postings table if it doesn't exist
      DBI::dbExecute(self$con, "
        CREATE TABLE IF NOT EXISTS posting (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          account_id INTEGER,
          booking_date TEXT,
          value_date TEXT,
          payee_name TEXT,
          payee_iban TEXT,
          description TEXT,
          amount REAL,
          balance REAL,
          currency TEXT,
          FOREIGN KEY (account_id) REFERENCES account(id),
          UNIQUE(account_id, booking_date, value_date, payee_name, payee_iban, description, amount, currency) ON CONFLICT IGNORE
        )
      ")

      # Create the rules table if it doesn't exist
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

      # Initialize FinanceData objects for accounts, postings, and rules
      self$accounts <- FinanceData$new(self$con, from = "account")
      self$postings <- FinanceData$new(self$con, from = "posting")
      self$rules <- FinanceData$new(self$con, from = "rule")
    },

    #' @description
    #' Retrieve transactions (postings) from the ledger, joined with
    #' account information and categorized using the rules table.
    #'
    #' @return A tibble containing transactions with account information and categorized data.
    #'
    #' @details
    #' The function performs the following steps:
    #' - Joins the `postings` table with the `accounts` table to get the account names.
    #' - Left-joins with the `rules` table to categorize transactions based on `payee_name` and `description`.
    #' - Transactions that aren't matched to any rule are categorized as "Income: Unknown" or "Expenses: Unknown" based on the transaction amount.
    #' - Dates are converted from character format to `Date` objects.
    #'
    #' @importFrom dplyr left_join select distinct collect mutate across ends_with
    get_transactions = function() {
      transactions <- self$postings$data |>
        left_join(
          self$accounts$data |> select(id, account_name),
          by = c("account_id" = "id")
        ) |>
        left_like_join(
          self$rules$data,
          by = c("payee_name", "description"), suffix = c("", "_rule")
        ) |>
        distinct(id, .keep_all = TRUE) |>
        collect() |>
        mutate(across(ends_with("date"), ~ as.Date(.x, format = "%Y-%m-%d")))

      return(transactions)
    },

    #' @description Import postings from a processed CSV file into the ledger system.
    #'
    #' @param postings A tibble containing processed postings data from the CSV file.
    #'
    #' @details
    #' The function performs the following steps:
    #' - Selects relevant columns from the postings data.
    #' - Extracts account information and either adds a new account or updates an existing account in the ledger.
    #' - Appends the postings data to the `postings` table in the ledger.
    #' - Missing values in character columns are replaced with empty strings.
    #'
    #' @importFrom dplyr select filter pull mutate across where
    #' @importFrom tibble tibble
    #' @importFrom tidyr replace_na
    import_postings = function(postings) {
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
      account <- self$accounts$data |> filter(account_iban == opening$account_iban)

      account_update <- tibble(
        account_name = opening$account_name,
        account_iban = opening$account_iban,
        opening_date = opening$booking_date,
        opening_balance = opening$balance - opening$amount,
        currency = opening$currency
      )

      # Append or update account
      if (length(account |> pull(opening_date)) == 0) {
        # Add a new account
        self$accounts$rows_append(account_update)
      } else if (account_update$opening_date < account |> pull(opening_date)) {
        # Update the values of the existing account entry
        self$accounts$rows_update(account_update |> mutate(id = account |> pull(id)))
      }

      # Append postings
      postings <- postings |>
        mutate(account_id = account |> pull(id)) |>
        select(-account_name, -account_iban) |>
        mutate(across(where(is.character), ~ replace_na(.x, "")))

      self$postings$rows_append(postings)
    },

    #' @description Import rules from a CSV file into the ledger system.
    #'
    #' @param rules A tibble containing rules data from the CSV file.
    #'
    #' @details
    #' The function performs the following steps:
    #' - Selects relevant columns from the postings data.
    #' - Missing values in character columns are replaced with empty strings.
    #'
    #' @importFrom dplyr select mutate across where
    #' @importFrom tidyr replace_na
    import_rules = function(rules) {
      rules <- rules |>
        select(
          payee_name = `Counterparty Regex`,
          description = `Description Regex`,
          category = `Category`,
          tags = `Tags`
        ) |>
        mutate(across(where(is.character), ~ replace_na(.x, "")))

      self$rules$rows_append(rules)
    },

    #' @description Disconnect the database connection when the `Ledger` object is finalized.
    finalize = function() {
      DBI::dbDisconnect(self$con)
    }
  )
)
