create_add_account_modal <- function(ns) {
  modalDialog(
    title = "Add Account",

    selectizeInput(ns("account_iban"), "Account IBAN:", choices = NULL),
    textInput(ns("account_name"), "Account Name:"),
    dateInput(ns("opening_date"), "Opening Date:"),
    numericInput(ns("opening_amount"), "Opening Amount:", value = 0),
    selectizeInput(ns("currency"), "Currency:", choices = list("EUR")),
    footer = tagList(
      modalButton("Cancel"),
      bs4Dash::actionButton(ns("confirm_delete"), "Confirm", status = "primary")
    ),
    easyClose = TRUE
  )
}

create_edit_account_modal <- function(ns) {
  modalDialog(
    title = "Edit Account",
    footer = tagList(
      modalButton("Cancel"),
      bs4Dash::actionButton(ns("confirm_edit"), "confirm", status = "primary")
    ),
    easyClose = TRUE
  )
}

create_delete_account_modal <- function(ns) {
  modalDialog(
    title = "Delete Account",
    p("Are you sure you want to delete this account?"),
    footer = tagList(
      modalButton("Cancel"),
      bs4Dash::actionButton(ns("confirm_delete"), "Delete", icon = icon("trash"), status = "danger")
    ),
    easyClose = TRUE
  )
}
