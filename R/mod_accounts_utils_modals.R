create_edit_account_modal <- function(ns, values) {
  modalDialog(
    title = "Edit Account",

    disabled(textInput(ns("account_iban"), "Account IBAN:", value = values$account_iban)),
    textInput(ns("account_name"), "Account Name:", value = values$account_name),
    dateInput(ns("opening_date"), "Opening Date:", value = values$opening_date),
    numericInput(ns("opening_amount"), "Opening Amount:", value = values$opening_amount),
    selectizeInput(ns("currency"), "Currency:", choices = list("EUR")),

    footer = tagList(
      modalButton("Cancel"),
      bs4Dash::actionButton(ns("confirm_edit"), "Confirm", status = "primary")
    ),
    easyClose = TRUE
  )
}
