create_edit_rule_modal <- function(ns, values) {
  modalDialog(
    title = "Edit Rule",

    textInput(ns("payee_name"), "Enter a regular expression to match Counterparty:", value = values$payee_name),
    textInput(ns("description"), "Enter a regular expression to match Description:", value = values$description),
    selectizeInput(ns("category"), "Select Category:", choices = NULL, options = list(create = TRUE)),
    selectizeInput(ns("tags"), "Select Tags:", choices = NULL, multiple = TRUE, options = list(create = TRUE)),

    footer = tagList(
      modalButton("Cancel"),
      bs4Dash::actionButton(ns("confirm_edit"), "Confirm", status = "primary")
    ),
    easyClose = TRUE
  )
}
