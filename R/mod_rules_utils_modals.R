#' @importFrom stringr str_glue
create_rule_edit_modal <- function(ns,
                                   edit_type = c("append", "update", "delete"),
                                   choices = NULL, values = NULL) {
  modalDialog(
    title = switch(
      edit_type,
      "append" = "Add New Rule",
      "update" = "Edit Rule",
      "delete" = "Delete Rule"
    ),

    if (edit_type == "delete") {
      "Are you sure you want to delete the selected rule?"
    } else {
      tagList(
        textInput(ns("payee_name"), "Enter a regular expression to match Counterparty:", value = values$payee_name),
        textInput(ns("description"), "Enter a regular expression to match Description:", value = values$description),
        selectizeInput(
          ns("category"), "Select Category:",
          choices = choices$category, selected = values$category,
          options = list(create = TRUE)
        ),
        selectizeInput(
          ns("tags"), "Select Tags:",
          choices = choices$tags, selected = values$tags,
          multiple = TRUE, options = list(create = TRUE)
        )
      )
    },

    footer = tagList(
      modalButton("Cancel"),
      bs4Dash::actionButton(
        ns(str_glue("confirm_rule_{edit_type}")), "Confirm",
        status = ifelse(edit_type == "delete", "danger", "primary")
      )
    ),
    easyClose = TRUE
  )
}
