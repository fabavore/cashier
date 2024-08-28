#' Create Rule Creation Modal Dialog
#'
#' This function creates a modal dialog for creating matching rules.
#'
#' @param ns A namespace function for module compatibility. This is used to prefix the IDs of the elements in the modal.
#' @return A `modalDialog` object that can be used in the UI definition.
#'
#' @noRd
create_rule_modal <- function(ns, values, choices) {
  modalDialog(
    title = "Create Rule from Transaction",
    tagList(
      textInput(ns("payee_name"), "Regular Expression for Couterparty:", value = values$payee_name),
      textInput(ns("description"), "Regular Expression for Description:", value = values$description),
      selectizeInput(ns("category"), "Category", choices = choices$category, options = list(create = TRUE)),
      selectizeInput(ns("tags"), "Tags", choices = choices$tags, multiple = TRUE, options = list(create = TRUE))
    ),
    footer = tagList(
      modalButton("Cancel"),
      bs4Dash::actionButton(ns("create_rule"), "Confirm", status = "primary")
    ),
    size = "m",
    easyClose = TRUE
  )
}
