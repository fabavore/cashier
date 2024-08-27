#' Create Import CSV Modal Dialog
#'
#' This function creates a modal dialog for uploading CSV files. The modal includes a file input for selecting
#' a CSV file and buttons for importing or canceling the operation.
#'
#' @param ns A namespace function for module compatibility. This is used to prefix the IDs of the elements in the modal.
#' @return A `modalDialog` object that can be used in the UI definition.
#'
#' @noRd
create_import_modal <- function(ns) {
  modalDialog(
    title = "Import CSV File",
    size = "s",
    footer = tagList(
      modalButton("Cancel"),
      bs4Dash::actionButton(ns("import"), "Import", status = "primary")
    ),
    fluidRow(
      column(width = 12,
             fileInput(ns("transaction_file"), "Choose CSV File",
                       accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
      )
    ),
    easyClose = TRUE
  )
}
