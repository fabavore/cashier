#' Perform a SQL-like Join Using `LIKE` Operator
#'
#' This function performs a left join between two data frames or tibbles using
#' the SQL `LIKE` operator for partial string matching. It is useful when you
#' need to match columns based on patterns rather than exact values.
#'
#' @param x A data frame or tibble. The left-hand side of the join.
#' @param y A data frame or tibble. The right-hand side of the join.
#' @param by A character vector specifying the columns to join by. If unnamed,
#'   the names of the columns in `x` and `y` are assumed to be the same. If
#'   named, `by` should specify the matching columns between `x` and `y`.
#' @param ... Additional arguments passed on to `dplyr::left_join()`.
#'
#' @return A data frame resulting from the left join of `x` and `y` with
#'   partial string matching using the `LIKE` operator.
#'
#' @examples
#' # Example usage
#' df1 <- data.frame(id = c("123", "456"), value = c("A", "B"))
#' df2 <- data.frame(id = c("12%", "45%"), description = c("Match1", "Match2"))
#' like_join(df1, df2, by = "id")
#'
#' @importFrom dplyr left_join
#' @importFrom stringr str_glue
#' @export
left_like_join <- function(x, y, by, ...) {
  if (is.null(names(by))) {
    names(by) <- by
  }

  left_join(
    x, y, ...,
    sql_on = sapply(names(by), \(x) str_glue("LHS.{x} LIKE RHS.{by[[x]]}")) |>
      paste(collapse = " AND ")
  )
}
