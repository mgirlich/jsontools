#' Remove JSON element
#'
#' Remote the elements at the specified paths from a JSON vector.
#'
#' @param x A JSON vector.
#' @param ... Paths to delete.
#'
#' @export
#' @examples
#' x <- c('{"a": 11, "b": {"x": 12}}', NA)
#'
#' json_delete(x, "$.a")
#' # remove from multiple paths at once
#' json_delete(x, "$.a", "$.b")
#' # remove at a nested path
#' json_delete(x, "$.b.x")
#'
#' # non-existing elements are just ignored
#' json_delete(x, "$.abc")
json_delete <- function(x, ...) {
  paths <- escape_paths(..., collapse = TRUE)

  sql <- glue("
    SELECT JSON_REMOVE(data, {paths}) AS result
      FROM my_tbl
  ")

  write_json_tbl(x)
  new_json2(exec_sqlite_json(sql)$result)
}

escape_paths <- function(..., collapse = FALSE) {
  paths <- DBI::dbQuoteString(con, c(...))

  if (is_true(collapse)) {
    paths <- paste0(paths, collapse = ", ")
  }

  paths
}
