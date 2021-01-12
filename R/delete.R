#' Delete key
#'
#' Delete the elements at the specified paths.
#'
#' @param x A JSON vector.
#' @param ... Paths to delete.
#'
#' @export
#' @examples
#' x <- c('{"a": 11, "b": {"x": 12}}', NA)
#'
#' json_delete(x, "$.a")
#' json_delete(x, "$.a", "$.b")
#' json_delete(x, "$.b.x")
#'
#' json_delete(x, "$.abc")
json_delete <- function(x, ...) {
  paths <- escape_paths(..., collapse = TRUE)

  sql <- glue("
    SELECT JSON_REMOVE(data, {paths}) AS result
      FROM my_tbl
  ")

  json2(exec_sqlite_json(x, sql)$result)
}
