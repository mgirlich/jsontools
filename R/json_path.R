#' Does the path exist?
#'
#' @inheritParams json_extract
#'
#' @return A logical vector.
#' @export
#'
#' @examples
#' json_path_exists(
#'   c(
#'     '{"a": 1}',
#'     '{"b": 2}',
#'     "[1, 2]",
#'     NA_character_
#'   ),
#'   "$.a"
#' )
json_path_exists <- function(x, path) {
  write_json_tbl(x)
  df <- exec_sqlite_json(
    glue_sql("
      SELECT
        JSON_TYPE(my_tbl.data, {path}) AS result
      FROM my_tbl
    ", .con = con)
  )

  ifelse(
    is.na(x),
    NA,
    !is.na(df$result)
  )
}

#' @noRd
#' @examples
#' json_path("a")
#' json_path("a", "b")
#' json_path(1, 2)
#' json_path("a", 1, "b", 2)
#' json_path(1, "a", 2, "b")
#' json_path(!!!list(1, "a", 2, "b"))
json_path <- function(...) {
  dots <- list2(...)
  if (any(lengths(dots) != 1)) {
    stop_jsontools("all elements must have length 1.")
  }
  dots_escaped <- lapply(dots, path_escape)
  paste0("$", paste0(dots_escaped, collapse = ""))
}

path_escape <- function(x) {
  UseMethod("path_escape")
}

#' @export
#' @method path_escape character
path_escape.character <- function(x) {
  paste0(".", x)
}

#' @export
#' @method path_escape numeric
path_escape.numeric <- function(x) {
  paste0("[", x, "]")
}
