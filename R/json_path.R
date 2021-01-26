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
