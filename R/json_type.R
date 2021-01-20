#' Query the JSON type
#'
#' The JSON types are
#' * null
#' * true, false
#' * integer
#' * real
#' * array
#' * object
#'
#' @param x A JSON vector
#' @inheritParams json_extract
#'
#' @return A character vector of JSON types
#' @export
#'
#' @examples
#' json_type(c(NA, "1", "null", "[1,2]", '{"a": 1}'))
json_type <- function(x, path = NULL) {
  write_json_tbl(x)

  if (is_null(path)) {
    exec_sqlite_json(
      "SELECT
        CASE JSON_VALID(data)
          WHEN true THEN JSON_TYPE(data)
          ELSE TYPEOF(data)
        END AS result
      FROM my_tbl"
    )$result
  } else {
    exec_sqlite_json(
      glue_sql(
        "SELECT JSON_TYPE(data, {path}) AS result
      FROM my_tbl",
        .con = con
      )
    )$result
  }
}
