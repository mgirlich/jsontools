#' Get keys of JSON object resp. array
#'
#' @param x A JSON vector
#'
#' @return A list of keys.
#' @export
#'
#' @examples
#' json_keys(c(
#'   '{"a": 1, "b": 2}',
#'   '{"x": 1, "y": 2}',
#'   "[1, 2]"
#' ))
json_keys <- function(x) {
  df <- json_each(x)
  types <- json_type(x)
  df_split <- vec_split(df$key, df$row_id)

  purrr::map2(
    df_split$val,
    types,
    function(keys, type) {
      if (type == "array") {
        as.integer(keys)
      } else {
        keys
      }
    }
  )
}

#' Does the path exist?
#'
#' @param x A JSON vector.
#' @param path A JSON path.
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
