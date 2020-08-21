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
#'   '[1, 2]'
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
#' @section SQL 2016:
#' Equivalent of `json_exists(<json>, <path>)`.
#'
#' Tests whether a specific path exists in JSON document. It evaluates to true, false or unknown.
#'
#' @return A logical vector.
#' @export
#'
#' @examples
#' json_path_exists(
#'   c(
#'     '{"a": 1}',
#'     '{"b": 2}',
#'     '[1, 2]',
#'     NA_character_
#'   ),
#'   "$.a"
#' )
json_path_exists <- function(x, path) {
  df <- exec_sqlite_json(
    x,
    glue_sql("
      SELECT
        JSON_TYPE(my_tbl.data, {path}) AS result
      FROM my_tbl
    ", .con = con
    )
  )

  ifelse(
    is.na(x),
    NA,
    !is.na(df$result)
  )
}

json_keys_exist <- function(x, keys) {
  paths <- json_path(keys)
  exists_clauses <- glue_sql(
    "JSON_TYPE(my_tbl.data, {paths}) AS {`keys`}",
    .con = con
  ) %>%
    paste0(collapse = ",\n") %>%
    DBI::SQL()

  df <- exec_sqlite_json(
    x,
    glue_sql("
      SELECT
       {exists_clauses}
      FROM my_tbl
    ", .con = con
    )
  )

  tibble::as_tibble(lapply(df, function(y) !is.na(y)))
}

json_paths_exist <- function(x, paths) {
  exists_clauses <- glue_sql(
    "JSON_TYPE(my_tbl.data, {paths}) AS {`paths`}",
    .con = con
  ) %>%
    paste0(collapse = ",\n") %>%
    DBI::SQL()

  df <- exec_sqlite_json(
    x,
    glue_sql("
      SELECT
       {exists_clauses}
      FROM my_tbl
    ", .con = con
    )
  )

  ifelse(
    is.na(x),
    NA,
    !is.na(df$result)
  )
}
