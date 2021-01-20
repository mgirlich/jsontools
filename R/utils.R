check_present <- function(x) {
  arg <- ensym(x)
  if (missing(x)) {
    abort(paste0("Argument `", arg, "` is missing with no default"))
  }
}

#' Query the JSON type
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
      FROM my_tbl", .con = con)
    )$result
  }
}

#' Unbox a vector or data frame
#'
#' Mark a vector of length one to not be wrapped in an array when formated as
#' `JSON`. This is only a tiny wrapper around `jsonlite::unbox()` to avoid
#' conflict with `rlang::unbox()`.
#'
#' @param x atomic vector of length 1, or data frame with 1 row.
#'
#' @return A singleton version of `x`.
#' @export
#'
#' @examples
#' format_json(list(foo = 123))
#' format_json(list(foo = json_u(123)))
json_u <- function(x) {
  jsonlite::unbox(x)
}

my_map_chr <- function(x, f) {
  f <- as_function(f)
  vapply(x, f, character(1))
}

my_map_int <- function(x, f) {
  f <- as_function(f)
  vapply(x, f, integer(1))
}
