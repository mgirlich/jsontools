check_present <- function(x) {
  arg <- ensym(x)
  if (missing(x)) {
    abort(paste0("Argument `", arg, "` is missing with no default"))
  }
}

#' Prettify/Minify a JSON vector
#'
#' @param x A JSON vector.
#'
#' @return A json2 vector
#' @export
#'
#' @examples
#' x <- c("[1,2,   3]", '{"a": 1, "b": 2}')
#' json_prettify(x)
#' json_minify(x)
json_prettify <- function(x, indent = 3) {
  json2_do(x, jsonlite::prettify, indent = indent, json2 = TRUE)
}

#' @rdname json_prettify
#' @export
json_minify <- function(x) {
  json2_do(x, jsonlite::minify, json2 = TRUE)
}

json2_do <- function(x, f, json2 = TRUE, ...) {
  x <- as.character(x)
  x_na <- is.na(x)
  x[!x_na] <- as.character(lapply(x[!x_na], f, ...))

  if (is_true(json2)) {
    new_json2(x)
  } else {
    x
  }
}

escape_paths <- function(..., collapse = FALSE) {
  paths <- DBI::dbQuoteString(con, c(...))

  if (is_true(collapse)) {
    paths <- paste0(paths, collapse = ", ")
  }

  paths
}

#' Get array length of JSON arrays
#'
#' @param x Vector with JSON.
#' @param path Path
#'
#' @return An integer vector of array lengths
#' @export
#'
#' @examples
#' json_array_length(c(NA, "[1, 2, 3]", "[1, 2]"))
json_array_length <- function(x, path = NULL) {
  # TODO parameter so that scalars have length 1 instead of zero?
  # * and warn about scalar elements?

  if (is.null(path)) {
    query <- glue_sql("JSON_ARRAY_LENGTH(data)", .con = con)
  } else {
    query <- glue_sql("JSON_ARRAY_LENGTH(data, {path})", .con = con)
  }

  array_lengths <- exec_sqlite_json(
    x,
    glue_sql("
      SELECT {query} AS result
      FROM my_tbl", .con = con)
  )$result

  as.integer(array_lengths)
}

#' Query the JSON type
#'
#' @param x A JSON vector
#'
#' @return A character vector of JSON types
#' @export
#'
#' @examples
#' json_type(c(NA, "1", "null", "[1,2]", '{"a": 1}'))
json_type <- function(x) {
  exec_sqlite_json(
    x,
    "SELECT
      CASE JSON_VALID(data)
        WHEN true THEN JSON_TYPE(data)
        ELSE TYPEOF(data)
      END AS result
    FROM my_tbl"
  )$result
}

#' Unbox a vector or data frame
#'
#' A wrapper around `jsonlite::unbox()` to avoid conflict with `rlang::unbox()`.
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


row_all <- function(x) {
  unname(apply(x, 1, all))
}

row_any <- function(x) {
  unname(apply(x, 1, any))
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

my_map_chr <- function(x, f) {
  f <- as_function(f)
  vapply(x, f, character(1))
}

my_map_int <- function(x, f) {
  f <- as_function(f)
  vapply(x, f, integer(1))
}
