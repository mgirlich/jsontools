#' Extract a value from JSON
#'
#' Extract a scalar JSON value at a given path. To extract a JSON object
#' or array use [json_get_query()].
#'
#' @param x A JSON vector.
#' @param path Path to element to extract.
#' @param ptype Output type. If `NULL`, the default, the output type is
#' determined by computing the common type across all elements of `...`.
#' @param default Default value if path doesn't exist or element at path is
#' empty.
#' @param na Default value if element of `x` is `NA`.
#' @param bigint_as_char Convert big integers to character?
#'
#' @return A vector with class given by `.ptype` and length equal to `x`.
#'
#' @export
#' @examples
#' x1 <- '{"a": 1, "b": 2}'
#' json_get_value(x1, "$.a")
#' json_get_value(c(NA_character_, x1), "$.a")
#' json_get_value(c(NA_character_, x1), "$.a", na = 3)
#' json_get_value(x1, "$.c", default = "not there")
#' try(json_get_value(x1, "$.c"))
#'
#' json_get_query('{"a": {"b": 1}}', "$.a")
json_get_value <- function(x, path, ptype = NULL, default = NULL, na = NA, bigint_as_char = TRUE) {
  # TODO should arrays/objects be allowed and parsed?
  if (!is_string(path)) {
    stop_jsontools("`path` must be a character vector of length 1")
  }

  if (!is_scalar_atomic(na)) {
    stop_jsontools("`na` must have length 1")
  }

  if (!(is.null(default) || is_scalar_atomic(default))) {
    stop_jsontools("`default` must be NULL or have length 1")
  }

  sql <- glue_sql("
    SELECT
      CAST(JSON_EXTRACT(data, {path}) AS text) AS value,
      JSON_TYPE(data, {path}) AS type
    FROM
      my_tbl
  ", .con = con)

  result_df <- exec_sqlite_json(x, sql)

  if (any(result_df$type %in% c("object", "array"), na.rm = TRUE)) {
    stop_jsontools("`json_get_value()` can only extract scalar values.")
  }

  x_result <- json_convert_value(
    x = result_df$value,
    json_types = result_df$type,
    ptype = ptype,
    bigint_as_char = bigint_as_char
  )

  # now that the values are parsed the original NAs can be replaced
  x_result <- replace_na(x_result, is.na(x), na)

  path_not_found <- is.na(result_df$type) & !is.na(x)
  replace_not_found(x_result, path_not_found, default)
}

#' Extract an object/array from JSON
#'
#' Extract a JSON object or array at a given path. To extract a JSON value
#' use [json_get_value()].
#'
#' @inheritParams json_get_value
#' @param wrap_scalars Should scalars be wrapped?
#'
#' @return A `json2` vector.
#'
#' @export
json_get_query <- function(x, path, wrap_scalars = FALSE, default = NULL, na = NA) {
  # TODO check default and na are objects/arrays?
  if (!is_string(path)) {
    abort("`path` must be a character vector of length 1")
  }

  check_json_string(na, na = TRUE, null = FALSE, x_name = "na")
  check_json_string(default, na = TRUE, null = TRUE, x_name = "default")

  extract_sql <- glue_sql(
    "CAST(JSON_EXTRACT(data, {path}) AS text)",
    .con = con
  )

  if (is_true(wrap_scalars)) {
    value_sql <- glue_sql(
      "CASE JSON_TYPE(data, {path}) in ('array', 'object', 'null')
        WHEN true THEN {extract_sql}
        ELSE JSON_ARRAY(JSON_QUOTE({extract_sql}))
      END",
      .con = con
    )
  } else {
    value_sql <- extract_sql
  }

  sql <- glue_sql("
    SELECT
      {value_sql} AS value,
      JSON_TYPE(data, {path}) AS type
    FROM
      my_tbl
    ", .con = con)

  result <- exec_sqlite_json(x, sql)

  if (is_true(wrap_scalars)) {
    if (any(result$type == "object", na.rm = TRUE)) {
      stop_jsontools("`wrap_scalars = TRUE` doesn't work when objects are found")
    }

    result$type[!is.na(result$type)] <- "array"
  } else {
    check_query_types(result$type)
  }

  x_result <- json_convert_value(
    x = result$value,
    json_types = result$type,
    ptype = character()
  )

  # TODO replace NA earlier on?
  x_result <- replace_na(x_result, is.na(x), na)

  path_not_found <- is.na(result$type) & !is.na(x)
  new_json2(replace_not_found(x_result, path_not_found, default))
}

replace_not_found <- function(x, not_found_flag, default) {
  if (any(not_found_flag)) {
    if (is.null(default)) {
      stop_jsontools("path not found")
    }

    x[not_found_flag] <- default
  }

  x
}

replace_na <- function(x, na_flag, na) {
  if (any(na_flag)) {
    x[na_flag] <- na
  }

  x
}

check_json_string <- function(x, na, null, x_name) {
  if (is_true(null) && is.null(x)) {
    return(TRUE)
  }

  if (is_true(na) && is.na(x) && length(x) == 1) {
    return(TRUE)
  }

  if (is_string(x) && jsonlite::validate(x)) {
    return(TRUE)
  }

  msg <- "must be a valid JSON string"
  if (is_true(null)) {
    msg <- paste0(msg, " or NULL")
  }

  if (is_true(na)) {
    msg <- paste0(msg, " or NA")
  }

  stop_jsontools(paste0("`", x_name, "`", msg))
}

check_query_types <- function(types) {
  if (all(types %in% c("object", "array", "null", NA))) {
    return()
  }

  if (!any(types %in% c("object", "array"))) {
    message <- c(
      i = "All elements are scalar.",
      i = "Did you want to use `json_get_value()`?"
    )
  } else {
    bad_idx <- which(!types %in% c("object", "array", "null", NA))
    bad_idx_list <- paste0(bad_idx, collapse = ", ")
    message <- c(
      i = "use `wrap_scalars = TRUE` to wrap scalars in an array",
      x = paste0("scalar elements at positions ", bad_idx_list)
    )
  }

  message <- c("Can only extract objects and arrays.", message)
  stop_jsontools(message)
}
