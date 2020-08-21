#' Extracts a value from JSON
#'
#' * extracts a scalar JSON value at the given path.
#' * errors if value at path is in object or an array
#' * tries to simplify
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
#' @section SQL 2016 - `json_value()`:
#' `json_value(<json>, <path> [returning <type>])`
#'
#' JSON_VALUE is an operator to extract an SQL scalar from a JSON value.
#' * it can only extract scalars
#' * it errors when the value at the path is an array or an object
#'
#' Arguments
#' * RETURNING: specifies the return type <-> ptype
#' * ON EMPTY: specifies what to do if the path expression is empty <-> default
#'   * triggers if
#'     * value is NULL
#'     * (lax mode): path does not exist
#'   * NULL: return NULL
#'   * ERROR: raise exception
#'   * DEFAULT \<expression\>: evaluate expression
#' * ON ERROR: Unhandled errors can arise if there is -> no equivalent
#'    * an input conversion error (for example, if the context item cannot be parsed),
#'    * an error returned by the SQL/JSON path engine,
#'    * or an output conversion error.
#'   * choices as in `ON EMPTY`
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
    abort("`path` must be a character vector of length 1")
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

#' Extract a JSON value
#'
#' @param wrap_scalars Should scalars be wrapped?
#'
#' @return A `json2` vector.
#'
#' @section SQL 2016 - `json_query()`:
#' `json_query(<json>, <path>, ...)`
#'
#'  JSON_QUERY is an operator to extract SQL/JSON values from a JSON value.
#'  * it can extract any JSON type;
#'  * it always returns a string;
#'  * it can extract multiple elements from a JSON document.
#'
#'  Arguments
#'  * WRAPPER:
#'    * WITHOUT ARRAY:
#'    * WITH CONDITIONAL ARRAY: -> ON EMPTY disallowed
#'    * WITH UNCONDITIONAL ARRAY: -> ON EMPTY disallowed
#'  * QUOTES
#'    * KEEP:
#'    * OMIT
#'  * ON EMPTY:
#'    * ERROR
#'    * NULL
#'    * EMPTY ARRAY
#'    * EMPTY OBJECT
#'  * ON ERROR: same choices as in ON EMPTY
#' @rdname json_get_value
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

  x_result <- replace_na(x_result, is.na(x), na)

  path_not_found <- is.na(result$type) & !is.na(x)
  replace_not_found(x_result, path_not_found, default)
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
