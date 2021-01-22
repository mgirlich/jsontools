#' Extract an element from JSON
#'
#' Extract an element at a given path.
#'
#' @param x A JSON vector.
#' @param path Path to element. This must be a valid
#' [JSONpath](https://goessner.net/articles/JsonPath/index.html#e2) expression.
#' For example `"$.a.b[0]` extracts the `1` in `{"a": {"b": [1, 2]}}`.
#' @param ptype Output type. If `NULL`, the default, the output type is
#' determined by computing the common type across all elements. Use
#' `new_json_array()` resp. `new_json_object()` if you know every element is
#' an array resp. object. Mind that the return type will only be `json2`.
#' @param default Default value if path doesn't exist or element at path is
#' empty.
#' @param na Default value if element of `x` is `NA`.
#' @param wrap_scalars Should scalar values be wrapped?
#'   Note that scalars are only wrapped if either
#'   * `ptype` is `new_json_array()` or `json2` vector.
#'   * `ptype` is `NULL` and the elements are a mix of scalar values and arrays.
#' @param bigint_as_char Convert big integers to character?
#'
#' @return A vector with class given by `ptype` and length equal to `x`. Mind
#' that for `new_json_array()` and `new_json_object()` the return type will
#' only be `json2`.
#'
#' @export
#' @examples
#' x1 <- '{"a": 1, "b": 2}'
#' json_extract(x1, "$.a")
#' json_extract('{"a": {"b": 1}}', "$.a")
#'
#' # `NA` values stay `NA` ...
#' json_extract(c(NA_character_, x1), "$.a")
#' # ... but can return the value of `na` instead.
#' json_extract(c(NA_character_, x1), "$.a", na = 3)
#'
#' # missing paths error by default ...
#' try(json_extract(x1, "$.c"))
#' # ... but can be replaced by the value of `default` instead.
#' json_extract(x1, "$.c", default = "not there")
#'
#' # make sure to error if you don't get back an array
#' json_extract('{"a": [1]}', "$.a", ptype = new_json_array())
#' try(json_extract('{"a": {"b": 1}}', "$.a", ptype = new_json_array()))
json_extract <- function(x,
                         path,
                         ptype = NULL,
                         default = NULL,
                         na = NA,
                         wrap_scalars = FALSE,
                         bigint_as_char = TRUE) {
  if (!is_string(path)) {
    stop_jsontools("`path` must be a character vector of length 1")
  }

  na <- vec_cast(na, ptype, x_arg = "na", to_arg = "ptype")
  if (length(na) != 1) {
    stop_jsontools("`na` must have length 1")
  }

  default <- vec_cast(default, ptype, x_arg = "default", to_arg = "ptype")
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

  write_json_tbl(x)
  result_df <- exec_sqlite_json(sql)

  x_result <- json_convert_value(
    x = result_df$value,
    json_types = result_df$type,
    ptype = ptype,
    bigint_as_char = bigint_as_char,
    wrap_scalars = wrap_scalars
  )

  # now that the values are parsed the original NAs can be replaced
  x_result <- replace_na(x_result, is.na(x), na)

  path_not_found <- is.na(result_df$type) & !is.na(x)
  replace_not_found(x_result, path_not_found, default)
}

replace_not_found <- function(x, not_found_flag, default) {
  if (any(not_found_flag)) {
    if (is_null(default)) {
      msg <- c(
        x = "`path` does not always exist.",
        i = "Did you provide an incorrect path?",
        i = "With `default` you can specify a default value for missing elements."
      )
      stop_jsontools(msg)
    }
  }

  replace_helper(x, not_found_flag, default, value_arg = "default")
}

replace_na <- function(x, na_flag, na) {
  replace_helper(x, na_flag, na, value_arg = "na")
}

replace_helper <- function(x, where, value, x_arg = "", value_arg = "") {
  if (all(!where)) {
    return(x)
  }

  if (all(where)) {
    return(vec_rep(value, vec_size(x)))
  }

  tryCatch(
    vec_assign(x, where, value, x_arg = x_arg, value_arg = value_arg),
    error = function(e) {
      msg <- c(
        paste0("not possible to assign `", value_arg, "`"),
        x = conditionMessage(e)
      )

      stop_jsontools(msg)
    }
  )
}
