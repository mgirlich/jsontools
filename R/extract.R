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
#' @param wrap_scalars Should scalars be wrapped?
#' @param bigint_as_char Convert big integers to character?
#'
#' @return A vector with class given by `.ptype` and length equal to `x`.
#'
#' @export
#' @examples
#' x1 <- '{"a": 1, "b": 2}'
#' json_extract(x1, "$.a")
#'
#' json_extract(c(NA_character_, x1), "$.a")
#' json_extract(c(NA_character_, x1), "$.a", na = 3)
#'
#' try(json_extract(x1, "$.c"))
#' json_extract(x1, "$.c", default = "not there")
#'
#' json_extract('{"a": {"b": 1}}', "$.a")
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

  na <- vec_cast(na, ptype)
  if (length(na) != 1) {
    stop_jsontools("`na` must have length 1")
  }

  default <- vec_cast(default, ptype)
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

  # TODO if `ptype` was `NULL` above then we don't get an understandable error
  # message if `na` or `default` doesn't fit to the ptype...

  # now that the values are parsed the original NAs can be replaced
  # TODO replace NA earlier on?
  x_result <- replace_na(x_result, is.na(x), na)

  path_not_found <- is.na(result_df$type) & !is.na(x)
  x_result <- replace_not_found(x_result, path_not_found, default)

  # TODO remove hack?
  if (inherits(x_result, "json2")) {
    x_result <- vec_cast(x_result, new_json2())
  }

  x_result
}

json_wrap_scalar_afterwards <- function(x_each, ptype) {
  if ((is_null(ptype) || inherits(ptype, "json2")) &&
      any(x_each$type %in% c("array", "object"), na.rm = TRUE)) {
    idx <- !x_each$type %in% c("null", "array", "object") & !is.na(x_each$type)
    x_each$value[idx] <- vapply(
      x_each$value[idx],
      json_agg_array,
      character(1)
    )
    x_each$type[idx] <- "array"
  }

  x_each
}

replace_not_found <- function(x, not_found_flag, default) {
  if (any(not_found_flag)) {
    if (is.null(default)) {
      msg <- c(
        x = "`path` does not always exist.",
        i = "Did you provide an incorrect path?",
        i = "With `default` you can specify a default value for missing elements."
      )
      stop_jsontools(msg)
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
