json_each <- function(x, path = NULL, allow_scalars = FALSE,
                      allowed_types = NULL) {
  path <- path %||% "$"

  if (!is_string(path)) {
    stop_jsontools("`path` must be `NULL` or a string")
  }

  if (is_true(allow_scalars)) {
    data_col <- glue_sql("
      CASE
        WHEN JSON_VALID(my_tbl.data) THEN my_tbl.data
        WHEN my_tbl.data IS NULL THEN 'null'
        ELSE JSON_ARRAY(JSON_QUOTE(my_tbl.data))
      END
    ", .con = con)
  } else {
    data_col <- DBI::SQL("my_tbl.data")
    data_col <- glue_sql("
      CASE
        WHEN my_tbl.data IS NULL THEN 'null'
        ELSE my_tbl.data
      END
    ", .con = con)
  }

  write_json_tbl(x)

  result <- exec_sqlite_json(
    glue_sql("
     SELECT
       row_id,
       CAST(value AS text) AS value,
       type,
       CAST(key AS text) AS key,
       JSON_TYPE({data_col}, {path}) AS col_type
     FROM
      my_tbl,
      JSON_EACH({data_col}, {path}) AS tmp1
    ", .con = con)
  )

  x_nms <- names2(x)
  result$name <- vec_slice(x_nms, result$row_id)

  if (is_false(allow_scalars) && any(result$col_type %in% scalar_json_types)) {
    msg <- c(
      x = "`x` contains scalar JSON values",
      i = "Use `allow_scalars = TRUE` if you want to allow them."
    )
    stop_jsontools(msg)
  }

  result
}

#' Flatten a JSON array
#'
#' @inheritParams json_extract
#' @param allow_scalars Do not error for scalar elements?
#'
#' @seealso [`json_unnest_longer()`], [`json_unnest_wider()`]
#'
#' @export
#' @examples
#' json_flatten(c("[1, 2]", "[3]"))
#'
#' # names are kep
#' json_flatten(c(x = "[1, 2]", y = "[3]"))
#'
#' # scalar elements produce an error ...
#' try(json_flatten(c(x = "[1, 2]", y = "3")))
#' # ... but can be explicitly allowed with `allow_scalars`
#' json_flatten(c(x = "[1, 2]", y = "3"), allow_scalars = TRUE)
json_flatten <- function(x,
                         ptype = NULL,
                         allow_scalars = FALSE,
                         wrap_scalars = FALSE,
                         bigint_as_char = bigint_default()) {
  # thoughts:
  # * no parameter `path` (for now) because then one should also add the other
  #   parameters from `json_extract`
  # * flattening objects should not be allowed here as usually the keys are
  #   important and the types not the same. One should use `json_each_df()` or
  #   `json_unnest_wider/longer()` instead.
  #   * if allowed: for objects use keys as name instead of recycling names of `x`?

  if (is.list(ptype)) {
    stop_jsontools("`ptype` must not be a list.")
  }

  if (!is_bool(allow_scalars)) {
    stop_jsontools("`allow_scalars` must be a bool.")
  }

  if (is_false(allow_scalars)) {
    if (!all(is_json_array(x, null = TRUE, na = TRUE))) {
      stop_jsontools("`x` must be an array of atoms")
    }
  }

  # drop `NA` as after flattening it is not clear where they came from anyway
  x <- x[!is.na(x)]
  x_each <- json_each(x, allow_scalars = allow_scalars)

  # drop nulls as after flattening it is not clear where they came from anyway
  x_each <- x_each[x_each$type != "null", ]

  if (is_false(allow_scalars)) {
    if (!all(x_each$col_type == "array")) {
      stop_jsontools("`x` must be an array of atoms")
    }

    if (any(x_each$col_type == "object")) {
      stop_jsontools("`x` must not be an object")
    }
  }

  result <- json_convert_value(
    x = x_each$value,
    json_types = x_each$type,
    ptype = ptype,
    wrap_scalars = wrap_scalars,
    bigint_as_char = bigint_as_char
  )

  maybe_name(result, x_each$name)
}

json_each_df <- function(x) {
  result <- json_each(x)
  result$value <- json_convert_value(result$value, result$type, ptype = list())
  nms <- names(result)
  nms[[1]] <- "index"
  names(result) <- nms

  result
}

maybe_name <- function(x, nms) {
  if (any(nms != "")) {
    names(x) <- nms
  }

  x
}
