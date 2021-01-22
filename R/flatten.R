json_each <- function(x, path = NULL, allow_scalars = FALSE) {
  # TODO handle NA
  # * error by default
  # * in `json_flatten*()` it probably doesn't make sense to support anything else?
  # * in `json_unnest_longer()` one might want to keep the NA

  # TODO decide how exactly scalars should be handled
  # * integers work (b/c they are valid JSON)
  # * character fail (b/c they are not valid JSON)
  # -> check if each element of x is an array
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
                         bigint_as_char = TRUE) {
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
  x_each <- json_each(x[!is.na(x)], allow_scalars = allow_scalars)

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

#' Unnest a JSON array column
#'
#' Unnest a column of JSON arrays in a data frame producing a longer data frame.
#'
#' @param data A data frame.
#' @param col JSON-column of arrays to extract components from.
#' @inheritParams json_extract
#' @param values_to Name of column to store vector values. Defaults to `col`.
#' @param row_numbers_to Name of column to store the row number before unnesting.
#' @param indices_to Name of column to store the array index of each element;
#'   note that this starts with 0.
#'
#' @seealso [`json_unnest_wider()`]
#'
#' @export
#' @examples
#' df <- tibble::tibble(
#'   x = c("a", "b"),
#'   json = c("[1, 2]", "[3, 4, 5]")
#' )
#' df
#'
#' df %>%
#'   json_unnest_longer(
#'     "json",
#'     row_numbers_to = "id",
#'     indices_to = "index"
#'   )
json_unnest_longer <- function(data,
                               col,
                               values_to = NULL,
                               row_numbers_to = NULL,
                               indices_to = NULL,
                               ptype = NULL,
                               wrap_scalars = FALSE) {
  check_present(col)
  col <- tidyselect::vars_pull(names(data), !!enquo(col))

  values_to <- values_to %||% col
  # drop empty strings
  data <- data[nchar(data[[col]]) > 0 | is.na(data[[col]]), ]

  x_each <- json_each(
    data[[col]],
    allow_scalars = FALSE
  )

  drop_na <- FALSE
  if (drop_na) {
    x_each <- x_each[!(
      is.na(x_each$type) |
        # drop JSON "null"
        x_each$type == "null"
    ), ]
  } else {
    # empty JSON arrays "[]" don't return a row in `x_each`
    missing_row_ids <- setdiff(vec_seq_along(data), x_each$row_id)

    row_ids <- c(x_each$row_id, missing_row_ids)
    x_each <- vec_rbind(
      x_each,
      tibble(
        row_id = missing_row_ids,
        name = names2(data[[col]])[missing_row_ids]
      )
    )

    x_each <- vec_slice(x_each, vec_order(row_ids))
  }

  data[[col]] <- NULL
  data <- vec_slice(data, x_each$row_id)
  data[[values_to]] <- json_convert_value(
    x_each$value,
    x_each$type,
    ptype = ptype,
    wrap_scalars = wrap_scalars
  )

  if (!is.null(row_numbers_to)) {
    data[[row_numbers_to]] <- x_each$row_id
  }

  if (!is.null(indices_to)) {
    data[[indices_to]] <- x_each$key
  }

  if (inherits(data[[values_to]], "json2")) {
    data[[values_to]] <- vec_cast(data[[values_to]], new_json2())
  }

  data
}

#' Unnest a JSON object into columns
#'
#' Unnest a column of JSON objects in a data frame producing a wider data frame.
#'
#' @inheritParams json_unnest_longer
#' @param names_sort Should the extracted columns be sorted by name? If `FALSE`,
#'   the default, the columns are sorted by appearance.
#' @param names_sep If `NULL`, the default, the keys of the objects in `col`
#'   are used as the column names. If a character it is used to join `col` and
#'   the object keys.
#' @param names_repair What happens if the output has invalid column names?
#' @param wrap_scalars A named list of `TRUE` or `FALSE` specifying for each
#'   field whether to wrap scalar values in a JSON array. Unspecified fields
#'   are not wrapped. This can also be a single value of `TRUE` or `FALSE`
#'   that is then used for every field.
#'   Note that scalars are only wrapped if either
#'   * `ptype` is `new_json_array()` or `json2` vector.
#'   * `ptype` is `NULL` and the elements are a mix of scalar values and arrays.
#'
#' @seealso [`json_unnest_longer()`]
#'
#' @export
#' @examples
#' # turn all components of item into columns with json_unnest_wider()
#' tibble::tibble(
#'   id = 1:2,
#'   x = c(
#'     '{"name": "Peter", "age": 19}',
#'     '{"age": 37}'
#'   )
#' ) %>%
#'   json_unnest_wider(x)
#'
#' # sort names and specify proto types
#' tibble::tibble(
#'   id = 1:2,
#'   x = c(
#'     '{"name": "Peter", "age": 19, "purchase_ids": [1, 2]}',
#'     '{"age": 37, "purchase_ids": []}'
#'   )
#' ) %>%
#'   json_unnest_wider(
#'     x,
#'     ptype = list(
#'       age = integer(),
#'       name = character(),
#'       purchase_id = new_json_array()
#'     ),
#'     names_sort = TRUE
#'   )
json_unnest_wider <- function(data,
                              col,
                              ptype = list(),
                              names_sort = FALSE,
                              names_sep = NULL,
                              names_repair = "check_unique",
                              wrap_scalars = FALSE) {
  check_present(col)
  col <- tidyselect::vars_pull(names(data), !!enquo(col))

  col_values <- data[[col]]

  x_each <- json_each(col_values)

  if (!all(x_each$col_type %in% c("object", "null"))) {
    stop_jsontools("every element of `col` must be a json object")
  }

  x_each <- x_each[!is.na(x_each$key), ]
  row_ids_dropped <- setdiff(vec_seq_along(data), x_each$row_id)

  x_each_split <- vec_split(
    x_each[c("row_id", "value", "type")],
    x_each$key
  )

  if (!is_null(names_sep)) {
    x_each_split$key <- paste(col, x_each_split$key, sep = names_sep)
  }

  x_each_split$key <- vec_as_names(x_each_split$key, repair = names_repair)
  if (is_true(names_sort)) {
    idx <- vec_order(x_each_split$key)
  } else {
    idx <- vec_seq_along(x_each_split)
  }

  if (is_scalar(ptype)) {
    ptype <- rep_named(x_each_split$key, ptype)
  }
  if (is_scalar(wrap_scalars)) {
    wrap_scalars <- rep_named(x_each_split$key, wrap_scalars)
  }

  for (i in idx) {
    key <- x_each_split$key[[i]]
    val <- x_each_split$val[[i]]

    tryCatch(
      values <- json_convert_value(
        val$value,
        val$type,
        ptype = ptype[[key]],
        wrap_scalars = wrap_scalars[[key]]
      ),
      error = function(e) {
        msg <- c(
          paste0("Issue when extracting key `", key, "`"),
          conditionMessage(e)
        )

        stop_jsontools(msg)
      }
    )

    out <- vec_init_along(vec_ptype(values), col_values)

    empty_flag <- lengths(val$value) == 0
    vec_slice(out, val$row_id[!empty_flag]) <- values
    data[[key]] <- out
  }

  data[[col]] <- NULL
  drop_na <- FALSE
  if (drop_na) {
    data <- vec_slice(data, -row_ids_dropped)
  }

  data
}

is_scalar <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  }

  if (is.list(x)) {
    (length(x) == 1) && !have_name(x)
  } else {
    length(x) == 1
  }
}

maybe_name <- function(x, nms) {
  if (any(nms != "")) {
    names(x) <- nms
  }

  x
}
