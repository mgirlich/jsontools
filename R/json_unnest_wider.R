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
#' @return A data frame, or subclass of data frame of the same length as `data`.
#' @export
#'
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
                              wrap_scalars = FALSE,
                              bigint_as_char = bigint_default()) {
  check_present(col)
  col <- tidyselect::vars_pull(names(data), !!enquo(col))

  col_values <- data[[col]]

  if (!is_json2(col_values)) {
    json_assert_valid(col_values, x_arg = col)
  }

  x_each <- json_each(col_values)

  if (!all(x_each$col_type %in% c("object", "null"))) {
    stop_jsontools("every element of `col` must be a JSON object")
  }

  # drop `NA` keys as we don't want to generate a column from them
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
  if (is_scalar(bigint_as_char)) {
    bigint_as_char <- rep_named(x_each_split$key, bigint_as_char)
  }

  for (i in idx) {
    key <- x_each_split$key[[i]]
    val <- x_each_split$val[[i]]

    tryCatch(
      values <- json_convert_value(
        val$value,
        val$type,
        ptype = ptype[[key]],
        wrap_scalars = wrap_scalars[[key]],
        bigint_as_char = bigint_as_char[[key]]
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
