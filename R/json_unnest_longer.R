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
                               wrap_scalars = FALSE,
                               bigint_as_char = bigint_default()) {
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
    wrap_scalars = wrap_scalars,
    bigint_as_char = bigint_as_char
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
