exec_sqlite_json <- function(x, sql, ...) {
  DBI::dbWriteTable(
    con,
    "my_tbl",
    tibble::tibble(row_id = seq_along(x), data = x, ...),
    overwrite = TRUE
  )

  suppressWarnings(df <- DBI::dbGetQuery(con, sql))

  tibble::as_tibble(df)
}

# sqlite_json_summarise <- function(x, sql, f = function(df) df$result,
#                                   .na_error = FALSE) {
#   if (is_true(.na_error) && any(is.na(x))) {
#     abort("NA discovered")
#   }
#
#   input <- x[!is.na(x)]
#   f(exec_sqlite_json(input, sql))
# }


json_each <- function(x, path = NULL, wrap_scalars = FALSE) {
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

  if (is_true(wrap_scalars)) {
    data_col <- glue_sql("
      CASE
        WHEN JSON_VALID(my_tbl.data) THEN my_tbl.data
        WHEN my_tbl.data IS NULL THEN 'null'
        ELSE JSON_ARRAY(JSON_QUOTE(my_tbl.data))
      END
    ", .con = con)
  } else {
    data_col <- DBI::SQL("my_tbl.data")
  }

  result <- exec_sqlite_json(
    x,
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

#' Flatten an array of JSON-objects or JSON-arrays
#'
#' @param x A JSON vector.
#'
#' @export
json_flatten_query <- function(x) {
  # thoughts:
  # * no parameter `path` (for now) because then one should also add the other
  #   parameters from `json_get_query`
  # TODO for objects use keys as name instead of recycling names of `x`?

  # drop `NA` as after flattening it is not clear where they came from anyway
  x <- x[!is.na(x)]
  x_each <- json_each(x[!is.na(x)])

  # drop nulls as after flattening it is not clear where they came from anyway
  x_each <- x_each[x_each$type != "null", ]

  if (!all(x_each$type %in% c("object")) &&
    !all(x_each$type %in% c("array"))) {
    stop_jsontools("`x` must be an array of objects or an array of arrays")
  }

  result <- json_convert_value(
    x = x_each$value,
    json_types = x_each$type,
    ptype = character()
  )

  json2(maybe_name(result, x_each$name))
}

#' Flatten an array of values
#'
#' @param x A JSON vector.
#' @param ptype Output type. If `NULL`, the default, the output type is
#' determined by computing the common type across all elements of `...`.
#' @param wrap_scalars Should scalars be wrapped?
#'
#' @export
json_flatten_value <- function(x, ptype = NULL, wrap_scalars = FALSE) {
  # thoughts:
  # * flattening objects should not be allowed here as usually the keys are
  #   important and the types not the same. One should use `json_each_df()` or
  #   `json_unnest_wider/longer()` instead.
  if (!is_bool(wrap_scalars)) {
    stop_jsontools("`wrap_scalars` must be a bool.")
  }

  if (is_false(wrap_scalars)) {
    if (!all(startsWith(x, "[") & endsWith(x, "]"), na.rm = TRUE)) {
      stop_jsontools("`x` must be an array of atoms")
    }
  }

  # drop `NA` as after flattening it is not clear where they came from anyway
  x <- x[!is.na(x)]
  x_each <- json_each(x, wrap_scalars = wrap_scalars)

  # drop nulls as after flattening it is not clear where they came from anyway
  x_each <- x_each[x_each$type != "null", ]

  if (any(x_each$type %in% c("object", "array"))) {
    stop_jsontools("`x` must be an array of atoms")
  }

  result <- json_convert_value(
    x = x_each$value,
    json_types = x_each$type,
    ptype = ptype
  )

  maybe_name(result, x_each$name)
}

#' Flatten an array
#'
#' @param x A JSON vector.
#'
#' @export
json_each_df <- function(x) {
  result <- json_each(x)
  result$value <- convert_json_type(result$value, result$type)
  nms <- names(result)
  nms[[1]] <- "index"
  names(result) <- nms

  result
}

#' Rectangle a JSON array column
#'
#' @param data A data frame.
#' @param col JSON-column to extract components from.
#' @param path Path where to extract from.
#' @param values_to Name of column to store vector values. Defaults to `col`.
#' @param indices_to A string giving the name of column which will contain the
#'   inner names or position (if not named) of the values.
#' @inheritParams json_flatten_value
#'
#' @export
#'
#' @examples
#' df <- tibble::tibble(json = discog_json)
#' df
#'
#' item_df <- df %>%
#'   json_unnest_longer(
#'     "json",
#'     values_to = "item"
#'   )
#' item_df
#'
#' item_df %>%
#'   json_unnest_longer(
#'     "item",
#'     path = c("$.basic_information.artists"),
#'     values_to = "artist",
#'     indices_to = "component_id"
#'   )
json_unnest_longer <- function(data, col,
                               path = NULL,
                               values_to = NULL,
                               indices_to = NULL,
                               keys_to = NULL,
                               ptype = NULL,
                               wrap_scalars = TRUE) {
  # TODO transform?
  check_present(col)
  col <- tidyselect::vars_pull(names(data), !!enquo(col))

  values_to <- values_to %||% col
  # drop empty strings
  data <- data[nchar(data[[col]]) > 0 | is.na(data[[col]]), ]

  x_each <- json_each(
    data[[col]],
    path = path,
    wrap_scalars = wrap_scalars
  )
  x_each <- x_each[
    # drop json NULL
    x_each$type != "null" |
      # but keep NA
      vec_slice(is.na(data[[col]]), x_each$row_id),
  ]

  data[[col]] <- NULL
  data <- vec_slice(data, x_each$row_id)
  data[[values_to]] <- json_convert_value(
    x_each$value,
    x_each$type,
    ptype = ptype
  )

  if (!is.null(indices_to)) {
    data[[indices_to]] <- x_each$row_id
  }

  if (!is.null(keys_to)) {
    data[[keys_to]] <- x_each$key
  }

  data
}

#' @export
json_unnest_wider <- function(data,
                              col,
                              path = NULL,
                              ptype = list(),
                              names_sort = FALSE,
                              names_sep = NULL,
                              names_repair = "check_unique") {
  # TODO argument how to handle `NA`?
  # TODO keeping rows where json type is null but dropping NA is inconsistent
  # --> `drop_na` to drop `NA`, `null`, and `[null]`?
  # TODO transform?
  check_present(col)
  col <- tidyselect::vars_pull(names(data), !!enquo(col))

  # drop na
  data <- data[!is.na(data[[col]]), ]
  col_values <- data[[col]]

  x_each <- json_each(
    col_values,
    path = path
  )

  if (!all(x_each$col_type == "object")) {
    stop_jsontools("every element of `col` must be a json object")
  }

  x_each <- x_each[!x_each$type == "null", ]
  x_each$value <- convert_json_type(x_each$value, x_each$type)

  x_each_split <- vec_split(
    x_each[c("row_id", "value")],
    x_each$key
  )

  data[[col]] <- NULL

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
    ptype <- rep_named(unique(spec$.value), x_each_split$key)
  }

  for (i in idx) {
    key <- x_each_split$key[[i]]
    val <- x_each_split$val[[i]]

    # TODO less hacky solution?
    out <- vec_init_along(NA, col_values)
    empty_flag <- lengths(val$value) == 0
    out[val$row_id[!empty_flag]] <- vec_c(!!!val$value, .ptype = ptype[[key]])
    data[[key]] <- out
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
