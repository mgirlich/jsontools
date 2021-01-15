write_json_tbl <- function(x, ...) {
  DBI::dbWriteTable(
    con,
    "my_tbl",
    tibble::tibble(row_id = seq_along(x), data = x, ...),
    overwrite = TRUE
  )
}

exec_sqlite_json <- function(sql) {
  tibble::as_tibble(DBI::dbGetQuery(con, sql))
}

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

#' Flatten an array of values
#'
#' @inheritParams json_extract
#'
#' @export
#' @examples
#' json_flatten(c(x = "[1, 2]", y = "[3]"))
json_flatten <- function(x, ptype = NULL, allow_scalars = FALSE, bigint_as_char = TRUE) {
  # thoughts:
  # * no parameter `path` (for now) because then one should also add the other
  #   parameters from `json_get_query`
  # * flattening objects should not be allowed here as usually the keys are
  #   important and the types not the same. One should use `json_each_df()` or
  #   `json_unnest_wider/longer()` instead.
  #   * if allowed: for objects use keys as name instead of recycling names of `x`?

  # add argument `wrap_scalars`?

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

  # TODO check type
  result <- json_convert_value(
    x = x_each$value,
    json_types = x_each$type,
    ptype = ptype,
    bigint_as_char = bigint_as_char
  )

  # TODO remove hack?
  if (inherits(result, "json2")) {
    result <- vec_cast(result, new_json2())
  }

  maybe_name(result, x_each$name)
}

json_each_df <- function(x) {
  result <- json_each(x)
  result$value <- convert_json_type(result$value, result$type)
  nms <- names(result)
  nms[[1]] <- "index"
  names(result) <- nms

  # TODO remove hack
  if (inherits(result$value, "json2")) {
    result$value <- vec_cast(result$value, new_json2())
  }

  result
}

#' Unnest a JSON array column
#'
#' @param data A data frame.
#' @param col JSON-column of arrays to extract components from.
#' @param path Path where to extract from.
#' @param values_to Name of column to store vector values. Defaults to `col`.
#' @param indices_to A string giving the name of column which will contain the
#'   rownumber before unnesting.
#' @param keys_to A string giving the name of column which will contain the
#'   object keys resp. the array indices.
#' @inheritParams json_flatten
#'
#' @export
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
json_unnest_longer <- function(data,
                               col,
                               path = NULL,
                               values_to = NULL,
                               indices_to = NULL,
                               keys_to = NULL,
                               ptype = NULL,
                               # allow_scalars = TRUE,
                               wrap_scalars = FALSE) {
  # TODO allow to unnest multiple columns at once?
  # TODO transform?
  # TODO parameter to drop rows of empty arrays? and drop NA?
  check_present(col)
  col <- tidyselect::vars_pull(names(data), !!enquo(col))

  values_to <- values_to %||% col
  # drop empty strings
  data <- data[nchar(data[[col]]) > 0 | is.na(data[[col]]), ]

  x_each <- json_each(
    data[[col]],
    path = path,
    # TODO should this be allowed as argument?
    allow_scalars = FALSE
    # wrap_scalars = allow_scalars
  )
  x_each <- x_each[
    # drop json NULL
    x_each$type != "null" |
      # but keep NA
      vec_slice(is.na(data[[col]]), x_each$row_id),
  ]

  # if (is_true(wrap_scalars)) {
  #   x_each <- json_wrap_scalar_afterwards(x_each, ptype)
  # }

  data[[col]] <- NULL
  data <- vec_slice(data, x_each$row_id)
  data[[values_to]] <- json_convert_value(
    x_each$value,
    x_each$type,
    ptype = ptype,
    wrap_scalars = wrap_scalars
  )

  if (!is.null(indices_to)) {
    data[[indices_to]] <- x_each$row_id
  }

  if (!is.null(keys_to)) {
    data[[keys_to]] <- x_each$key
  }

  if (inherits(data[[values_to]], "json2")) {
    data[[values_to]] <- vec_cast(data[[values_to]], new_json2())
  }

  data
}

#' Unnest a JSON object into columns
#'
#' @param data A data frame.
#' @param col JSON-column of objects to extract components from.
#' @param path Path where to extract from.
#' @param names_sort Should the extracted columns be sorted by name? If `FALSE`,
#'   the default, the columns are sorted by appearance.
#' @param names_sep If `NULL`, the default, the keys of the objects in `col`
#'   are used as the column names. If a character it is used to join `col` and
#'   the object keys.
#' @param names_repair What happens if the output has invalid column names?
#' @inheritParams json_flatten
#'
#' @export
#' @examples
#' # turn all components of item into columns with json_unnest_wider()
#' item_df %>%
#'   json_unnest_wider(item)
json_unnest_wider <- function(data,
                              col,
                              path = NULL,
                              ptype = list(),
                              names_sort = FALSE,
                              names_sep = NULL,
                              names_repair = "check_unique",
                              wrap_scalars = FALSE) {
  # TODO argument how to handle `NA`?
  # TODO keeping rows where json type is null but dropping NA is inconsistent
  # --> `drop_na` to drop `NA`, `null`, and `[null]`?
  # TODO transform?

  # TODO argument to only unnest a defined set of values (similar to hoist?)
  # --> add `json_hoist()`?
  # TODO mix of array and text gives text? add `wrap_scalars` instead??

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
  x_each_split <- vec_split(
    x_each[c("row_id", "value", "type")],
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
    ptype <- rep_named(x_each_split$key, ptype)
  }
  if (is_scalar(wrap_scalars)) {
    wrap_scalars <- rep_named(x_each_split$key, wrap_scalars)
  }

  for (i in idx) {
    key <- x_each_split$key[[i]]
    val <- x_each_split$val[[i]]

    if (is_true(wrap_scalars[[key]])) {
      val <- json_wrap_scalar_afterwards(val, ptype[[key]])
    }

    val$value <- convert_json_type(val$value, val$type)

    # TODO less hacky solution?
    # out <- vec_init_along(NA, col_values)
    # empty_flag <- lengths(val$value) == 0
    # out[val$row_id[!empty_flag]] <- vec_c(!!!val$value, .ptype = ptype[[key]])

    values <- vec_c(!!!val$value, .ptype = ptype[[key]])
    out <- vec_init_along(vec_ptype(values), col_values)

    # TODO remove hack
    if (inherits(out, "json2")) {
      out <- vec_cast(out, new_json2())
    }

    empty_flag <- lengths(val$value) == 0
    vec_slice(out, val$row_id[!empty_flag]) <- values
    data[[key]] <- out
  }

  data
}

json_wrap_scalars <- function(x) {
  # TODO without path could do it purely in R
  # TODO allow wrapping scalars at a path? like a combination of modify and wrap?
  # json_type(x)
  write_json_tbl(x)

  exec_sqlite_json(
    "SELECT
      CASE
        WHEN JSON_VALID(my_tbl.data) THEN my_tbl.data
        WHEN my_tbl.data IS NULL THEN 'null'
        ELSE JSON_ARRAY(JSON_QUOTE(my_tbl.data))
      END AS result
    FROM my_tbl")$result %>% json2()
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
