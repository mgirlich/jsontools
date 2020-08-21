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

  if (!is.null(path) && !is_string(path)) {
    stop_jsontools("`path` must be `NULL` or a string")
  }

  if (is_true(wrap_scalars)) {
    data_col <- glue_sql("
      CASE JSON_VALID(my_tbl.data)
        WHEN true THEN my_tbl.data
        ELSE JSON_ARRAY(JSON_QUOTE(my_tbl.data))
      END
    ", .con = con)
  } else {
    data_col <- DBI::SQL("my_tbl.data")
  }

  if (is_empty(path)) {
    each_tbl <- glue_sql("JSON_EACH({data_col}) AS tmp1", .con = con)
  } else {
    each_tbl <- glue_sql("JSON_EACH({data_col}, {path}) AS tmp1", .con = con)
  }

  result <- exec_sqlite_json(
    x,
    glue_sql("
     SELECT
       row_id,
       CAST(value AS text) AS value,
       type,
       key
     FROM
      my_tbl,
      {each_tbl}
    ", .con = con
    )
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
  # TODO add parameter `path`?
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
  if (!is_bool(wrap_scalars)) {
    stop_jsontools("`wrap_scalars` must be a bool.")
  }

  x <- x[!is.na(x)]

  if (is_false(wrap_scalars)) {
    if (!all(startsWith(x, "[") & endsWith(x, "]"), na.rm = TRUE)) {
      stop_jsontools("`x` must be an array of atoms")
    }
  }

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
#' @param .na_error Error on NA value?
#'
#' @export
#'
#' @examples
#' df <- tibble::tibble(json = readr::read_file(repurrrsive::discog_json()))
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
                               wrap_scalars = TRUE) {
  # TODO think about array vs object -> index: number vs name
  # TODO ptype, transform?
  # TODO handle NA
  check_present(col)
  col <- tidyselect::vars_pull(names(data), !!enquo(col))

  values_to <- values_to %||% col

  x_each <- json_each(
    data[[col]],
    path = path,
    wrap_scalars = wrap_scalars
  )
  x_each <- x_each[x_each$type != "null", ]
  x_each$value <- convert_json_type(x_each$value, x_each$type)

  data[[col]] <- NULL
  data <- vec_slice(data, x_each$row_id)
  data[[values_to]] <- json_vec_c(x_each$value, x_each$type)

  if (!is.null(indices_to)) {
    data[[indices_to]] <- x_each$row_id
  }

  data
}

maybe_name <- function(x, nms) {
  if (any(nms != "")) {
    names(x) <- nms
  }

  x
}

#' Aggregate into a JSON array
#'
#' @param x Vector to collapse into JSON array.
#'
#' @return A json2 object.
#' @export
#'
#' @examples
#' json_agg_array(1:3)
#' json_agg_array(json2(c('{"a": 1}', '{"b": 2}')))
json_agg_array <- function(x) {
  UseMethod("json_agg_array")
}

#' @export
json_agg_array.json2 <- function(x) {
  new_json2(sprintf("[%s]", paste0(x, collapse = ",")))
}

#' @export
json_agg_array.integer <- function(x) {
  new_json2(sprintf("[%s]", paste0(x, collapse = ",")))
}

#' @export
json_agg_array.character <- function(x) {
  x_escaped <- gsub(r"{(\n|\f|\b|\r|\t|\"|\\)}", r"{\1}", x)
  x_escaped_string <- paste0('"', x_escaped, '"')

  new_json2(sprintf("[%s]", paste0(x_escaped_string, collapse = ",")))
}