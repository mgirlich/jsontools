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
  # TODO what about \b?
  x_escaped <- gsub(r"{(\n|\f|\r|\t|\"|\\)}", r"{\\\1}", x)
  x_escaped_string <- paste0('"', x_escaped, '"')

  new_json2(sprintf("[%s]", paste0(x_escaped_string, collapse = ",")))
}

#' Get array length of JSON arrays
#'
#' @param x Vector with JSON.
#' @param path Path
#'
#' @return An integer vector of array lengths
#' @export
#'
#' @examples
#' json_array_length(c(NA, "[1, 2, 3]", "[1, 2]"))
#' json_array_length(1, wrap_scalars = TRUE)
json_array_length <- function(x, path = NULL, wrap_scalars = FALSE) {
  # TODO parameter so that scalars have length 1 instead of zero?
  # * and warn about scalar elements?
  path <- path %||% "$"

  # if (is_true(wrap_scalars)) {
  #   query <- glue_sql(
  #     "CASE JSON_TYPE(data, {path}) in ('array', 'object', 'null')
  #       WHEN true THEN JSON_ARRAY_LENGTH(data, {path})
  #       ELSE JSON_ARRAY_LENGTH(data, JSON_ARRAY(JSON_QUOTE({path})))
  #     END",
  #     .con = con
  #   )
  # } else {
  #   array_flag <- startsWith(x, "[") & endsWith(x, "]")
  #   if (!all(array_flag, na.rm = TRUE)) {
  #     stop_jsontools()
  #   }
  #
  #   query <- glue_sql("JSON_ARRAY_LENGTH(data, {path})", .con = con)
  # }

  array_info_df <- exec_sqlite_json(
    x,
    glue_sql("
      SELECT
        JSON_ARRAY_LENGTH(data, {path}) AS result,
        JSON_TYPE(data, {path}) AS type
      FROM my_tbl", .con = con)
  )

  if (is_true(wrap_scalars)) {
    array_lengths <- array_info_df$result + !array_info_df$type %in% c("array", "null")
  } else {
    array_lengths <- array_info_df$result
  }

  as.integer(array_lengths)
}
