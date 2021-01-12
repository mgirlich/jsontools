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
  agg_array(x)
}

#' @export
json_agg_array.double <- function(x) {
  agg_array(x)
}

#' @export
json_agg_array.logical <- function(x) {
  agg_array(x)
}

#' @export
json_agg_array.character <- function(x) {
  agg_array(x)
}

#' @export
json_agg_array.factor <- function(x) {
  agg_array(x)
}

#' @export
json_agg_array.POSIXct <- function(x) {
  agg_array(x)
}

#' @export
json_agg_array.POSIXlt <- function(x) {
  agg_array(x)
}

#' @export
json_agg_array.Date <- function(x) {
  agg_array(x)
}

#' @export
json_agg_array.complex<- function(x) {
  agg_array(x)
}

agg_array <- function(x) {
  new_json2(jsonlite::toJSON(x))
}

#' Get array length of JSON arrays
#'
#' @param x Vector with JSON.
#' @param path Path
#' @param wrap_scalars Consider scalars as length one array?
#'
#' @return An integer vector of array lengths
#' @export
#'
#' @examples
#' json_array_length(c(NA, "[1, 2, 3]", "[1, 2]"))
#' json_array_length(1, wrap_scalars = TRUE)
json_array_length <- function(x, path = NULL, wrap_scalars = FALSE) {
  path <- path %||% "$"

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
    if (!all(is_json_array(x))) {
      stop_jsontools(
        c(
          x = "`x` has scalar elements.",
          i = "use `wrap_scalars = TRUE` to consider scalars as length 1 array."
        )
      )
    }

    array_lengths <- array_info_df$result
  }

  as.integer(array_lengths)
}

is_json_array <- function(x, null = TRUE, na = TRUE) {
  x <- as.character(x)

  (grepl("^\\s*\\[", x) & grepl("]\\s*$", x) & !is.na(x)) |
    (null & x == "null" & !is.na(x)) |
    (na & is.na(x))
}
