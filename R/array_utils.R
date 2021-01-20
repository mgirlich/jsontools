#' Aggregate into a JSON array
#'
#' @param x Vector to collapse into JSON array.
#'
#' @return A json2 object.
#' @export
#'
#' @examples
#' json_array_agg(1:3)
#' json_array_agg(json2(c('{"a": 1}', '{"b": 2}')))
#'
#' # can be quite useful in combination with `dplyr::group_by()`
#' if (require("dplyr", quietly = TRUE, warn.conflicts = FALSE)) {
#'   tibble::tibble(
#'     group = c(1, 1, 2, 2),
#'     json = c(1:4)
#'   ) %>%
#'     dplyr::group_by(group) %>%
#'     dplyr::summarise(json = json_array_agg(json))
#' }
json_array_agg <- function(x) {
  UseMethod("json_array_agg")
}

#' @export
json_array_agg.json2 <- function(x) {
  new_json2(sprintf("[%s]", paste0(x, collapse = ",")))
}

#' @export
json_array_agg.integer <- function(x) {
  agg_array(x)
}

#' @export
json_array_agg.double <- function(x) {
  agg_array(x)
}

#' @export
json_array_agg.logical <- function(x) {
  agg_array(x)
}

#' @export
json_array_agg.character <- function(x) {
  agg_array(x)
}

#' @export
json_array_agg.factor <- function(x) {
  agg_array(x)
}

#' @export
json_array_agg.POSIXct <- function(x) {
  agg_array(x)
}

#' @export
json_array_agg.POSIXlt <- function(x) {
  agg_array(x)
}

#' @export
json_array_agg.Date <- function(x) {
  agg_array(x)
}

#' @export
json_array_agg.complex <- function(x) {
  agg_array(x)
}

agg_array <- function(x) {
  new_json2(jsonlite::toJSON(x))
}

#' Get array length of JSON arrays
#'
#' @inheritParams json_extract
#' @param wrap_scalars Consider scalars as length one array?
#'
#' @return An integer vector of array lengths
#' @export
#'
#' @examples
#' json_array_length(c(NA, "[1, 2, 3]", "[1, 2]"))
#'
#' # scalars produce an error unless `wrap_scalars` is `TRUE`
#' json_array_length(1, wrap_scalars = TRUE)
json_array_length <- function(x, wrap_scalars = FALSE) {
  path <- "$"

  write_json_tbl(x)
  array_info_df <- exec_sqlite_json(
    glue_sql("
      SELECT
        JSON_ARRAY_LENGTH(data, {path}) AS result,
        JSON_TYPE(data, {path}) AS type
      FROM my_tbl", .con = con)
  )

  if (is_true(wrap_scalars)) {
    array_lengths <- array_info_df$result + !array_info_df$type %in% c("array", "null")
  } else {
    # if (!all(is_json_array(x))) {
    if (!all(array_info_df$type %in% c("array", "null") | is.na(array_info_df$type))) {
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

#' Get the type of array elements
#'
#' This is barely useful on its own but can be of some use in combination with
#' [`json_array_length()`].
#'
#' @inheritParams json_extract
#'
#' @return A character vector of JSON types.
#'
#' @seealso [`json_type()`]
#'
#' @export
#' @examples
#' json_array_types(c("[1, true]", '["a", [1]]'))
json_array_types <- function(x) {
  json_each(x)$type
}

is_json_array <- function(x, null = TRUE, na = TRUE) {
  x <- as.character(x)

  (grepl("^\\s*\\[", x) & grepl("]\\s*$", x) & !is.na(x)) |
    (null & x == "null" & !is.na(x)) |
    (na & is.na(x))
}

#' Wrap scalars in a JSON array
#'
#' @param x A character or numeric vector.
#'
#' @return A `json2` vector.
#' @export
#'
#' @examples
#' json_wrap_scalars(c('["a", "b"]', "c", "d"))
#' json_wrap_scalars(c(1, 2))
json_wrap_scalars <- function(x) {
  write_json_tbl(x)

  exec_sqlite_json(
    "SELECT
      CASE
        WHEN JSON_VALID(my_tbl.data) THEN CASE
          WHEN
            JSON_TYPE(my_tbl.data) NOT IN ('array', 'object')
          THEN
            JSON_ARRAY(my_tbl.data)
          ELSE
            my_tbl.data
          END
        WHEN my_tbl.data IS NULL THEN 'null'
        ELSE JSON_ARRAY(JSON_QUOTE(my_tbl.data))
      END AS result
    FROM my_tbl"
  )$result %>% json2()
}
