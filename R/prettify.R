#' Prettify/Minify a JSON vector
#'
#' A wrapper around [`jsonlite::prettify()`] resp. [`jsonlite::minify()`].
#'
#' @param x A JSON vector.
#' @param indent number of spaces to indent.
#'
#' @return A `json2` vector.
#' @export
#'
#' @examples
#' x <- c("[1,2,   3]", '{"a": 1, "b": 2}')
#' json_prettify(x)
#' json_minify(x)
json_prettify <- function(x, indent = 3) {
  json2_do(x, jsonlite::prettify, indent = indent, json2 = TRUE)
}

#' @rdname json_prettify
#' @export
json_minify <- function(x) {
  json2_do(x, jsonlite::minify, json2 = TRUE)
}

json2_do <- function(x, f, json2 = TRUE, ...) {
  x <- as.character(x)
  x_na <- is.na(x)
  x[!x_na] <- as.character(lapply(x[!x_na], f, ...))

  if (is_true(json2)) {
    new_json2(x)
  } else {
    x
  }
}
