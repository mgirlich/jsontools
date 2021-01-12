#' Casting
#'
#' Double dispatch methods to support [vctrs::vec_cast()].
#'
#' @inheritParams vctrs::vec_cast
#'
#' @keywords internal
#' @method vec_cast json2
#' @export
#' @export vec_cast.json2
vec_cast.json2 <- function(x, to, ...) UseMethod("vec_cast.json2")

#' @method vec_cast.json2 json2
#' @export
vec_cast.json2.json2 <- function(x, to, ...) x

#' @method vec_cast.json2 character
#' @export
vec_cast.json2.character <- function(x, to, ...) {
  # workaround for problem with rendering in markdown
  x <- ifelse(x == "__NA__", NA, x)
  json2(vec_data(x))
}

#' @method vec_cast.character json2
#' @export
vec_cast.character.json2 <- function(x, to, ...) {
  vec_data(x)
}

#' @method vec_cast.list json2
#' @export
vec_cast.list.json2 <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  lapply(seq_along(x), function(i) x[[i]])
}


# json classes from other packages ----------------------------------------

#' @method vec_cast.json2 jqson
#' @export
vec_cast.json2.jqson <- function(x, to, ...) {
  json2(vec_data(x))
}

#' @method vec_cast.json2 json
#' @export
vec_cast.json2.json <- function(x, to, ...) {
  json2(vec_data(x))
}

#' @method vec_cast.json2 pq_jsonb
#' @export
vec_cast.json2.pq_jsonb <- function(x, to, ...) {
  json2(vec_data(x))
}

#' @method vec_cast.json2 pq_json
#' @export
vec_cast.json2.pq_json <- function(x, to, ...) {
  json2(vec_data(x))
}

#' @method vec_cast.character json
#' @export
vec_cast.character.json <- function(x, to, ...) {
  # implemented as a workaround so that `json_verbatim = TRUE`
  # works for json2 objects
  vec_data(x)
}
