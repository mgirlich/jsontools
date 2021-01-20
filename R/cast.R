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
#' @noRd
#' @export
vec_cast.json2_object <- function(x, to, ...) UseMethod("vec_cast.json2_object")
#' @noRd
#' @export
vec_cast.json2_array <- function(x, to, ...) UseMethod("vec_cast.json2_array")

# self cast
#' @export
vec_cast.json2.json2 <- function(x, to, ...) x
#' @export
vec_cast.json2_object.json2_object <- function(x, to, ...) x
#' @export
vec_cast.json2_array.json2_array <- function(x, to, ...) x

# cast to json2
#' @export
vec_cast.json2.json2_object <- function(x, to, ...) new_json2(x)
#' @export
vec_cast.json2.json2_array <- function(x, to, ...) new_json2(x)

# cast to object/array
#' @export
vec_cast.json2_object.json2 <- function(x, to, ...) {
  stop("not yet implemented")
}
#' @export
vec_cast.json2_array.json2 <- function(x, to, ...) {
  stop("not yet implemented")
}

# cast to character
#' @export
vec_cast.character.json2 <- function(x, to, ...) vec_data(x)
#' @export
vec_cast.character.json2_object <- function(x, to, ...) vec_data(x)
#' @export
vec_cast.character.json2_array <- function(x, to, ...) vec_data(x)

#' @export
vec_cast.list.json2 <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  lapply(seq_along(x), function(i) x[[i]])
}

# cast from character
#' @export
vec_cast.json2.character <- function(x, to, ...) {
  # workaround for problem with rendering in markdown
  x <- ifelse(x == "__NA__", NA, x)
  json2(vec_data(x))
}
#' @export
vec_cast.json_array.character <- function(x, to, ...) {
  # stop("not yet implemented")
  if (!grepl("^\\s.\\[", x)) {
    abort("`x` must be a JSON array.")
  }
  new_json_array(json2(vec_data(x)))
}
#' @export
vec_cast.json_object.character <- function(x, to, ...) {
  if (!grepl("^\\s.\\[", x)) {
    abort("`x` must be a JSON object.")
  }
  new_json_object(json2(vec_data(x)))
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
