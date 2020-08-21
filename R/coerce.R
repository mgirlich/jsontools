#' Coercion
#'
#' Double dispatch methods to support [vctrs::vec_ptype2()].
#'
#' @inheritParams vctrs::vec_ptype2
#'
#' @method vec_ptype2 json2
#' @export
#' @export vec_ptype2.json2
vec_ptype2.json2 <- function(x, y, ..., x_arg = "", y_arg = "") UseMethod("vec_ptype2.json2", y)

#' @method vec_ptype2.json2 default
#' @export
vec_ptype2.json2.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.json2 json2
#' @export
vec_ptype2.json2.json2 <- function(x, y, ...) new_json2()

#' @method vec_ptype2.json2 character
#' @export
vec_ptype2.json2.character <- function(x, y, ...) {
  # validate_json2(y)
  # new_json2()
  character()
}

#' @method vec_ptype2.character json2
#' @export
vec_ptype2.character.json2 <- function(x, y, ...) {
  # validate_json2(x)
  # new_json2()
  character()
}
