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

#' @export
vec_ptype2.json2.character <- function(x, y, ...) character()
vec_ptype2.character.json2 <- function(x, y, ...) character()


# json classes from other packages ----------------------------------------

#' @rdname vec_ptype2.json2
#'
#' @method vec_ptype2 json
#' @export
#' @export vec_ptype2.json
vec_ptype2.json <- function(x, y, ..., x_arg = "", y_arg = "") UseMethod("vec_ptype2.json", y)
#' @method vec_ptype2.json json2
#' @export
vec_ptype2.json.json2 <- function(x, y, ...) new_json2()
#' @method vec_ptype2.json2 json
#' @export
vec_ptype2.json2.json <- function(x, y, ...) new_json2()

#' @rdname vec_ptype2.json2
#'
#' @method vec_ptype2 jqson
#' @export
#' @export vec_ptype2.jqson
vec_ptype2.jqson <- function(x, y, ..., x_arg = "", y_arg = "") UseMethod("vec_ptype2.jqson", y)
#' @method vec_ptype2.jqson json2
#' @export
vec_ptype2.jqson.json2 <- function(x, y, ...) new_json2()
#' @method vec_ptype2.json2 jqson
#' @export
vec_ptype2.json2.jqson <- function(x, y, ...) new_json2()

#' @rdname vec_ptype2.json2
#'
#' @method vec_ptype2 pq_jsonb
#' @export
#' @export vec_ptype2.pq_jsonb
vec_ptype2.pq_jsonb <- function(x, y, ..., x_arg = "", y_arg = "") UseMethod("vec_ptype2.pq_jsonb", y)
#' @method vec_ptype2.pq_jsonb json2
#' @export
vec_ptype2.pq_jsonb.json2 <- function(x, y, ...) new_json2()
#' @method vec_ptype2.json2 pq_jsonb
#' @export
vec_ptype2.json2.pq_jsonb <- function(x, y, ...) new_json2()

#' @rdname vec_ptype2.json2
#'
#' @export
#' @export vec_ptype2.pq_json
vec_ptype2.pq_json <- function(x, y, ..., x_arg = "", y_arg = "") UseMethod("vec_ptype2.pq_json", y)
#' @export
vec_ptype2.pq_json.json2 <- function(x, y, ...) new_json2()
#' @export
vec_ptype2.json2.pq_json <- function(x, y, ...) new_json2()
