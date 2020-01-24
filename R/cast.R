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

#' @method vec_cast.json2 default
#' @export
vec_cast.json2.default <- function(x, to, ...) vec_default_cast(x, to, ...)

#' @method vec_cast.json2 json2
#' @export
vec_cast.json2.json2 <- function(x, to, ...) x

#' @method vec_cast.json2 character
#' @export
vec_cast.json2.character <- function(x, to, ...) {
  attributes(x) <- NULL
  json2(unclass(x))
  # json2(vec_data(x))
}

#' @method vec_cast.character json2
#' @export
vec_cast.character.json2 <- function(x, to, ...) {
  # attributes(x) <- NULL
  # as_json2(unclass(x))
  vec_data(x)
}

#' #' @method vec_cast.json2 list
#' #' @export
#' vec_cast.json2.list <- function(x, to, ..., x_arg = "x", to_arg = "to") {
#'   vec_list_cast(x, to, x_arg = x_arg, to_arg = to_arg)
#' }
#'
#' #' @method vec_cast.json2 vctrs_list_of
#' #' @export
#' vec_cast.json2.vctrs_list_of <- function(x, to, ..., x_arg = "x", to_arg = "to") {
#'   vec_list_cast(x, to, x_arg = x_arg, to_arg = to_arg)
#' }


# json classes from other packages ----------------------------------------

#' @method vec_cast.json2 jqson
#' @export
vec_cast.json2.jqson <- function(x, to, ...) {
  # TODO really remove all attributes?
  # attributes(x) <- NULL
  # as_json2(unclass(x))
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



#' @method vec_cast.character jqson
#' @export
vec_cast.character.jqson <- function(x, to, ...) {
  # TODO find better solution than exporting these functions?
  vec_data(x)
}

#' @method vec_cast.character json
#' @export
vec_cast.character.json <- function(x, to, ...) {
  vec_data(x)
}

#' @method vec_cast.character pq_jsonb
#' @export
vec_cast.character.pq_jsonb <- function(x, to, ...) {
  vec_data(x)
}

#' @method vec_cast.character pq_json
#' @export
vec_cast.character.pq_json <- function(x, to, ...) {
  vec_data(x)
}
