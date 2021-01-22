check_present <- function(x) {
  arg <- ensym(x)
  if (missing(x)) {
    abort(paste0("Argument `", arg, "` is missing with no default"))
  }
}

#' Unbox a vector or data frame
#'
#' Mark a vector of length one to not be wrapped in an array when formatted as
#' `JSON`. This is only a tiny wrapper around `jsonlite::unbox()` to avoid
#' conflict with `rlang::unbox()`.
#'
#' @param x atomic vector of length 1, or data frame with 1 row.
#'
#' @return A singleton version of `x`.
#' @export
#'
#' @examples
#' format_json(list(foo = 123))
#' format_json(list(foo = json_u(123)))
json_u <- function(x) {
  jsonlite::unbox(x)
}

my_map_chr <- function(x, f) {
  f <- as_function(f)
  vapply(x, f, character(1))
}

my_map_int <- function(x, f) {
  f <- as_function(f)
  vapply(x, f, integer(1))
}
