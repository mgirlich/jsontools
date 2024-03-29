#' @importFrom methods setOldClass
methods::setOldClass(c("json2", "vctrs_vctr"))

#' Construct a json2 object
#'
#' `new_json2()` is a fast, low-level constructor that takes a character vector.
#' `json2()` checks the input for validity.
#' `as_json2()` and `is_json2()` are simple forwarders to `vctrs::vec_cast()`
#' and `vctrs::vec_is()` respectively.
#'
#' @param x A character vector.
#'
#' @return A `json2` vector.
#' @export
#'
#' @examples
#' json2()
#' json2('{"abc": 1}')
json2 <- function(x = character()) {
  if (inherits(x, c("jqson", "json", "pq_json", "pq_jsonb"))) {
    x <- as.character(x)
  } else {
    x <- vec_cast(x, character())
  }

  json_assert_valid(vec_data(x))
  new_json2(x)
}

#' @rdname json2
#'
#' @export
#' @examples
#'
#' new_json2()
#' new_json2('{"abc": 1}')
#' new_json2(c('{"abc": 1}', '{"def": 2}', "[1, 2, 3]", NA))
new_json2 <- function(x = character()) {
  if (!is.character(x)) {
    stop_jsontools("`x` must be a character vector")
  }
  new_vctr(x, class = c("json2", "json"), inherit_base_type = TRUE)
}

#' @rdname json2
#' @export
as_json2 <- function(x) {
  vec_cast(x, new_json2())
}

#' @rdname json2
#' @export
is_json2 <- function(x) {
  vec_is(x, new_json2())
}


#' @rdname json_assert_valid
#' @export
json_is_valid <- function(x) {
  if (!is.character(x)) {
    stop_jsontools("`x` must be a character vector.")
  }

  vapply(x, validate_scalar_json, logical(1))
}


#' Assert vector is valid JSON.
#'
#' Uses [`jsonlite::validate()`] under the hood.
#'
#' @param x A character vector.
#' @param x_arg Argument name for `x`. Used in error message to inform the user
#'   about the location of the error.
#' @param ... These dots are for future extensions and must be empty.
#' @param error_call The execution environment of a currently running function,
#'   e.g. [rlang::caller_env()]. The function will be mentioned in error
#'   messages as the source of the error. See the call argument of
#'   [rlang::abort()] for more information.
#'
#' @return `json_is_valid()` returns a vector of `TRUE` and `FALSE`.
#'   `json_assert_valid()` either throws an error with information on the
#'   invalid elements or returns `x` invisibly
#' @export
#'
#' @examples
#' json_is_valid("[1, 2]")
#' json_is_valid("[1, 2")
#'
#' json_assert_valid("[1, 2]")
#' \dontrun{
#' json_assert_valid("[1, 2")
#' }
json_assert_valid <- function(x, x_arg = "", ..., error_call = caller_env()) {
  validate_results <- lapply(x, validate_scalar_json)
  valid_flags <- unlist(validate_results)

  if (!all(valid_flags)) {
    errors <- purrr::map_chr(validate_results[!valid_flags], ~ attr(.x, "err"))
    offsets <- purrr::map_int(validate_results[!valid_flags], ~ attr(.x, "offset"))
    locations <- which(!valid_flags)

    stop_jsontools(
      message = "The JSON isn't valid",
      error_type = "invalid_json",
      errors = errors,
      offsets = offsets,
      locations = locations,
      x_arg = x_arg,
      call = error_call
    )
  }

  invisible(x)
}

validate_scalar_json <- function(x) {
  if (is.na(x)) {
    TRUE
  } else {
    jsonlite::validate(x)
  }
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.json2 <- function(x, ...) {
  out <- format(x)
  pillar::new_pillar_shaft_simple(out, align = "right", min_width = 10)
}
