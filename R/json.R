#' @import vctrs
#' @import rlang
NULL

#' importFrom methods setOldClass
# methods::setOldClass(c("json2", "vctrs_vctr"))

#' Construct a json2 object
#'
#' @export
#' @examples
#' json2()
#' json2('{"abc": 1}')
json2 <- function(x = character()) {
  x <- vec_cast(x, character())
  validate_json2(vec_data(x))
  new_json2(x)
}

#' new_json2()
#' new_json2('{"abc": 1}')
#' new_json2(c('{"abc": 1}', '{"def": 2}', '[1, 2, 3]', NA))
#' @export
#' @rdname json2
new_json2 <- function(x = character()) {
  # TODO really export new_json2?
  vec_assert(x, character())
  # TODO remove original formatting? OR keep original formatting?
  # TODO add attribute `pretty`?
  x_sorted <- jq_do(x, flags = jqr::jq_flags(sorted = TRUE), json2 = FALSE)
  new_vctr(x_sorted, class = c("json2"), inherit_base_type = TRUE)
}

#' @export
#' @rdname json2
as_json2 <- function(x) {
  vec_cast(x, new_json2())
}

#' @export
#' @rdname json2
is_json2 <- function(x) {
  vec_is(x, new_json2())
}


validate_json2 <- function(x) {
  validate_results <- lapply(x, validate_single_element)
  valid_flags <- unlist(validate_results)
  if (!all(valid_flags)) {
    errors <- purrr::map_chr(validate_results[!valid_flags], ~ attr(.x, "err"))
    offsets <- purrr::map_int(validate_results[!valid_flags], ~ attr(.x, "offset"))
    locations <- which(!valid_flags)

    stop_jsontools("invalid_json", errors = errors, offsets = offsets, locations = locations)
  }
}

validate_single_element <- function(x) {
  if (length(x) == 0 || is.na(x)) {
    TRUE
  } else {
    jsonlite::validate(x)
  }
}
