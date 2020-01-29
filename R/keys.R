#' Get keys
#'
#' Postgres: `jsonb_object_keys(x)`
#' jq: `keys(x)`
#'
#' @export
#' @examples
#' x1 <- c('{"a": {"x": 1}, "b": 2, "c": 3}')
#' x2 <- c('{"a": {"x": 11, "y": 22}, "s": 12, "t": [1, 2, 3]}')
#' x <- c(x1, x2)
#' jsonr_keys(x)
#' jsonr_keys(c(x, NA), .na = c("a", "b"))
#' jsonr_keys1(x1)
jsonr_keys <- function(x, .na = json_na_error()) {
  r_jq <- jq_do(x, "keys", .na = NA_character_)
  r <- parse_json_vector(r_jq, .na = .na)
  as_list_of(r, .ptype = character())
}

#' @export
jsonr_keys1 <- function(x, .na = json_na_error()) {
  check1(x)
  jsonr_keys(x, .na = .na)[[1]]
}


#' Does key exists?
#'
#' Postgres
#' * single string: '{"a":1, "b":2}'::jsonb ? 'b'
#' * multiple strings (and): '{"a":1, "b":2, "c":3}'::jsonb ?| array['b', 'c']
#' * multiple strings (or): '{"a":1, "b":2, "c":3}'::jsonb ?| array['b', 'c']
#'
#' jq
#' * single string: jq '{"a":1, "b":2}' | 'has("b")'
#' * multiple strings (and): combine single strings with all OR
#' * multiple strings (or): combine single strings with any
#'
#' @export
#' @examples
#' jsonr_has_key(x, "b")
#'
#' jsonr_has_keys(x, c("a", "b"))
#' jsonr_has_all_keys(x, c("a", "b"))
#' jsonr_has_any_keys(x, c("a", "b"))
#'
#' jsonr_has_key1(x1, "b")
#'
#' jsonr_has_keys1(x1, c("a", "b"))
#' jsonr_has_all_keys1(x1, c("a", "b"))
#' jsonr_has_any_keys1(x1, c("a", "b"))
jsonr_has_keys <- function(x, keys) {
  jq_cmd <- jq_has_keys(keys, object = TRUE)
  template <- as.list(set_names(rep_along(keys, NA), keys))
  r_jq <- jq_do(x, jq_cmd, .na = NA_character_)
  r <- parse_json_vector(r_jq, .na = template)
  dplyr::bind_rows(r)
}

#' @rdname jsonr_has_keys
#' @export
jsonr_has_key <- function(x, key) {
  check1(key)
  jsonr_has_keys(x, key)[[key]]
}

#' @rdname jsonr_has_keys
#' @export
jsonr_has_all_keys <- function(x, keys) {
  row_all(jsonr_has_keys(x, keys))
}

#' @rdname jsonr_has_keys
#' @export
jsonr_has_any_keys <- function(x, keys) {
  row_any(jsonr_has_keys(x, keys))
}

#' @rdname jsonr_has_keys
#' @export
jsonr_has_key1 <- function(x, key) {
  check1(x)
  jsonr_has_key(x, key)
}

#' @rdname jsonr_has_keys
#' @export
jsonr_has_keys1 <- function(x, keys) {
  check1(x)
  unlist(jsonr_has_keys(x, keys))
}

#' @rdname jsonr_has_keys
#' @export
jsonr_has_all_keys1 <- function(x, keys) {
  check1(x)
  jsonr_has_all_keys(x, keys)
}

#' @rdname jsonr_has_keys
#' @export
jsonr_has_any_keys1 <- function(x, keys) {
  check1(x)
  jsonr_has_any_keys(x, keys)
}
