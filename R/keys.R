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
#' json_keys(x)
#' json_keys1(x1)
json_keys <- function(x) {
  r <- parse_json_vector(jq_do(x, "keys"))
  r <- lapply(r, `%||%`, character())
  as_list_of(r, .ptype = character())
}

#' @export
json_keys1 <- function(x) {
  check1(x)
  json_keys(x)[[1]]
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
#' json_has_key(x, "b")
#'
#' json_has_keys(x, c("a", "b"))
#' json_has_all_keys(x, c("a", "b"))
#' json_has_any_keys(x, c("a", "b"))
#'
#' json_has_key1(x1, "b")
#'
#' json_has_keys1(x1, c("a", "b"))
#' json_has_all_keys1(x1, c("a", "b"))
#' json_has_any_keys1(x1, c("a", "b"))
json_has_keys <- function(x, keys) {
  # TODO keys via dots or via single argument?
  # TODO use IN for multiple keys?
  # https://stedolan.github.io/jq/manual/#example20
  # jqr::jq('["foo", "bar"]', '.[] | in({"foo": 42})')
  jq_cmd <- jq_has_keys(keys)
  template <- rep_along(keys, NA)
  purrr::map(
    jq_do(x, jq_cmd),
    ~ setNames(parse_json(.x, na = template), keys)
  )
}

#' @rdname json_has_keys
#' @export
json_has_key <- function(x, key) {
  check1(key)
  r <- purrr::flatten_lgl(json_has_keys(x, key))
  unname(r)
}

#' @rdname json_has_keys
#' @export
json_has_all_keys <- function(x, keys) {
  purrr::map_lgl(json_has_keys(x, keys), all)
}

#' @rdname json_has_keys
#' @export
json_has_any_keys <- function(x, keys) {
  purrr::map_lgl(json_has_keys(x, keys), any)
}

#' @rdname json_has_keys
#' @export
json_has_key1 <- function(x, key) {
  check1(x)
  json_has_key(x, key)
}

#' @rdname json_has_keys
#' @export
json_has_keys1 <- function(x, keys) {
  check1(x)
  json_has_keys(x, keys)[[1]]
}

#' @rdname json_has_keys
#' @export
json_has_all_keys1 <- function(x, keys) {
  check1(x)
  json_has_all_keys(x, keys)[[1]]
}

#' @rdname json_has_keys
#' @export
json_has_any_keys1 <- function(x, keys) {
  check1(x)
  json_has_any_keys(x, keys)[[1]]
}
