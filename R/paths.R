#' Get paths
#'
#' Postgres: doesn't exist
#' jq: `paths(x)`
#'
#' @export
#' @examples
#' x1 = c('{"a": {"x": 1}, "b": 2, "c": 3}')
#' x2 = c('{"a": {"x": 11, "y": 22}, "s": 12, "t": [1, 2, 3]}')
#' x <- c(x1, x2)
#' json_paths(x)
#'
#' json_paths1(x1)
json_paths <- function(x) {
  # TODO this is annoying for arrays
  r <- parse_json_vector(jq_do(x, "[paths]"))
  # TODO what should be returned for x = NA?
  # r <- lapply(r, `%||%`, character())
  as_list_of(r, .ptype = list_of(.ptype = character()))
}

#' @rdname json_paths
#' @export
json_paths1 <- function(x) {
  check1(x)
  json_paths(x)[[1]]
}


#' Does path exists?
#'
#' @details
#' Postgres
#' * single string: '{"a":1, "b":2}'::jsonb ? 'b'
#' * multiple strings (and): '{"a":1, "b":2, "c":3}'::jsonb ?| array['b', 'c']
#' * multiple strings (or): '{"a":1, "b":2, "c":3}'::jsonb ?| array['b', 'c']
#'
#' jq
#' * single string: jq '{"a":1, "b":2}' | 'has("b")'
#' * multiple strings (and): jq '{"a":1, "b":2}' | 'has("b") and has("c")'
#' * multiple strings (or): jq '{"a":1, "b":2}' | 'has("b") or has("c")'
#'
#' @export
#' @examples
#' json_has_path(x, c("a", "y"))
#' json_has_path(x, list("a", "y"))
#'
#' json_has_paths(x, list(c("a", "x"), c("a", "y")))
#' json_has_all_paths(x, list(c("a", "x"), c("a", "y")))
#' json_has_any_paths(x, list(c("a", "x"), c("a", "y")))
#'
#'
#' json_has_path1(x1, c("a", "y"))
#' json_has_path1(x1, list("a", "y"))
#'
#' json_has_paths1(x1, list(c("a", "x"), c("a", "y")))
#' json_has_all_paths1(x1, list(c("a", "x"), c("a", "y")))
#' json_has_any_paths1(x1, list(c("a", "x"), c("a", "y")))
json_has_paths <- function(x, paths) {
  paths <- purrr::map_chr(paths, vec_as_jq_path)
  path_check_elts <- paste0("[.[] == ", paths, "]", collapse = ", ")
  path_check <- paste0("[", path_check_elts, "]")
  jq_cmd <- c("[paths]", path_check, "map(any)")

  template <- rep_along(paths, NA)
  # TODO use original input as path names?
  purrr::map(
    jq_do(x, jq_cmd),
    ~ setNames(parse_json(.x, na = template), paths)
  )
}

#' @rdname json_has_paths
#' @export
json_has_path <- function(x, path) {
  # TODO check path...
  if (any(lengths(path) > 1)) {
    abort("all path elements must have lenth 1")
  }

  r <- purrr::flatten_lgl(json_has_paths(x, list(path)))
  unname(r)
}

#' @rdname json_has_paths
#' @export
json_has_all_paths <- function(x, paths) {
  purrr::map_lgl(json_has_paths(x, paths), all)
}

#' @rdname json_has_paths
#' @export
json_has_any_paths <- function(x, paths) {
  purrr::map_lgl(json_has_paths(x, paths), any)
}

#' @rdname json_has_paths
#' @export
json_has_path1 <- function(x, path) {
  check1(x)
  json_has_path(x, path)
}

#' @rdname json_has_paths
#' @export
json_has_paths1 <- function(x, paths) {
  check1(x)
  json_has_paths(x, paths)[[1]]
}

#' @rdname json_has_paths
#' @export
json_has_all_paths1 <- function(x, paths) {
  check1(x)
  json_has_all_paths(x, paths)[[1]]
}

#' @rdname json_has_paths
#' @export
json_has_any_paths1 <- function(x, paths) {
  check1(x)
  json_has_any_paths(x, paths)[[1]]
}
