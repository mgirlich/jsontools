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
#' jsonr_has_path(x, ".a.y")
#'
#' jsonr_has_paths(x, c(".a.x", ".a.y"))
#' jsonr_has_all_paths(x, c(".a.x", ".a.y"))
#' jsonr_has_any_paths(x, c(".a.x", ".a.y"))
#'
#'
#' jsonr_has_path1(x1, ".a.y")
#'
#' jsonr_has_paths1(x1, c(".a.x", ".a.y"))
#' jsonr_has_all_paths1(x1, c(".a.x", ".a.y"))
#' jsonr_has_any_paths1(x1, c(".a.x", ".a.y"))
jsonr_has_paths <- function(x, paths) {
  # convert paths to index notation: https://github.com/stedolan/jq/issues/1949
  # strategy:
  # 1. get all paths
  # 2. convert all paths to index notation
  # 3. build object that checks for each index path if it exists in array

  pathexpr <- 'map(
    if type == "number" then
      "[\\(tostring)]"
    else
      "."+.
    end
  ) | join("")'
  # alternative: convert index notation to paths via path

  jq_get_path_as_index <- paste0("[paths] | map(", pathexpr, ")")
  path_check_elts <- glue_jq('"{paths}": ([.[] == "{paths}"] | any)')

  path_check <- paste0("{", paste0(path_check_elts, collapse = ", "), "}")
  jq_cmd <- c(jq_get_path_as_index, path_check)

  template <- as.list(set_names(rep_along(paths, NA), paths))
  r_jq <- jq_do(x, jq_cmd, .na = NA_character_)
  r <- parse_json_vector(r_jq, .na = template)
  dplyr::bind_rows(r)
}

#' @rdname jsonr_has_paths
#' @export
jsonr_has_path <- function(x, path) {
  if (any(lengths(path) > 1)) {
    abort("all path elements must have length 1")
  }

  jsonr_has_paths(x, path)[[path]]
}

#' @rdname jsonr_has_paths
#' @export
jsonr_has_all_paths <- function(x, paths) {
  row_all(jsonr_has_paths(x, paths))
}

#' @rdname jsonr_has_paths
#' @export
jsonr_has_any_paths <- function(x, paths) {
  row_any(jsonr_has_paths(x, paths))
}

#' @rdname jsonr_has_paths
#' @export
jsonr_has_path1 <- function(x, path) {
  check1(x)
  jsonr_has_path(x, path)
}

#' @rdname jsonr_has_paths
#' @export
jsonr_has_paths1 <- function(x, paths) {
  check1(x)
  unlist(jsonr_has_paths(x, paths))
}

#' @rdname jsonr_has_paths
#' @export
jsonr_has_all_paths1 <- function(x, paths) {
  check1(x)
  jsonr_has_all_paths(x, paths)
}

#' @rdname jsonr_has_paths
#' @export
jsonr_has_any_paths1 <- function(x, paths) {
  check1(x)
  jsonr_has_any_paths(x, paths)
}
