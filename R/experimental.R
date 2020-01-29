#' Get paths
#'
#' Postgres: doesn't exist
#' jq: `paths(x)`
#'
#' @examples
#' x1 <- c('{"a": {"x": 1}, "b": 2, "c": 3}')
#' x2 <- c('{"a": {"x": 11, "y": 22}, "s": 12, "t": [1, 2, 3]}')
#' x <- c(x1, x2)
#' jsonr_paths(x)
#'
#' jsonr_paths1(x1)
jsonr_paths <- function(x) {
  # TODO this is annoying for arrays
  r <- parse_json_vector(jq_do(x, "[paths]"), .na = NULL)
  as_list_of(r, .ptype = list_of(.ptype = character()))
}

#' @rdname jsonr_paths
jsonr_paths1 <- function(x) {
  check1(x)
  jsonr_paths(x)[[1]]
}
