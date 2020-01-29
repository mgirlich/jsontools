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


# vec_as_jq_index(c("commit", "committer", "name"))
# vec_as_jq_index(list("commit", "committer", "name", 1:10))
vec_as_jq_index <- function(x) {
  # noquote(paste0('."', x, '"', collapse = ""))
  noquote(paste0(".[", escape(x), "]", collapse = ""))
}

#' jq_path("abc")
#' jq_path(c("abc", "def"))
#' jq_path(list("abc", 2))
vec_as_jq_path <- function(x) {
  x <- escape(x)
  path <- paste0(x, collapse = ",")
  structure(
    glue("[{path}]"),
    class = c("jq_path", "character")
  )
}


print.jq_path <- function(x, ...) {
  cat(x)
}
