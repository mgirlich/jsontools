# glue_jq <- function() {
#
# }
#
#
# jq_transformer <- function() {
#   # convert list/vector to path
#
#   # object construction: named list
#   # {message: .commit.message, name: .commit.committer.name}'
#   # {message: .["commit"].["message"], name: .["commit"].["committer"].["name"]}'
#   x <- list(
#     message = c("commit", "message"),
#     name = c("commit", "committer", "name")
#   )
#
#   # array/string slice? .[10:15]
#
#   # array representation of path
# }


# vec_as_jq_index(c("commit", "committer", "name"))
# vec_as_jq_index(list("commit", "committer", "name", 1:10))
vec_as_jq_index <- function(x) {
  # noquote(paste0('."', x, '"', collapse = ""))
  noquote(paste0('.[', escape(x), "]", collapse = ""))
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


#' @importFrom jqr jq
#' @method jq pq_jsonb
#' @export
#' @export jq.pq_jsonb
jq.pq_jsonb <- function(x, ...) {
  jqr::jq(as.character(x), ...)
}


print.jq_path <- function(x, ...) {
  cat(x)
}


#' jq_get_key("abc")
#' jq_get_key(c("abc", "def"))
#' jq_get_key(list("abc", 2))
jq_get_key <- function(key) {
  path <- vec_as_jq_path(key)
  glue("getpath({path})")
}


#' jq_has_key("abc")
#' jq_has_key(c("abc", "Foo"))
jq_has_key <- function(key) {
  if (length(key) != 1) {
    abort("key must have length 1")
  }

  key <- escape(key)
  glue('has({key})')
}


#' jq_has_keys(c("abc", "Foo"))
jq_has_keys <- function(keys) {
  # TODO could return object instead?
  array_elts <- lapply(keys, jq_has_key)
  paste0("[", paste0(array_elts, collapse = ", "), "]")
}


#' jq_set("abc", 1)
#' jq_set(c("abc", "def"), 1)
jq_set <- function(key, value) {
  path <- vec_as_jq_path(key)
  value <- escape(value)
  glue("setpath({path}; {value})")
}


jq_delete <- function(key) {
  path <- vec_as_jq_path(key)
  glue("delpaths([{path}])")
}
