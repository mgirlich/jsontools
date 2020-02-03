#' Update values
#'
#' @export
json_mutate <- function(x, ...) {
  dots <- list(...)
  jq_cmd <- purrr::imap_chr(
    dots,
    ~ paste(jq_set_id(.y, .x), collapse = " |\n")
  )
  jq_do(x, c(jq_cmd, ".[]"), slurp = any(lengths(dots) > 1), .na = NA_character_)
}

#' Delete key
#'
#' @export
#' @examples
#' json_delete_path(x, id = ".abc")
#' json_delete_path(x, id = ".abc.def")
#' json_delete_path(x, id = ".not_there.def")
json_delete_id <- function(x, id) {
  # TODO support multiple keys via list(key1, key2, ...)?
  # --> problem: syntax different than for other verbs
  # --> maybe support via dots?
  check1(id)
  jq_cmd <- jq_delete_id(id)
  jq_do(x, jq_cmd)
}

#' Merge two jsons
#'
#' @export
#' @examples
#' # something like list_modify and list_merge?
#' json_merge('{"a": 1, "c": 3}', '{"a": 11, "b": 2}')
json_merge <- function(x, y) {
  # TODO support length(y) > 1
  check1(y)
  jq_do(x, glue(". + {y}"))
}
