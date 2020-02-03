# jq help:
# Merge two arrays: https://github.com/stedolan/jq/issues/680

glue_jq <- function(..., .envir = parent.frame()) {
  noquote(glue::glue(..., .envir = .envir))
}


#' @importFrom jqr jq
#' @method jq pq_jsonb
#' @export
#' @export jq.pq_jsonb
jq.pq_jsonb <- function(x, ...) {
  jqr::jq(as.character(x), ...)
}


#' jq_has_key("abc")
#' jq_has_key(c("abc", "Foo"))
jq_has_key <- function(key) {
  if (length(key) != 1) {
    abort("key must have length 1")
  }

  key <- escape(key)
  glue_jq("has({key})")
}


#' jq_has_keys(c("abc", "Foo"))
#' jq_has_keys(c("abc", "Foo"), object = TRUE)
jq_has_keys <- function(keys, object = FALSE) {
  array_elts <- lapply(keys, jq_has_key)
  if (is_true(object)) {
    values <- paste0(escape(keys), ": ", array_elts, collapse = ", ")
    paste0("{", values, "}")
  } else {
    paste0("[", paste0(array_elts, collapse = ", "), "]")
  }
}


jq_set_id <- function(id, value) {
  value <- escape(value)

  if (length(value) == 1) {
    glue_jq("{id} = {value}")
  } else {
    c(
      glue_jq("{json_nest(value)} as $values"),
      "[$values, .]",
      "transpose",
      glue_jq('map((.[0] as $val | .[1] | {id} = $val))')
      # ".[]"
    )
  }
}


jq_delete_id <- function(id) {
  glue_jq("del({id})")
}


jq_validate_path <- function(path) {
  if (length(path) > 1) {
    abort("concatenate multi paths with .")
  }

  if (!startsWith(path, ".")) {
    abort("path must start .")
  }
}
