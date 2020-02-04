# jq help:
# Merge two arrays: https://github.com/stedolan/jq/issues/680

# jqquery class -----------------------------------------------------------

new_jqquery <- function(...) {
  new_vctr(c(...), class = c("jqquery"), inherit_base_type = TRUE)
}

print.jqquery <- function(x, ...) {
  x <- vec_data(x)
  cat(noquote(jq_query_combine(x, sep = "\n")))
}

vec_ptype2.jqquery <- function(x, y, ..., x_arg = "", y_arg = "") {
  UseMethod("vec_ptype2.jqquery", y)
}

vec_ptype2.jqquery.jqquery <- function(x, y, ...) new_jqquery()

vec_ptype2.jqquery.character <- function(x, y, ...) {
  character()
}

vec_ptype2.character.jqquery <- function(x, y, ...) {
  character()
}

vec_cast.jqquery <- function(x, to, ...) UseMethod("vec_cast.jqquery")

vec_cast.jqquery.default <- function(x, to, ...) vec_default_cast(x, to, ...)

vec_cast.jqquery.jqquery <- function(x, to, ...) x

vec_cast.jqquery.character <- function(x, to, ...) {
  new_jqquery(vec_data(x))
}

vec_cast.character.jqquery <- function(x, to, ...) {
  vec_data(x)
}


# jq helpers --------------------------------------------------------------

jq_query_combine <- function(x, sep = " ") {
  sep <- paste0(" |", sep)
  new_jqquery(paste0(x, collapse = sep))
}

glue_jq <- function(..., .envir = parent.frame(), combine = FALSE) {
  r <- new_jqquery(glue::glue(..., .envir = .envir))
  if (is_true(combine)) {
    r <- jq_query_combine(r)
  }

  r
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
    jq_query <- paste0("{", values, "}")
  } else {
    jq_query <- paste0("[", paste0(array_elts, collapse = ", "), "]")
  }

  new_jqquery(jq_query)
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
