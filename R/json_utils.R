escape <- function(x) {
  if (is.list(x)) {
    # return(purrr::map_chr(x, escape))
    return(purrr::map(x, escape))
  } else if (is.character(x)) {
    paste0('"', x, '"')
  } else if (is.numeric(x)) {
    # TODO should check whether conversion is valid
    as.integer(x)
  } else {
    abort("cannot escape")
  }
}


jq_do <- function(x, ..., json2 = TRUE) {
  #' Known jqr bugs:
  #' * jqr::jq(c("1", "2")) # returns 12
  #' * jqr::jq(NA_character_) # errors
  #' * jqr::jq(c('{"a": 1}', '2'), ".a") # suppresses error and doesn't return anything for 2nd argument
  # TODO jq_do(c("1", "2"))
  idx <- !is.na(vec_data(x))
  r <- rep_along(x, NA_character_)
  r_jq <- jqr::jq(x[idx], ...)

  if (length(r_jq) != sum(idx)) {
    # TODO jqr doesn't get the error message...
    abort("some error in jq")
  }

  r[idx] <- vec_data(r_jq)

  if (isTRUE(json2)) {
    r <- new_json2(r)
  }

  r
}


check1 <- function(x) {
  # TODO better error message
  if (!length(x) == 1) {
    abort("length must be one")
  }
}


#' Update value
#'
#' json: [{"f1": 1, "f2": null}, 2]
#' Postgres:
#'   jsonb_set(x, '{0,f3}', '3')
#' jq:
#'   key: x | (.[0].f3|=3)
#'   path: setpath([0, "f3"], 3)
#'
#' @export
#' @examples
#' json_set(x, path = "abc", value = "new value")
#' json_set(x, path = c("abc", "def"), value = "new value")
#' json_set(x, path = "new_key", value = "new value")
#' json_set(x, path = "new key", value = "new value")
json_set <- function(x, path, value, create = FALSE) {
  jq_cmd <- jq_set(path, value)
  jq_do(x, jq_cmd)
}

#' Update values
#'
#' @export
#' @examples
#' json_modify(
#'   x,
#'   abc = list(
#'     def = 20,
#'     xyz = 10
#'   ),
#'   new_key = "new value"
#' )
#' # cannot update array element
json_modify <- function(x, ...) {
  # 1) iteratively update via json_set
  # for (i in seq_along(values)) {
  #   x <- json_set(x, path = names(values)[i], value = values[[i]])
  # }
  # x

  dots_json <- jsonlite::toJSON(list(...), auto_unbox = TRUE)
  json_merge(x, dots_json)
}

#' Delete key
#'
#' @export
#' @examples
#' json_delete_path(x, path = "abc")
#' json_delete_path(x, path = c("abc", "def"))
#' json_delete_path(x, path = c("not_there", "def"))
json_delete_path <- function(x, path) {
  # TODO support multiple keys via list(key1, key2, ...)?
  # --> problem: syntax different than for other verbs
  # --> maybe support via dots?
  jq_cmd <- jq_delete(path)
  jq_do(x, jq_cmd)
}

#' Merge two jsons
#'
#' @export
#' @examples
#' something like list_modify and list_merge?
#' json_merge('{"a": 1, "c": 3}', '{"a": 11, "b": 2}')
json_merge <- function(x, y) {
  # TODO support length(y) > 1
  check1(y)
  jq_do(x, glue(". + {y}"))
}
