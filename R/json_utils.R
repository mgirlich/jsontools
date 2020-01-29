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


#' Execute jq command
#'
#' @examples
#' jq_do('{"a": 1}', ".a")
#' jq_do(c('{"a": 1}', '{"a": 2}'), ".a")
#' jq_do(NA)
#' jq_do(NA, .na = NA)
jq_do <- function(x, ..., json2 = TRUE, .na = json_na_error(),
                  slurp = FALSE) {
  # workaround for NA handling of jqr
  # https://github.com/ropensci/jqr/issues/78
  na_flag <- is.na(vec_data(x))
  r <- rep_along(x, NA_character_)

  # make sure that all NA input can be handled
  if (all(na_flag)) {
    x <- as.character(x)
  }

  input <- x[!na_flag]
  if (is_true(slurp)) {
    input <- jq_slurp(input)
  }
  r_jq <- jqr::jq(input, ...)

  if (any(na_flag)) {
    check1(.na)
    r[!na_flag] <- .na
  }

  # workaround for jqr bug
  # https://github.com/ropensci/jqr/issues/80
  if (length(r_jq) != sum(!na_flag)) {
    abort("some error in jq")
  }

  r[!na_flag] <- vec_data(r_jq)

  if (isTRUE(json2)) {
    r <- new_json2(r)
  }

  r
}


check1 <- function(x) {
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
#' json_set(x, id = ".abc", value = "new value")
#' json_set(x, id = ".a.x", value = "new value")
#' json_set(x, id = '.["new key"]', value = "new value")
json_set <- function(x, id, value) {
  if (length(value) > 1 && (length(x) != length(value))) {
    stop_incompatible_size(
      x, value,
      length(x), length(value),
      x_arg = "x",
      y_arg = "value"
    )
  }

  jq_cmd <- jq_set_id(id, value)
  jq_do(x, jq_cmd, slurp = length(value) > 1, .na = NA_character_)
}

#' json_set_path(x, path = "abc", value = "new value")
#' json_set_path(x, path = c("abc", "def"), value = "new value")
#' json_set_path(x, path = "new_key", value = "new value")
#' json_set_path(x, path = "new key", value = "new value")
json_set_path <- function(x, path, value, create = FALSE) {
  jq_cmd <- jq_set_path(path, value)
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


#' @export
json_sort <- function(x) {
  jq_do(x, flags = jqr::jq_flags(sorted = TRUE), .na = NA_character_)
}


#' @export
json_equal <- function(x, y, na_equal = FALSE) {
  vec_equal(json_sort(x), json_sort(y), na_equal = na_equal)
}
