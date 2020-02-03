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
jq_do <- function(x, ...,
                  json2 = TRUE,
                  .na = json_na_error(),
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
    input <- json_nest(input)
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

jq_do2 <- function(x, ...,
                   json2 = TRUE,
                   .na_error = FALSE,
                   slurp = FALSE) {
  if (any(is.na(x)) && is_true(.na_error)) {
    abort("NA discovered")
  }

  input <- x[!is.na(x)]
  if (is_true(slurp)) {
    input <- json_nest(input)
  }
  r <- vec_data(jqr::jq(input, ...))

  if (is_true(json2)) {
    r <- new_json2(r)
  }

  r
}


check1 <- function(x) {
  if (!length(x) == 1) {
    abort("length must be one")
  }
}


#' Nest a JSON array
#'
#' @export
json_nest <- function(x, .na_error = TRUE) {
  if (is_true(.na_error) && any(is.na(x))) {
    abort("x contains NA")
  }
  new_json2(sprintf("[%s]", paste0(x, collapse = ",")))
}


#' Unnest a JSON array
#'
#' @export
json_unnest <- function(x, .na_error = FALSE) {
  # TODO add .id argument?
  jq_do2(x, ".[]", .na_error = .na_error)
}


#' @export
json_sort <- function(x) {
  jq_do(x, flags = jqr::jq_flags(sorted = TRUE), .na = NA_character_)
}


#' @export
json_equal <- function(x, y, na_equal = FALSE) {
  vec_equal(json_sort(x), json_sort(y), na_equal = na_equal)
}
