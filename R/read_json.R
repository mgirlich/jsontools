#' Convert JSON to an \R{} object
#'
#' @param x a scalar JSON character
#' @param simplifyVector coerce JSON arrays containing only primitives into an atomic vector
#' @param simplifyDataFrame coerce JSON arrays containing only records (JSON objects) into a data frame
#' @param simplifyMatrix coerce JSON arrays containing vectors of equal mode and dimension into matrix or array
#' @param flatten automatically \code{\link{flatten}} nested data frames into a single non-nested data frame
#' @param bigint_as_char Parse big ints as character?
#' @param na Value to return `x` is `NA`
#' @param ... arguments passed on to [`jsonlite::parse_json`]
#'
#' @export
parse_json <- function(x, simplifyVector = TRUE, simplifyDataFrame = FALSE,
                       simplifyMatrix = FALSE, flatten = FALSE,
                       bigint_as_char = TRUE, na = list(), ...) {
  # TODO use list_parse and get rid of simplifyVector, ...?
  # TODO add argument spec
  # TODO flag to sort before parsing?
  if (vec_size(x) == 0) {
    # TODO should this return na?
    return(NULL)
  }

  if (length(x) > 1 || !is.character(x)) {
    abort("x must be a scalar character")
  }

  if (is.na(x)) {
    # TODO return parse result according to spec
    return(na)
  }

  jsonlite::parse_json(
    x,
    simplifyVector = simplifyVector,
    simplifyDataFrame = simplifyDataFrame,
    simplifyMatrix = simplifyMatrix,
    flatten = flatten,
    bigint_as_char = bigint_as_char,
    ...
  )
}

from_json <- parse_json

#' Read JSON from disk or url
#'
#' @param path Path or connection to read from
#' @param ... arguments passed on to [`parse_json`]
read_json <- function(path, ...) {
  if (!is.character(txt) && !inherits(txt, "connection")) {
    stop("Argument 'path' must be a path, URL or file.")
  }

  if (is.character(txt) && length(txt) == 1 && nchar(txt, type = "bytes") <
    2084 && !validate(txt)) {
    if (grepl("^https?://", txt, useBytes = TRUE)) {
      loadpkg("curl")
      h <- curl::new_handle(useragent = paste(
        "jsonlite /",
        R.version.string
      ))
      curl::handle_setheaders(h, Accept = "application/json, text/*, */*")
      txt <- curl::curl(txt, handle = h)
    }
    else if (file.exists(txt)) {
      txt <- file(txt)
    }
  } else {
    stop("not a path")
  }

  parse_json(txt, ...)
}


#' Parse a vector of jsons into a list
#'
#' @export
#' @examples
#' parse_json_vector(x = c('"a"', '"b"'))
#' parse_json_vector(x = c('"a"', '["b", "c"]'))
parse_json_vector <- function(x, simplifyVector = TRUE, simplifyDataFrame = FALSE,
                              simplifyMatrix = FALSE, flatten = FALSE,
                              bigint_as_char = TRUE, na = list(), ...) {
  # TODO see from_json
  lapply(
    x,
    parse_json,
    simplifyVector = simplifyVector,
    simplifyDataFrame = simplifyDataFrame,
    simplifyMatrix = simplifyMatrix,
    flatten = flatten,
    bigint_as_char = bigint_as_char,
    na = na,
    ...
  )
}
