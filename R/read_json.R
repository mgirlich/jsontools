#' Convert JSON to an \R{} object
#'
#' @param x a scalar JSON character
#' @param simplifyVector coerce JSON arrays containing only primitives into an atomic vector
#' @param simplifyDataFrame coerce JSON arrays containing only records (JSON objects) into a data frame
#' @param simplifyMatrix coerce JSON arrays containing vectors of equal mode and dimension into matrix or array
#' @param flatten automatically \code{\link{flatten}} nested data frames into a single non-nested data frame
#' @param bigint_as_char Parse big ints as character?
#' @param .na Value to return if `x` is `NA`. By default an error of class
#' `jsontools_error_na_json` is thrown.
#' @param .null Return the prototype of `.null` if `x` is `NULL`
#'   or a zero length character
#' @param ... arguments passed on to [`jsonlite::parse_json`]
#'
#' @export
#' @examples
#' # Parse escaped unicode
#' parse_json('{"city" : "Z\\u00FCrich"}')
#'
#' # big integers
#' big_num <- "9007199254740993"
#' as.character(parse_json(big_num, bigint_as_char = FALSE))
#' as.character(parse_json(big_num, bigint_as_char = TRUE))
#'
#' # NA error by default
#' parse_json(NA)
#' # ... but one can specify a default value
#' parse_json(NA, .na = data.frame(a = 1, b = 2))
#'
#' # input of size 0
#' parse_json(NULL)
#' parse_json(character(), .null = data.frame(a = 1, b = 2))
parse_json <- function(x, simplifyVector = TRUE, simplifyDataFrame = FALSE,
                       simplifyMatrix = FALSE, flatten = FALSE,
                       bigint_as_char = TRUE, .na = json_na_error(),
                       .null = NULL,
                       ...) {
  if (is_null(x) || (is_character(x) && vec_size(x) == 0)) {
    return(vec_ptype(.null))
  }

  if (is.na(x)) {
    return(.na)
  }

  if (length(x) > 1 || !is_character(x)) {
    abort("x must be a scalar character")
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
#' parse_json_vector(x = c('"a"', NA), .na = 1)
parse_json_vector <- function(x, simplifyVector = TRUE, simplifyDataFrame = FALSE,
                              simplifyMatrix = FALSE, flatten = FALSE,
                              bigint_as_char = TRUE, .na = json_na_error(),
                              .null = NULL,
                              ...) {
  lapply(
    x,
    parse_json,
    simplifyVector = simplifyVector,
    simplifyDataFrame = simplifyDataFrame,
    simplifyMatrix = simplifyMatrix,
    flatten = flatten,
    bigint_as_char = bigint_as_char,
    .na = .na,
    .null = .null,
    ...
  )
}


json_na_error <- function() {
  stop_jsontools("na_json", message = "input is NA.\nUse argument .na to specify a default value.")
}
