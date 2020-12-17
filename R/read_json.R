#' Convert JSON to an \R{} object
#'
#' @param x a scalar JSON character
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
#' try(parse_json(NA))
#' # ... but one can specify a default value
#' parse_json(NA, .na = data.frame(a = 1, b = 2))
#'
#' # input of size 0
#' parse_json(NULL)
#' parse_json(character(), .null = data.frame(a = 1, b = 2))
parse_json <- function(x,
                       flatten = FALSE,
                       bigint_as_char = TRUE,
                       .na = json_na_error(),
                       .null = NULL,
                       ...) {
  if (is_null(x) || (is_character(x) && vec_size(x) == 0)) {
    return(vec_ptype(.null))
  }

  if (is.na(x)) {
    return(.na)
  }

  if (length(x) > 1 || !is_character(x)) {
    stop_jsontools("`x` must be a scalar character")
  }

  jsonlite::parse_json(
    x,
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE,
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
    stop_jsontools("Argument 'path' must be a path, URL or file.")
  }

  if (is.character(txt) && length(txt) == 1 && nchar(txt, type = "bytes") <
    2084 && !json_is_valid(txt)) {
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
    stop_jsontools("not a path")
  }

  parse_json(txt, ...)
}


loadpkg <- function(pkg) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    message <- paste0(
      "Required package ", pkg,
      " not found. Please run: install.packages('", pkg, "')"
    )

    stop_jsontools(message)
  }
}


#' Parse a vector of jsons into a list
#'
#' @inheritParams parse_json
#'
#' @export
#' @examples
#' parse_json_vector(x = c('"a"', '"b"'))
#' parse_json_vector(x = c('"a"', '["b", "c"]'))
#' parse_json_vector(x = c('"a"', NA), .na = 1)
parse_json_vector <- function(x,
                              flatten = FALSE,
                              bigint_as_char = TRUE,
                              .na = json_na_error(),
                              .null = NULL,
                              ...) {
  r <- lapply(
    x,
    parse_json,
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE,
    flatten = flatten,
    bigint_as_char = bigint_as_char,
    .na = .na,
    .null = .null,
    ...
  )

  r
}


json_na_error <- function() {
  stop_jsontools(
    message = "input is NA.\nTo use a default value use the argument `.na`.",
    error_type = "na_json"
  )
}
