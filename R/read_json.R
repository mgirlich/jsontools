#' Convert JSON to an \R{} object
#'
#' A wrapper around the great [`jsonlite::parse_json`]. The differences are:
#' * expose argument `bigint_as_char` with default `TRUE`.
#' * control how to handle `NA` and `NULL`.
#' * `simplifyDataFrame`, `simplifyMatrix`, and `flatten` default to `FALSE` as
#'   they are not very stable in many real world APIs. Use the
#'   [tibblify package](https://cran.r-project.org/package=tibblify)
#'   for a more robust conversion to a dataframe.
#' * don't collapse strings but error instead if they have more than one element.
#'
#' To parse a vector of JSON use [`parse_json_vector`].
#'
#' @param x a scalar JSON character
#' @param simplifyVector,simplifyDataFrame,simplifyMatrix,flatten,... passed on
#'   to [`jsonlite::parse_json`].
#' @param bigint_as_char Parse big integers as character? The option
#'   `jsontools.bigint_as_char` is used as default.
#' @param .na Value to return if `x` is `NA`. By default an error of class
#' `jsontools_error_na_json` is thrown.
#' @param .null Return the prototype of `.null` if `x` is `NULL`
#'   or a zero length character
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
                       .na = json_na_error(),
                       .null = NULL,
                       simplifyVector = TRUE,
                       simplifyDataFrame = FALSE,
                       simplifyMatrix = FALSE,
                       flatten = FALSE,
                       bigint_as_char = bigint_default(),
                       ...) {
  x <- vec_cast(x, character())
  if (is_null(x) || (is_character(x) && vec_size(x) == 0)) {
    return(vec_ptype(.null))
  }

  if (length(x) > 1 || !is_character(x)) {
    stop_jsontools("`x` must be a scalar character")
  }

  if (is.na(x)) {
    return(.na)
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

# nocov start
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
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message <- paste0(
      "Required package ", pkg,
      " not found. Please run: install.packages('", pkg, "')"
    )

    stop_jsontools(message)
  }
}
# nocov end

#' Parse a vector of JSON into a list
#'
#' @inheritParams parse_json
#'
#' @return A list of the same length as `x`.
#'
#' @export
#' @examples
#' parse_json_vector(x = c('"a"', '"b"'))
#' parse_json_vector(x = c('"a"', '["b", "c"]'))
#' parse_json_vector(x = c('"a"', NA), .na = 1)
parse_json_vector <- function(x,
                              .na = json_na_error(),
                              .null = NULL,
                              simplifyVector = TRUE,
                              simplifyDataFrame = FALSE,
                              simplifyMatrix = FALSE,
                              flatten = FALSE,
                              bigint_as_char = bigint_default(),
                              ...) {
  lapply(
    x,
    parse_json,
    .na = .na,
    .null = .null,
    simplifyVector = simplifyVector,
    simplifyDataFrame = simplifyDataFrame,
    simplifyMatrix = simplifyMatrix,
    flatten = flatten,
    bigint_as_char = bigint_as_char,
    ...
  )
}


json_na_error <- function() {
  stop_jsontools(
    message = "input is NA.\nTo use a default value use the argument `.na`.",
    error_type = "na_json"
  )
}
