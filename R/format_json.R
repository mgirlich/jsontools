#' Convert \R{} objects to JSON
#' @rdname format_json
#'
#' @description
#' These functions are used to convert between JSON data and \R{} objects. The [`jsonlite::toJSON`] and [`jsonlite::fromJSON`] functions use a class based mapping, which follows conventions outlined in this paper: [https://arxiv.org/abs/1403.2805] (also available as vignette).
#'
#' @details
#' The \code{\link{toJSON}} and \code{\link{fromJSON}} functions are drop-in replacements for the identically named functions
#' in packages \code{rjson} and \code{RJSONIO}. Our implementation uses an alternative, somewhat more consistent mapping
#' between \R{} objects and JSON strings.
#'
#' The \code{\link{serializeJSON}} and \code{\link{unserializeJSON}} functions in this package use an
#' alternative system to convert between \R{} objects and JSON, which supports more classes but is much more verbose.
#'
#' A JSON string is always unicode, using \code{UTF-8} by default, hence there is usually no need to escape any characters.
#' However, the JSON format does support escaping of unicode characters, which are encoded using a backslash followed by
#' a lower case \code{"u"} and 4 hex characters, for example: \code{"Z\\u00FCrich"}. The \code{fromJSON} function
#' will parse such escape sequences but it is usually preferable to encode unicode characters in JSON using native
#' \code{UTF-8} rather than escape sequences.

#' @param x the object to be encoded
#' @param null how to encode NULL values within a list: must be one of 'null' or 'list'
#' @param na how to print NA values: must be one of 'null' or 'string'. Defaults are class specific
#' @param auto_unbox automatically \code{\link{unbox}} all atomic vectors of length 1. It is usually safer to avoid this and instead use the \code{\link{unbox}} function to unbox individual elements.
#'   An exception is that objects of class \code{AsIs} (i.e. wrapped in \code{I()}) are not automatically unboxed. This is a way to mark single values as length-1 arrays.
#' @param dataframe how to encode data.frame objects: must be one of 'rows', 'columns' or 'values'
#' @param matrix how to encode matrices and higher dimensional arrays: must be one of 'rowmajor' or 'columnmajor'.
#' @param Date how to encode Date objects: must be one of 'ISO8601' or 'epoch'
#' @param POSIXt how to encode POSIXt (datetime) objects: must be one of 'string', 'ISO8601', 'epoch' or 'mongo'
#' @param factor how to encode factor objects: must be one of 'string' or 'integer'
#' @param complex how to encode complex numbers: must be one of 'string' or 'list'
#' @param raw how to encode raw objects: must be one of 'base64', 'hex' or 'mongo'
#' @param digits max number of decimal digits to print for numeric values. Use \code{I()} to specify significant digits. Use \code{NA} for max precision.
#' @param json_verbatim Leave json as it is and do not encode it again?
#' @param force unclass/skip objects of classes with no defined JSON mapping
#' @param pretty adds indentation whitespace to JSON output. Can be TRUE/FALSE or a number specifying the number of spaces to indent. See \code{\link{prettify}}
#' @param rownames For data.frames add a field `_row` with the row name?
#' @param always_decimal Use real number notation in whole number doubles?
#' @param ... arguments passed on to class specific \code{print} methods
#' @references Jeroen Ooms (2014). The \code{jsonlite} Package: A Practical and Consistent Mapping Between JSON Data and \R{} Objects. \emph{arXiv:1403.2805}. \url{https://arxiv.org/abs/1403.2805}
#' @export
#' @examples
#' # Stringify some data
#' cat(format_json(mtcars, pretty = TRUE))
#'
#' # Decimal vs significant digits
#' format_json(pi, digits = 3)
#' format_json(pi, digits = I(3))
format_json <- function(x,
                        null = c("list", "null"),
                        na = c("null", "string"), auto_unbox = FALSE,
                        dataframe = c("rows", "columns", "values"),
                        matrix = c("rowmajor", "columnmajor"),
                        Date = c("ISO8601", "epoch"),
                        POSIXt = c("string", "ISO8601", "epoch", "mongo"),
                        factor = c("string", "integer"), complex = c("string", "list"),
                        raw = c("base64", "hex", "mongo"),
                        digits = 4, json_verbatim = TRUE, force = FALSE,
                        pretty = FALSE, rownames = FALSE, always_decimal = FALSE, ...) {
  # * time_format = "%Y-%m-%dT%H:%M:%SZ" (for UTC)
  #               = "%Y-%m-%dT%H:%M:%S" (for non UTC)
  #               = "" (for POSIXt == "string")
  # * UTC

  dataframe <- match.arg(dataframe)
  matrix <- match.arg(matrix)
  Date <- match.arg(Date)
  POSIXt <- match.arg(POSIXt)
  factor <- match.arg(factor)
  complex <- match.arg(complex)
  raw <- match.arg(raw)
  null <- match.arg(null)
  if (!missing(na)) {
    na <- match.arg(na)
  } else {
    na <- NULL
  }

  # TODO what about documentation? Copy manually from jsonlite::toJSON?
  # TODO what about new arguments? enough when passed on to ...?
  # TODO should return json2
  jsonlite::toJSON(
    x,
    null = null, na = na, auto_unbox = auto_unbox,
    dataframe = dataframe, matrix = matrix,
    Date = Date, POSIXt = POSIXt, factor = factor,
    complex = complex, raw = raw,
    digits = digits, json_verbatim = json_verbatim,
    force = force, pretty = pretty, rownames = rownames,
    always_decimal = always_decimal,
    ...
  )
}

to_json <- format_json

#' Write JSON
#'
#' @param x An object to write to disk.
#' @param path Path or connection to write to.
#' @param ... arguments passed on to [`format_json`]
#'
#' @export
write_json <- function(x, path, ...) {
  json <- format_json(x, ...)
  writeLines(json, path, useBytes = TRUE)
}


#' Convert `R` objects to/from JSON
#'
#' @export
#' @examples
#' format_json_vector(letters)
#' format_json_vector(list(list(a = 1), list(a = 1, b = 2)))
format_json_vector <- function(x, null = c("list", "null"),
                               na = c("null", "string"), auto_unbox = TRUE,
                               dataframe = c("rows", "columns", "values"),
                               matrix = c("rowmajor", "columnmajor"),
                               Date = c("ISO8601", "epoch"),
                               POSIXt = c("string", "ISO8601", "epoch", "mongo"),
                               factor = c("string", "integer"), complex = c("string", "list"),
                               raw = c("base64", "hex", "mongo"),
                               digits = 4, json_verbatim = TRUE, force = FALSE, ...) {
  if (!is.vector(x)) {
    abort("only works with vectors or list. Did you want to use `format_json_rowwise()` instead?")
  }

  dataframe <- match.arg(dataframe)
  matrix <- match.arg(matrix)
  Date <- match.arg(Date)
  POSIXt <- match.arg(POSIXt)
  factor <- match.arg(factor)
  complex <- match.arg(complex)
  raw <- match.arg(raw)
  null <- match.arg(null)
  if (!missing(na)) {
    na <- match.arg(na)
  } else {
    na <- NULL
  }

  purrr::map_chr(
    x,
    function(elt) {
      format_json(
        elt,
        null = null, na = na, auto_unbox = auto_unbox,
        dataframe = dataframe, matrix = matrix,
        Date = Date, POSIXt = POSIXt, factor = factor,
        complex = complex, raw = raw,
        digits = digits, json_verbatim = json_verbatim,
        force = force
      )
    }
  ) %>%
    as_json2()
}
