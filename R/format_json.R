#' Convert \R{} objects to JSON
#' @rdname format_json
#'
#' @description
#' These functions are used to convert between JSON data and \R{} objects. The [`jsonlite::toJSON`] and [`jsonlite::fromJSON`] functions use a class based mapping, which follows conventions outlined in this [paper](https://arxiv.org/abs/1403.2805) (also available as vignette).
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
#' # null
#' x_null <- list(a = NULL, b = 1)
#' format_json(x_null)
#' format_json(x_null, null = "null")
#'
#' # na
#' x_na <- list(a = NA, b = 1)
#' format_json(x_na)
#' format_json(x_na, na = "string")
#'
#' # auto_unbox
#' x_autounbox <- list(1, 1:3)
#' format_json(x_autounbox)
#' format_json(x_autounbox, auto_unbox = TRUE)
#'
#' # dataframe conversion
#' x_df <- iris[1:2, ]
#' format_json(x_df, pretty = TRUE)
#' format_json(x_df, dataframe = "columns", pretty = TRUE)
#' format_json(x_df, dataframe = "values", pretty = TRUE)
#'
#' # json_verbatim
#' x_json <- json2('["a","b"]')
#' format_json(x_json)
#' format_json(x_json, json_verbatim = FALSE)
#'
#' # Decimal vs significant digits
#' x <- 10 + pi
#' format_json(x)
#' format_json(x, digits = NA)
#' format_json(x, digits = 2)
#' format_json(x, digits = I(2))
#'
#' # Force decimal representation
#' format_json(12)
#' format_json(12, always_decimal = TRUE)
format_json <- function(x,
                        null = c("list", "null"),
                        na = c("null", "string"),
                        auto_unbox = FALSE,
                        dataframe = c("rows", "columns", "values"),
                        matrix = c("rowmajor", "columnmajor"),
                        Date = c("ISO8601", "epoch"),
                        POSIXt = c("string", "ISO8601", "epoch", "mongo"),
                        factor = c("string", "integer"),
                        complex = c("string", "list"),
                        raw = c("base64", "hex", "mongo"),
                        digits = 4,
                        json_verbatim = TRUE,
                        force = FALSE,
                        pretty = FALSE,
                        rownames = FALSE,
                        always_decimal = FALSE,
                        ...) {
  # * time_format = "%Y-%m-%dT%H:%M:%SZ" (for UTC)
  #               = "%Y-%m-%dT%H:%M:%S" (for non UTC)
  #               = "" (for POSIXt == "string")
  # * UTC

  # TODO this should warn/error for partially named lists

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
  ) %>%
    unclass() %>%
    new_json2()
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


#' @export
#' @rdname format_json
format_json_list <- function(x,
                             null = c("list", "null"),
                             na = c("null", "string"),
                             auto_unbox = FALSE,
                             dataframe = c("rows", "columns", "values"),
                             matrix = c("rowmajor", "columnmajor"),
                             Date = c("ISO8601", "epoch"),
                             POSIXt = c("string", "ISO8601", "epoch", "mongo"),
                             factor = c("string", "integer"),
                             complex = c("string", "list"),
                             raw = c("base64", "hex", "mongo"),
                             digits = 4,
                             json_verbatim = TRUE,
                             force = FALSE,
                             pretty = FALSE,
                             rownames = FALSE,
                             always_decimal = FALSE,
                             ...) {
  if (!is.list(x)) {
    abort(c(
      "only works with list.",
      i = "Did you want to use `format_json_rowwise()` instead?"
    ))
  }

  if (!missing(na)) {
    na <- match.arg(na)
  } else {
    na <- NULL
  }

  purrr::map(
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
    purrr::flatten_chr() %>%
    new_json2()
}
