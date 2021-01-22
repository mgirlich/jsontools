#' Convert \R{} objects to JSON
#' @rdname format_json
#'
#' @description
#' `format_json` is only a wrapper around the great [`jsonlite::toJSON()`].
#' The differences are
#' * expose argument `json_verbatim`, `rownames`, and `always_decimal`.
#' * use `json_verbatim = TRUE` by default so that JSON isn't escaped again.
#' * return a `json2` object.
#'
#' `format_json_list()` converts each element of a list to JSON.
#'
#' To make sure that a length one vector is not turned into an array use
#' [`json_u()`] or [`jsonlite::unbox()`].
#'
#' @param x the object to be encoded
#' @param json_verbatim Leave JSON as it is and do not encode it again?
#' @param rownames For data.frames add a field `_row` with the row name?
#' @param always_decimal Use real number notation in whole number doubles?
#' @param null,na,auto_unbox,dataframe,matrix,Date passed on to [`jsonlite::toJSON`].
#' @param POSIXt,factor,complex,raw,digits,force,pretty,... passed on to [`jsonlite::toJSON`].
#'
#' @seealso [`write_json()`], [`format_json_rowwise()`]
#'
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
                        raw = c("base64", "hex", "mongo", "int", "js"),
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
#' @seealso [`format_json()`], [`format_json_rowwise()`]
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
                             raw = c("base64", "hex", "mongo", "int", "js"),
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
