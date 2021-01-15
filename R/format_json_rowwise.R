#' Convert each tbl row to JSON
#'
#' Convert each row of a data frame to a JSON object (this basically produces
#' [ndjson](http://ndjson.org/)).
#'
#' @details Under the hood [`jsonlite::stream_out`] is used for the conversion.
#'
#' @param df A dataframe.
#' @param null,na,auto_unbox,dataframe,matrix,Date passed on to [`jsonlite::toJSON`].
#' @param POSIXt,factor,complex,raw,digits,force,pretty,... passed on to [`jsonlite::toJSON`].
#' @inheritParams format_json
#'
#' @return A json2 vector of json objects.
#'
#' @seealso [`write_json()`], [`format_json()`]
#'
#' @export
#' @examples
#' format_json_rowwise(mtcars[1:3, ])
#'
#' # use a dataframe column for nested objects
#' df <- data.frame(x = 1:2)
#' df$y <- data.frame(z = c("a", "b"))
#' format_json_rowwise(df)
#'
#' if (require("dplyr", quietly = TRUE, warn.conflicts = FALSE)) {
#'   # often useful in mutate/transmute
#'   mtcars %>%
#'     transmute(json = format_json_rowwise(tibble(mpg, cyl, extra = tibble(disp, hp))))
#' }
format_json_rowwise <- function(df,
                                null = c("list", "null"),
                                na = c("null", "string"),
                                auto_unbox = TRUE,
                                matrix = c("rowmajor", "columnmajor"),
                                Date = c("ISO8601", "epoch"),
                                POSIXt = c("string", "ISO8601", "epoch", "mongo"),
                                factor = c("string", "integer"),
                                complex = c("string", "list"),
                                raw = c("base64", "hex", "mongo"),
                                digits = 4,
                                json_verbatim = TRUE,
                                force = FALSE,
                                rownames = FALSE,
                                ...) {
  if (!is.data.frame(df)) {
    abort("x must be a tbl")
  }

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

  # necessary until vctrs is fixed
  list_of_flag <- vapply(df, inherits, "vctrs_list_of", FUN.VALUE = logical(1))

  df[list_of_flag] <- lapply(df[list_of_flag], as.list)

  # needed for input from jsonlite::toJSON so that it stays a json after subsetting
  `[.json` <- function(x, i) {
    structure(NextMethod("["), class = c("json", "character"))
  }

  tmp_file <- NULL
  tmp_file <- withr::local_tempfile()
  textcon <- withr::local_connection(file(tmp_file, "a+"))
  jsonlite::stream_out(
    df,
    con = textcon,
    verbose = FALSE,
    null = null, na = na, auto_unbox = auto_unbox,
    matrix = matrix,
    Date = Date, POSIXt = POSIXt, factor = factor,
    complex = complex, raw = raw,
    digits = digits, json_verbatim = json_verbatim,
    force = force, rownames = rownames
  )

  as_json2(readLines(textcon))
}
