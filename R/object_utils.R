#' Creating JSON objects
#'
#' @section Create a JSON object from `R` values:
#'
#' Given some `R` values you can easily create an object with them by putting
#' them into a named list and applying `format_json()`:
#'
#' ```
#' id <- 1
#' x <- 1:3
#' y <- c("a", "b")
#'
#' list(id = json_u(id), x = x, y = y) %>%
#'   format_json()
#'
#' # or in some cases you might be interested in using `dataframe = "columns"`
#' tibble::tibble(
#'   x = 1:3,
#'   y = c("a", "b", "c")
#' ) %>%
#'   format_json(dataframe = "columns")
#' ```
#'
#' To create multiple objects at once (basically a vectorised version) put the
#' values in a data frame and apply `format_json_rowwise()`:
#'
#' ```
#' df <- tibble::tibble(
#'   id = 1,
#'   x = list(1:2, 3:4, 5),
#'   y = c("a", "b", "c")
#' )
#' format_json_rowwise(df)
#' ```
#'
#' @name object
NULL
