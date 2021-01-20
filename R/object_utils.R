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

#' Create a JSON object from JSON
#'
#' @param x Input JSON.
#' @param ... Object specification
#'
#' @return A json2
#' @export
#' @examples
#' json_object_extract('{"a": 1, "b": 2}', y = .a)
json_object_extract <- function(x, ...) {
  # TODO
  # * feels a bit like `dplyr::transmute()`
  # * vector of json-objects <-> tibble?

  # * class json_path? -> use extract
  # * use AsIs class for actual data?

  dots <- enquos(...)
  keys <- names(dots)

  if (is_empty(dots)) {
    return(new_json2())
  }

  tibble_vals <- list()

  values_sql <- purrr::map(
    dots,
    function(x) {
      if (is_symbol(quo_get_expr(x))) {
        x_name <- paste0("$", as_name(x))
        glue_sql("JSON_EXTRACT(data, {x_name})", .con = con)
      } else {
        x <- eval_bare(quo_get_expr(x))
        if (length(x) > 1) {
          nm <- rand_string(12)
          tibble_vals[[nm]] <<- x
          DBI::dbQuoteIdentifier(con, nm)
        } else {
          glue_sql("{x}", .con = con)
        }
      }
    }
  )

  body <- glue_sql(
    "{keys}, {values_sql}",
    .con = con
  ) %>%
    paste0(collapse = ",\n") %>%
    DBI::SQL()

  sql <- glue_sql("
    SELECT
      JSON_OBJECT(
        {body}
      ) AS result
    FROM my_tbl", .con = con)

  write_json_tbl(x, !!!tibble_vals)
  exec_sqlite_json(sql)$result %>%
    new_json2()
}


rand_string <- function(n) {
  paste0(sample(letters, size = n, replace = TRUE), collapse = "")
}

#' Create R objects from values
#'
#' A simple wrapper around the `format_json_rowwise(tibble(...))`
#'
#' @param ...
#'
#' @return
#' @export
#'
#' #' @examples
#' format_json_rowwise(tibble(
#' json_object_create(
#'
#' json_object_create(
#'   x = 1:3,
#'   y = letters[1:3]
#' )
#'
#' if (require("dplyr", quietly = TRUE, warn.conflicts = FALSE)) {
#'   iris %>%
#'     dplyr::mutate(json = json_object_create(Species, Sepal.Length))
#' }
json_object_create <- function(...) {
  tibble(...) %>%
    format_json_rowwise()
}
