#' Build a JSON object
#'
#' @param x Input JSON.
#' @param ... Object specification.
#'
#' @return A json2
#' @export
#' @examples
#' json_build_object('{"a": 1, "b": 2}', y = .a)
json_build_object <- function(x, ...) {
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
