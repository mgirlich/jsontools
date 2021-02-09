#' Update values
#'
#' @param x A JSON vector.
#' @param ... Name-value pairs. The name is the JSON path (without leading "$").
#'
#' @return A `json2` vector similar to `x` with the components modified as
#'   specified in `...`.
#' @export
#'
#' @examples
#' x_na <- c('{"a": 11, "b": {"x": 12}}', NA, '{"a": 21, "b": {"x": 22}}')
#' # update with different values
#' json_mutate(x_na, .a = 1:3)
#'
#' # NA is translated to null
#' json_mutate(x_na, .a = 1:3, .b.x = NA)
#'
#' # create new keys
#' json_mutate(x_na, .c = 0, .d.x = c("a", "b", "c"))
json_mutate <- function(x, ...) {
  dots <- list(...)
  if (is_empty(dots)) {
    return(x)
  }

  dots <- lapply(dots, escape_value)

  paths <- paste0("$", names(dots))
  val_cols <- paste0("val", seq_along(dots))

  input <- tibble::tibble(
    row_id = seq_along(x),
    data = x,
    !!!set_names(dots, val_cols)
  )

  DBI::dbWriteTable(
    con,
    "my_tbl",
    input,
    overwrite = TRUE
  )

  query <- DBI::SQL("data")
  for (i in seq_along(dots)) {
    if (is_json2(dots[[i]])) {
      value_query <- glue_sql("JSON({`val_cols[[i]]`})", .con = con)
    } else {
      value_query <- glue_sql("{`val_cols[[i]]`}", .con = con)
    }

    query <- glue_sql("
      JSON_SET(
        {query},
        {paths[[i]]},
        {value_query}
      )", .con = con)
  }

  sql <- glue_sql("
    SELECT
      {query} AS result
    FROM my_tbl
  ")

  result <- DBI::dbGetQuery(con, sql)$result

  new_json2(result)
}

#' Merge two JSON objects
#'
#' By merging two objects you can add, modify, or remove elements of an object.
#' Arrays cannot be modified but only replaced as a whole. It is mostly a small
#' wrapper around the SQLite function
#' [`json_patch()`](https://www.sqlite.org/json1.html#jpatch).
#'
#' @param x A JSON vector to update.
#' @param y A JSON vector with updated values.
#'
#' @export
#' @examples
#' # update element with key "a"
#' json_merge('{"a": 1, "c": 3}', '{"a": 11}')
#'
#' # you can also add elements
#' json_merge('{"a": 1, "c": 3}', '{"b": 2}')
#'
#' # remove elements with `null`
#' json_merge('{"a": 1, "c": 3}', '{"c": null}')
json_merge <- function(x, y) {
  if (any(is.na(y) & !is.na(x))) {
    stop_jsontools("`y` can only be NA where `x` is as well.")
  }

  DBI::dbWriteTable(
    con,
    "my_tbl",
    tibble::tibble(x, y),
    overwrite = TRUE
  )

  sql <- "SELECT JSON_PATCH(x, y) AS result FROM my_tbl"

  new_json2(as.character(DBI::dbGetQuery(con, sql)$result))
}


escape_value <- function(x) {
  UseMethod("escape_value")
}

#' @export
escape_value.list <- function(x) {
  # format_json()
  stop_jsontools("list is not supported")
}

#' @export
escape_value.logical <- function(x) {
  new_json2(as.character(ifelse(x, "true", "false")))
}

#' @export
escape_value.default <- function(x) {
  x
}
