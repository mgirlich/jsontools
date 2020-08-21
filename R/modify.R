#' Update values
#'
#' @export
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

  result <- suppressWarnings(DBI::dbGetQuery(con, sql)$result)

  new_json2(result)
}

#' Merge two jsons
#'
#' @param x A JSON vector to update.
#' @param y A JSON vector with updated values.
#'
#' @export
#' @examples
#' # something like list_modify and list_merge?
#' json_merge('{"a": 1, "c": 3}', '{"a": 11, "b": 2}')
json_merge <- function(x, y) {
  DBI::dbWriteTable(
    con,
    "my_tbl",
    tibble::tibble(x, y),
    overwrite = TRUE
  )

  sql <- "SELECT JSON_PATCH(x, y) AS result FROM my_tbl"

  new_json2(DBI::dbGetQuery(con, sql)$result)
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
