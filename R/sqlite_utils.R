write_json_tbl <- function(x, ...) {
  DBI::dbWriteTable(
    con,
    "my_tbl",
    tibble::tibble(row_id = seq_along(x), data = x, ...),
    overwrite = TRUE
  )
}

exec_sqlite_json <- function(sql) {
  tibble::as_tibble(DBI::dbGetQuery(con, sql))
}

#' @noRd
#' @examples
#' values <- tibble::tibble(
#'   id = 1:3,
#'   letters = letters[1:3],
#'   json = x
#' )
#' sql_with_vals(values)
sql_with_vals <- function(values) {
  sql_values <- DBI::sqlData(con, values, row.names = FALSE)
  fields <- DBI::dbQuoteIdentifier(con, names(sql_values))

  # Convert fields into a character matrix
  rows <- do.call(paste, c(sql_values, sep = ", "))
  DBI::SQL(paste0(
    "WITH T(", paste(fields, collapse = ", "), ") AS (\n",
    "VALUES\n",
    paste0("  (", rows, ")", collapse = ",\n"),
    ")"
  ))
}
