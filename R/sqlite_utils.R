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
