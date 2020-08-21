jsontools_env <- environment()

.onLoad <- function(...) {
  # s3_register("pillar::pillar_shaft", "json2")

  assign("con", DBI::dbConnect(RSQLite::SQLite(), ":memory:"), envir = jsontools_env)
}

.onUnload <- function(...) {
  DBI::dbDisconnect(con)
}
