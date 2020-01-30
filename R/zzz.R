.onLoad <- function(...) {
  s3_register("pillar::pillar_shaft", "json2")
  s3_register("pillar::is_vector_s3", "json2")

  if (is_installed("dbplyr")) {
    assign_in_dbplyr("base_scalar", fixed_base_scalar)
    assign_in_dbplyr("sql_translate_env.PqConnection", fixed_pq_translator)
  }

  invisible()
}
