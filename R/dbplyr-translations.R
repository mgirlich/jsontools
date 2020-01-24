if (FALSE) {
  postgres_translator <- dbplyr:::sql_translate_env.PqConnection()

  # update translators
  scalar_new <- tmp$scalar
  aggregate_new <- tmp$aggregate
  window_new <- tmp$window

  # create new variant
  variant <- dbplyr::sql_variant(
    scalar = scalar_new,
    aggregate = aggregate_new,
    window = window_new
  )

  sql_translate_env_new <- function(con) {
    variant
  }

  # apply dbplyr fixes
  environment(fixed_pq_translator) <- asNamespace("dbplyr")
  # hack: need to assign sql_translate_env here so that assignInNamespace doesn't complain...
  sql_translate_env <- function(con) {
    UseMethod("sql_translate_env")
  }
  suppressWarnings({
    assignInNamespace("base_scalar", fixed_base_scalar, ns = "dbplyr")
    assignInNamespace(
      "sql_translate_env.PqConnection",
      fixed_pq_translator,
      ns = "dbplyr"
    )
  })
}
