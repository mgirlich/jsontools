assign_in_dbplyr <- function(name, f) {
  # hack: need to assign sql_translate_env here so that assignInNamespace doesn't complain...
  sql_translate_env <- function(con) {
    UseMethod("sql_translate_env")
  }

  ns <- getNamespace("dbplyr")
  unlockBinding(name, ns)
  # assign(name, f, envir = ns)
  # assignInNamespace(name, f, ns = ns)
  assignInNamespace(name, f, ns = "dbplyr")
  lockBinding(name, ns)
}


# x <- c(".a", ".a.b", '.["a b"].[2]')
jq_index_to_pg_path <- function(path) {
  jq_validate_path(path)
  # \.(\w|\[".+?"\]|\[\d+])
  matches1 <- stringr::str_match_all(path, '\\.(\\w+|\\[".+?"\\]|\\[\\d+?])')[[1]]
  matches2 <- stringr::str_match(matches1[, 2], '^\\[?\\"?(.+?)\\"?\\]?$')[, 2]
  paste0("{", paste0(matches2, collapse = ","), "}")
}

postgres_translator <- dbplyr:::sql_translate_env.PqConnection()

# update translators
scalar_new <- postgres_translator$scalar
aggregate_new <- postgres_translator$aggregate
window_new <- postgres_translator$window

fixed_pq_translator <- function(con) {
  scalar_new$jsonr_has_key = function(x, key) {
    dbplyr::build_sql(!!x, " ? ", key)
  }

  scalar_new$jsonr_has_path = function(x, path) {
    dbplyr::translate_sql(!is.na(json_extract(x, path)))
  }

  scalar_new$json_extract = function(x, path) {
    path <- jq_index_to_pg_path(path)
    dbplyr::build_sql("(", !!x, "#>", path, ")")
  }

  scalar_new$jsonr_extract_chr = function(x, path) {
    path <- jq_index_to_pg_path(path)
    dbplyr::build_sql("(", !!x, "#>>", path, ")")
  }

  scalar_new$jsonr_extract_lgl = function(x, path) {
    dbplyr::translate_sql(as.numeric(jsonr_extract_chr(!!x, !!path)))
  }

  scalar_new$jsonr_extract_int = function(x, path) {
    dbplyr::translate_sql(as.integer(jsonr_extract_chr(!!x, !!path)), con = con)
  }

  scalar_new$jsonr_extract_dbl = function(x, path) {
    dbplyr::translate_sql(as.numeric(jsonr_extract_chr(!!x, !!path)))
  }

  scalar_new$jsonr_extract_dat = function(x, path, format = "%Y-%m-%d") {
    dbplyr::translate_sql(TO_DATE(jsonr_extract_chr(!!x, !!path), !!format))
  }

  scalar_new$jsonr_extract_dtt <- function(x, path, format = "%Y-%m-%d %H:%M:%S") {
    dbplyr::translate_sql(TO_TIMESTAMP(jsonr_extract_chr(!!x, !!path), !!format))
  }

  scalar_new$jsonr_extract_lst = function(x, path) {
    stop("not supported")
  }

  dbplyr::sql_variant(
    scalar = scalar_new,
    aggregate = aggregate_new,
    window = window_new
  )
}
