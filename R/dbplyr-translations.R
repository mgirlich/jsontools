assign_in_dbplyr <- function(f) {
  # hack: need to assign sql_translate_env here so that assignInNamespace doesn't complain...
  if (utils::packageVersion("dbplyr") >= "2.0.0") {
    sql_translation <- function(con) {
      UseMethod("sql_translate_env")
    }

    name <- "sql_translation.PqConnection"
  } else {
    sql_translate_env <- function(con) {
      UseMethod("sql_translate_env")
    }

    name <- "sql_translate_env.PqConnection"
  }

  ns <- getNamespace("dbplyr")
  unlockBinding(name, ns)
  # assign(name, f, envir = ns)
  # assignInNamespace(name, f, ns = ns)
  utils::assignInNamespace(name, f, ns = "dbplyr")
  lockBinding(name, ns)
}


# path <- c("$.a", "$.a.b", '$.["a b"].[2]')
# jsonpath_to_pg_path(path[1])
# jsonpath_to_pg_path(path[2])
# jsonpath_to_pg_path(path[3])
jsonpath_to_pg_path <- function(path) {
  matches1 <- stringr::str_match_all(path, r'{\.(\w+|\[".+?"\]|\[\d+?])}')[[1]]
  matches2 <- stringr::str_match(matches1[, 2], '^\\[?\\"?(.+?)\\"?\\]?$')[, 2]
  paste0("{", paste0(matches2, collapse = ","), "}")
}

# update translators
cast_to_ptype <- function(ptype, sql) {
  # TODO improve this ugly hack
  switch(class(ptype)[[1]],
    integer = dbplyr::translate_sql(as.integer({!!sql})),
    numeric = dbplyr::translate_sql(as.double({!!sql})),
    Date = dbplyr::translate_sql(as.Date({!!sql})),
    POSIXct = dbplyr::translate_sql(as_datetime({!!sql})),
    sql
  )
}

fixed_pq_translator <- function(con) {
  postgres_translator <- dbplyr:::sql_translation.PqConnection()
  scalar_new <- postgres_translator$scalar

  scalar_new$jsonr_has_key = function(x, key) {
    dbplyr::build_sql(!!x, " ? ", key)
  }

  scalar_new$jsonr_has_path = function(x, path) {
    dbplyr::translate_sql(!is.na(json_extract(x, path)))
  }

  scalar_new$json_flatten = function(x, ptype, allow_scalars = FALSE, wrap_scalars = FALSE, bigint_as_char = TRUE) {
    if (is_true(allow_scalars)) {
      flatten_sql <- dbplyr::translate_sql(json_array_elements(x))

      flatten_sql <- dbplyr::translate_sql(
        ifelse(
          json_typeof(x) %in% c("string", "number", "boolean", "null"),
          x,
          !!flatten_sql
        )
      )

      flatten_sql <- dbplyr::translate_sql(as.character(!!flatten_sql))
    } else {
      flatten_sql <- dbplyr::translate_sql(json_array_elements_text(x))
    }

    if (is_true(wrap_scalars)) {
      # TODO wrap_scalars?
      abort("`wrap_scalars` is not supported")
    }

    cast_to_ptype(ptype, flatten_sql)
  }

  scalar_new$json_extract = function(x, path, ptype, default = NULL, na = NA, wrap_scalars = FALSE, bigint_as_char) {
    if (is_json2(ptype)) {
      # extract as JSON
      extract_sql <- dbplyr::translate_sql(x %#>% !!path)
      # could use other function:
      # * `json_extract_path()`: uses a variadic list for the path
      # * `jsonb_path_query()`: uses json path language
      # see: https://www.postgresql.org/docs/current/functions-json.html#FUNCTIONS-JSON-PROCESSING-TABLE
    } else {
      # extract as text
      extract_sql <- dbplyr::translate_sql(x %#>>% !!path)
    }

    if (!is.null(default)) {
      extract_sql <- dbplyr::translate_sql(coalesce(!!extract_sql, !!default))
    }

    if (!is.na(na) && !is.null(default)) {
      extract_sql <- dbplyr::translate_sql(ifelse(
        is.na(x),
        na,
        !!extract_sql
      ))
    }

    if (is_true(wrap_scalars)) {
      extract_sql <- dbplyr::translate_sql(
        ifelse(
          type_of(!!extract_sql) %in% !!c("object", "array"),
          !!extract_sql,
          json_build_array(!!extract_sql)
        )
      )
    }

    cast_to_ptype(ptype, extract_sql)
  }

  dbplyr::sql_variant(
    scalar = scalar_new,
    aggregate = postgres_translator$aggregate,
    window = postgres_translator$window
  )
}
