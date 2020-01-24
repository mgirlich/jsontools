#' Get value from path
#'
#' Postgres:
#' * key: '{"a": {"b":"foo"}}'::json->'a'
#' * path: '{"a": {"b":{"c": "foo"}}}'::json#>'{a,b}'
#' jq:
#' * key: .a
#'        .["a"]
#' * path: getpath(["a", "b"])
#'         .a.b
#'
#' @export
json_get <- function(x, path, json2 = TRUE) {
  jq_cmd <- jq_get_key(path)
  jq_do(x, jq_cmd, json2 = json2)
}


#' Extract from a json
#'
#' @export
#' @examples
#' dd %>%
#'   transmute(
#'     budget = json_extract_lst(body, "budget"),
#'     amount = json_extract_dbl(body, c("budget", "amount")),
#'   )
#'
#' db_tbl %>%
#'   mutate(
#'     array_parsed = json_extract_lst(json_array, ptype = double()),
#'     array_parsed2 = json_extract_dbl(json_object, path = list("Sepal.Width", 0)),
#'     array_species = json_extract_chr(json_object, path = "Species")
#'   ) %>%
#'   select(-json_array, -json_object) %>%
#'   tidyr::unnest(cols = c(array_parsed))
json_extract <- function(x, ptype, path = zap()) {
  if (!rlang::is_zap(path)) {
    x <- json_get(x, path, json2 = FALSE)
  }
  x_parsed_list <- lapply(x, parse_json)

  # NOTE dropping column doesn't work in a mutate verb
  # --> to drop the column one would need another verb :-(
  vctrs::vec_cast(x_parsed_list, to = ptype)
}

#' @rdname json_extract
#' @export
json_extract_lgl <- function(x, path = zap()) {
  json_extract(x, ptype = logical(), path = path)
}

#' @rdname json_extract
#' @export
json_extract_int <- function(x, path = zap()) {
  json_extract(x, ptype = integer(), path = path)
}

#' @rdname json_extract
#' @export
json_extract_dbl <- function(x, path = zap()) {
  json_extract(x, ptype = double(), path = path)
}

#' @rdname json_extract
#' @export
json_extract_chr <- function(x, path = zap()) {
  json_extract(x, ptype = character(), path = path)
}

#' @rdname json_extract
#' @export
json_extract_fct <- function(x, path = zap()) {
  json_extract(x, ptype = factor(), path = path)
}

#' @rdname json_extract
#' @export
json_extract_dat <- function(x, path = zap()) {
  json_extract(x, ptype = vctrs::new_date(), path = path)
}

#' @rdname json_extract
#' @export
json_extract_dtt <- function(x, path = zap()) {
  json_extract(x, ptype = vctrs::new_datetime(), path = path)
}

#' @rdname json_extract
#' @export
json_extract_lst <- function(x, path = zap(), ptype = zap()) {
  if (rlang::is_zap(ptype)) {
    ptype <- list()
  } else {
    ptype <- vctrs::list_of(.ptype = ptype)
  }
  json_extract(x, ptype = ptype, path = path)
}
