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
json_get <- function(x, path, json2 = TRUE) {
  jq_validate_path(path)
  jq_do(x, path, json2 = json2, .na = NA_character_)
}

#' Extract from a json
#'
#' @export
#' @examples
#' jsonr_extract_int(x, ".b")
#' jsonr_extract_lst(x, ".a")
#' jsonr_extract_int(x, ".a.x")
#'
#' if (FALSE) {
#'   dd %>%
#'     transmute(
#'       budget = jsonr_extract_lst(body, "budget"),
#'       amount = jsonr_extract_dbl(body, c("budget", "amount")),
#'     )
#'
#'   db_tbl %>%
#'     mutate(
#'       array_parsed = jsonr_extract_lst(json_array, ptype = double()),
#'       array_parsed2 = jsonr_extract_dbl(json_object, path = list("Sepal.Width", 0)),
#'       array_species = jsonr_extract_chr(json_object, path = "Species")
#'     ) %>%
#'     select(-json_array, -json_object) %>%
#'     tidyr::unnest(cols = c(array_parsed))
#' }
jsonr_extract <- function(x, ptype, path, ...) {
  if (!is_zap(path)) {
    x <- json_get(x, path, json2 = FALSE)
  }
  x_parsed_list <- parse_json_vector(x, .na = NA)

  # NOTE dropping column doesn't work in a mutate verb
  # --> to drop the column one would need another verb :-(
  vctrs::vec_cast(x_parsed_list, to = ptype, ...)
}

#' @rdname jsonr_extract
#' @export
jsonr_extract_lgl <- function(x, path) {
  jsonr_extract(x, ptype = logical(), path = path)
}

#' @rdname jsonr_extract
#' @export
jsonr_extract_int <- function(x, path) {
  jsonr_extract(x, ptype = integer(), path = path)
}

#' @rdname jsonr_extract
#' @export
jsonr_extract_dbl <- function(x, path) {
  jsonr_extract(x, ptype = double(), path = path)
}

#' @rdname jsonr_extract
#' @export
jsonr_extract_chr <- function(x, path) {
  jsonr_extract(x, ptype = character(), path = path)
}

#' @rdname jsonr_extract
#' @export
jsonr_extract_fct <- function(x, path) {
  jsonr_extract(x, ptype = factor(), path = path)
}

#' @rdname jsonr_extract
#' @export
jsonr_extract_dat <- function(x, path, format = "%Y-%m-%d") {
  stop("not yet properly supported")
  jsonr_extract(x, ptype = vctrs::new_date(), path = path, format = format)
}

#' @rdname jsonr_extract
#' @export
jsonr_extract_dtt <- function(x, path, format = "%Y-%m-%d %H:%M:%S") {
  stop("not yet properly supported")
  jsonr_extract(x, ptype = vctrs::new_datetime(), path = path, format = format)
}

#' @rdname jsonr_extract
#' @export
jsonr_extract_lst <- function(x, path, ptype = zap()) {
  if (is_zap(ptype)) {
    ptype <- list()
  } else {
    ptype <- vctrs::list_of(.ptype = ptype)
  }
  jsonr_extract(x, ptype = ptype, path = path)
}
