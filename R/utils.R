json2_do <- function(x, f, json2 = TRUE) {
  r <- purrr::map_if(x, ~ !is.na(.x), f) %>%
    purrr::flatten_chr()

  if (is_true(json2)) {
    new_json2(r)
  } else {
    r
  }
}

#' @export
prettify <- function(x, json2 = TRUE) {
  json2_do(x, jsonlite::prettify, json2 = json2)
}

#' @export
minify <- function(x, json2 = TRUE) {
  json2_do(x, jsonlite::minify, json2 = json2)
}

#' @export
json_u <- function(x) {
  jsonlite::unbox(x)
}


row_all <- function(x) {
  unname(apply(x, 1, all))
}

row_any <- function(x) {
  unname(apply(x, 1, any))
}
