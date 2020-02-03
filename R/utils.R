prettify <- function(x) {
  purrr::map_if(x, ~ !is.na(.x), jsonlite::prettify) %>%
    purrr::flatten_chr() %>%
    new_json2()
}


minify <- function(x) {
  purrr::map_if(x, ~ !is.na(.x), jsonlite::minify) %>%
    purrr::flatten_chr() %>%
    new_json2()
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
