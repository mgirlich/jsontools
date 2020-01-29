prettify <- function(x) {
  purrr::modify_if(x, ~ !is.na(.x), jsonlite::prettify)
}


minify <- function(x) {
  purrr::modify_if(x, ~ !is.na(.x), jsonlite::minify)
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
