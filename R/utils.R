pretty <- function() {
  stop("not yet implemented")
  purrr::modify(x, jsonlite::prettify)
}


stop_jsontools <- function(error_type, ...) {
  abort(
    class = paste0("jsontools_error_", error_type),
    ...
  )
}

#' @export
conditionMessage.jsontools_error_invalid_json <- function(cnd) {
  size <- min(length(cnd$errors), 6)
  paste0(
    "invalid json\n",
    glue::glue_data(cnd, "{locations}: offset {offsets}\n{errors}")[1:size]
  )
}


#' @export
json_u <- function(x) {
  jsonlite::unbox(x)
}
