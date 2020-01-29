stop_jsontools <- function(error_type, ...) {
  abort(
    class = paste0("jsontools_error_", error_type),
    ...
  )
}

#' @export
conditionMessage.jsontools_error_invalid_json <- function(cnd) {
  errors_shown <- 5

  n_invalid <- length(cnd$offsets)
  if (n_invalid > 10) {
    locs <- c(head(cnd$locations, 5), "...", tail(cnd$locations, 5))
  } else {
    locs <- cnd$locations
  }
  locs <- paste0(locs, collapse = ", ")

  head <- glue("
  invalid JSON at {n_invalid} locations:
    {locs}
  ")

  size <- min(length(cnd$errors), errors_shown)
  body <- glue::glue_data(cnd, "{locations}: offset {offsets}\n{errors}")[1:size]
  if (n_invalid > errors_shown) {
    body <- c(body, paste0("... (", n_invalid - size, " more errors)"))
  }
  body <- paste0(body, collapse = "\n")

  paste0(head, "\n\n", body)
}
