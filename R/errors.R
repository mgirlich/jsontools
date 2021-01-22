stop_jsontools <- function(message, ..., error_type = NULL) {
  class <- "jsontools_error"
  if (!is.null(error_type)) {
    class <- c(paste0("jsontools_error_", error_type), class)
  }

  abort(
    message = message,
    class = class,
    ...
  )
}

#' @export
conditionMessage.jsontools_error_invalid_json <- function(c) {
  errors_shown <- 5

  n_invalid <- length(c$offsets)
  if (n_invalid > 10) {
    locs <- c(head(c$locations, 5), "...", utils::tail(c$locations, 5))
  } else {
    locs <- c$locations
  }
  locs <- paste0(locs, collapse = ", ")

  if (c$x_arg != "") {
    c$x_arg <- paste0("`", c$x_arg, "` has ")
  }

  head <- glue("
  {c$x_arg}invalid JSON at {n_invalid} locations:
    {locs}
  ")

  size <- min(length(c$errors), errors_shown)
  body <- glue::glue_data(c, "{locations}: offset {offsets}\n{errors}")[1:size]
  if (n_invalid > errors_shown) {
    body <- c(body, paste0("... (", n_invalid - size, " more errors)"))
  }
  body <- paste0(body, collapse = "\n")

  paste0(head, "\n\n", body)
}
