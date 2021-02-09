#' Rectangle a JSON vector
#'
#' @param .data A data frame.
#' @param .col JSON-column.
#' @param ... Elements of `.col` to turn into columns in the form
#'   `col_name = "JSON path"`.
#' @param .remove If `TRUE`, the default, will remove the column `.col`.
#' @param .ptype,.wrap_scalars,.default,.na,bigint_as_char Optionally, a named
#'   list of parameters passed to [`json_extract()`].
#'
#' @return A data frame, or subclass of data frame with new columns as
#'   specified in `...`.
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   id = 1:5,
#'   json = json_flatten(got_chars_json)
#' )
#' json_hoist(df, json, url = "$.url", name = "$.name")
#'
#' # the names can also be generated automatically
#' json_hoist(df, json, "$.url", "$.name")
json_hoist <- function(.data,
                       .col,
                       ...,
                       .remove = TRUE,
                       .ptype = list(),
                       .wrap_scalars = list(),
                       .default = list(),
                       .na = list(),
                       bigint_as_char = bigint_default()) {
  check_present(.col)
  .col <- tidyselect::vars_pull(names(.data), !!enquo(.col))

  values <- .data[[.col]]
  dots <- list2(...)

  nms <- names2(dots)
  unnamed <- nms == ""
  names(dots)[unnamed] <- substr(dots, 3, nchar(dots))[unnamed]

  if (vec_duplicate_any(names2(dots))) {
    stop_jsontools("The names of `...` must be unique.")
  }

  .ptype <- prep_list_arg(.ptype, names(dots), ".ptype")
  .wrap_scalars <- prep_list_arg(.wrap_scalars, names(dots), ".wrap_scalars")
  .default <- prep_list_arg(.default, names(dots), ".default")
  .na <- prep_list_arg(.na, names(dots), ".na")
  bigint_as_char <- prep_list_arg(bigint_as_char, names(dots), ".na")

  extracted_values <- purrr::imap(
    dots,
    ~ {
      json_extract(
        values,
        path = .x,
        ptype = .ptype[[.y]],
        wrap_scalars = .wrap_scalars[[.y]] %||% FALSE,
        bigint_as_char = bigint_as_char[[.y]]
      )
    }
  )

  out <- vec_cbind(.data, !!!extracted_values)

  if (is_true(.remove)) {
    out[[.col]] <- NULL
  }

  out
}
