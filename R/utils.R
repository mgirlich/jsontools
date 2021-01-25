check_present <- function(x) {
  arg <- ensym(x)
  if (missing(x)) {
    abort(paste0("Argument `", arg, "` is missing with no default"))
  }
}

#' Unbox a vector or data frame
#'
#' Mark a vector of length one to not be wrapped in an array when formatted as
#' `JSON`. This is only a tiny wrapper around `jsonlite::unbox()` to avoid
#' conflict with `rlang::unbox()`.
#'
#' @param x atomic vector of length 1, or data frame with 1 row.
#'
#' @return A singleton version of `x`.
#' @export
#'
#' @examples
#' format_json(list(foo = 123))
#' format_json(list(foo = json_u(123)))
json_u <- function(x) {
  jsonlite::unbox(x)
}

prep_list_arg <- function(x, nms, x_arg) {
  extra_nm <- setdiff(names2(x), c(nms, ""))
  if (!is_empty(extra_nm)) {
    msg <- vec_c(
      x = paste0("Specified options in `", x_arg, "` that are not used."),
      i = paste0("`", extra_nm, "` not found"),
      .name_spec = "{outer}"
    )
    stop_jsontools(msg)
  }

  if (is_scalar(x)) {
    x <- rep_named(nms, list(x))
  }

  x
}

is_scalar <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  }

  if (is.list(x)) {
    (length(x) == 1) && !have_name(x)
  } else {
    length(x) == 1
  }
}

bigint_default <- function() {
  getOption("jsontools.bigint_as_char") %||% FALSE
}
