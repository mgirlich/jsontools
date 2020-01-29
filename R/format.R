#' @export
format.json2 <- function(x, ..., pretty = FALSE) {
  if (length(x) == 0) {
    return(character())
  }

  if (is_true(pretty)) {
    x <- prettify(x)
  }

  vec_data(x)
}

#' @export
# obj_print_data.json2 <- function(x, ..., pretty = FALSE) {
#   if (length(x) == 0) {
#     return()
#   }
#
#   if (is_true(pretty)) {
#     cat(format(x, pretty = TRUE))
#   } else {
#     out <- stats::setNames(format(x), names(x))
#     print(out, quote = FALSE)
#   }
#
#   invisible(x)
# }

#' @export
vec_ptype_abbr.json2 <- function(x) {
  "json2"
}

#' @export
vec_ptype_full.json2 <- function(x) {
  "json2"
}

# Dynamically exported, see zzz.R
is_vector_s3.json2 <- function(x) TRUE


#' @export
pillar_shaft.json2 <- function(x, ...) {
  # out <- ifelse(
  #   is.na(x),
  #   NA_character_,
  #   paste0(pillar::style_subtle("<raw "), blob_size(x, ...), pillar::style_subtle(">"))
  # )

  out <- noquote(x)
  pillar::new_pillar_shaft_simple(out, align = "right")
}
