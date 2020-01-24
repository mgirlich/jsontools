#' @export
format.json2 <- function(x, ..., pretty = FALSE) {
  if (length(x) == 0)
    return(character())

  # TODO left align doesn't look nice?
  # --> that is the default formatting...
  # x <- c('{"def": 1, "abc": 2}', '{"def": 2}', '[2, 1, 3]')
  # json2(x)

  if (is_true(pretty)) {
    x <- purrr::map_chr(x, jsonlite::prettify)
  }

  # TODO maybe use justify = "none"
  # ifelse(is.na(x), "<NA>", NextMethod("format", x, justify = "none"))
  ifelse(is.na(x), "<NA>", x)
}

#' @export
obj_print_data.json2 <- function(x, ..., pretty = FALSE) {
  if (length(x) == 0)
    return()

  if (is_true(pretty)) {
    # TODO should this support names via `labels` arg?
    cat(format(x, pretty = TRUE))
  } else {
    out <- stats::setNames(format(x), names(x))
    # TODO use cat() to get justify = "none"?
    # --> doesn't print names then
    print(out, quote = FALSE)
  }

  invisible(x)
}

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
