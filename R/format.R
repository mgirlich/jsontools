#' @export
format.json2 <- function(x, ..., pretty = FALSE) {
  # TODO by default don't print too many elements?
  if (is_true(pretty)) {
    x <- prettify(x)
  }

  vec_data(x)
}


#' @export
print.json2 <- function(x, ...) {
  cat(x, ..., sep = "\n")
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

#' @export
pillar_shaft.json2 <- function(x, ...) {
  out <- noquote(format(x))
  out[is.na(x)] <- NA
  pillar::new_pillar_shaft_simple(out, align = "right")
}
