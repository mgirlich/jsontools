#' @export
format.json2 <- function(x, ..., pretty = FALSE) {
  # TODO by default don't print too many elements?
  if (is_true(pretty)) {
    x <- json_prettify(x)
  }

  vec_data(x)
}


#' @export
print.json2 <- function(x, ..., max_char = 1e3) {
  # suffix <- " ... <more elements>"
  # nchar_suffix <- nchar(suffix)
  # chars <- sum(nchar(x))
  # cumchars <- cumsum(nchar(x))
  #
  # if (chars > (max_char + nchar_suffix)) {
  #   x <- x[1]
  #   message("String too long, truncated")
  # }

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

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.json2 <- function(x, ...) {
  out <- noquote(format(x))
  out[is.na(x)] <- NA
  pillar::new_pillar_shaft_simple(out, align = "right")
}
