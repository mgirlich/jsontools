#' Build a JSON object
#'
#' @export
#' @examples
#' json_build_object(
#'   '{"foo": 5, "bar": {"foo": [1, 2, 3], "bar": 3}}',
#'   f = .foo, bf1 = `.bar.foo | .[1]`, bb = `.bar.bar`
#' )
json_build_object <- function(x, ...) {
  force(x)

  jqr_ob <- jqr::build_object(x, ...)
  jq_do(x, jqr_ob$args[[1]], .na = NA_character_)
}
