#' Equality
#'
#' @keywords internal
#' @method vec_proxy_equal json2
#' @export
#' @export vec_proxy_equal.json2
#' @examples
#' x <- new_json2('{"a": {"x": 1, "y": 2}, "b": 2, "c": 3}')
#' x2 <- new_json2(
#'   '{
#'     "b": 2,
#'     "c": 3,
#'     "a": {
#'      "y": 2,
#'      "x": 1
#'     }
#'   }'
#' )
#'
#' # order and format of json doesn't matter
#' x == x2
#'
#' # different values are recognized
#' y <- new_json2('{"a": {"x": 1}, "b": 2, "c": 4}')
#' x == y
vec_proxy_equal.json2 <- function(x, ...) {
  # jq --argfile a a.json --argfile b b.json -n '($a | (.. | arrays) |= sort) as $a | ($b | (.. | arrays) |= sort) as $b | $a == $b'
  # need to unclass because as.character doesn't remove the json class
  # need to check for NA and "null" because jqr errors on these inputs

  # https://stackoverflow.com/questions/31930041/using-jq-or-alternative-command-line-tools-to-compare-json-files
  # jq --argfile a a.json --argfile b b.json -n '($a | (.. | arrays) |= sort) as $a | ($b | (.. | arrays) |= sort) as $b | $a == $b'


  idx <- is.na(unclass(x)) | (unclass(x) == "null")

  # TODO crashes because jqr::jq doesn't handle null very well
  # jqr::jq("null")
  # jqr::jq("1")
  # jqr::jq(c("1", "null"))

  x[!idx] <- jq_do(x[!idx], flags = jqr::jq_flags(sorted = TRUE))
  x
}
