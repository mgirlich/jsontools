#' Get keys of JSON object resp. array
#'
#' @param x A JSON vector
#'
#' @return A list of keys.
#' @export
#'
#' @examples
#' json_keys(c(
#'   '{"a": 1, "b": 2}',
#'   '{"x": 1, "y": 2}',
#'   "[1, 2]"
#' ))
json_keys <- function(x) {
  df <- json_each(x)
  types <- json_type(x)
  df_split <- vec_split(df$key, df$row_id)

  purrr::map2(
    df_split$val,
    types,
    function(keys, type) {
      if (type == "array") {
        as.integer(keys)
      } else {
        keys
      }
    }
  )
}
