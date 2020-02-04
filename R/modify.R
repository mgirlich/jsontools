#' Update values
#'
#' @export
#' @examples
#' x_na <- c('{"a": 11, "b": {"x": 12}}', NA, '{"a": 21, "b": {"x": 22}}')
#' # update with different values
#' json_mutate(x_na, .a = 1:3)
#'
#' # NA is translated to null
#' json_mutate(x_na, .a = 1:3, .b.x = NA)
#'
#' # create new keys
#' json_mutate(x_na, .c = 0, .d.x = c("a", "b", "c"))
json_mutate <- function(x, ...) {
  dots <- list(...)
  na_flag <- is.na(x)

  if (all(lengths(dots) == 1)) {
    values <- purrr::map(dots, escape_value) %>%
      purrr::flatten_chr()

    jq_cmd <- glue_jq("{names(dots)} = {values}")
    jq_do(x, jq_cmd, slurp = FALSE, .na = NA_character_)
  } else {
    values <- vec_recycle_common(!!!dots, .size = length(x)) %>%
      purrr::map(escape_value) %>%
      purrr::map(~ .x[!na_flag]) %>%
      purrr::map(json_nest)

    updates <- glue_jq(
      "{names(dots)} = $values[{seq_along(dots) - 1}]",
      combine = TRUE
    )

    jq_cmd <- new_jqquery(
      glue_jq("[({json_nest(values)} | transpose), .]"),
      "transpose",
      glue_jq('map(.[0] as $values | .[1] | {updates})'),
      ".[]"
    )

    jq_do(x, jq_cmd, slurp = TRUE, .na = NA_character_)
  }
}

#' Delete key
#'
#' @export
#' @examples
#' json_delete_path(x, id = ".abc")
#' json_delete_path(x, id = ".abc.def")
#' json_delete_path(x, id = ".not_there.def")
json_delete_id <- function(x, id) {
  # TODO support multiple keys via list(key1, key2, ...)?
  # --> problem: syntax different than for other verbs
  # --> maybe support via dots?
  check1(id)
  jq_cmd <- jq_delete_id(id)
  jq_do(x, jq_cmd)
}

#' Merge two jsons
#'
#' @export
#' @examples
#' # something like list_modify and list_merge?
#' json_merge('{"a": 1, "c": 3}', '{"a": 11, "b": 2}')
json_merge <- function(x, y) {
  # TODO support length(y) > 1
  check1(y)
  jq_do(x, glue(". + {y}"))
}
