# json_each ---------------------------------------------------------------

test_that("json_each works", {
  df <- tibble::tribble(
    ~ description,    ~ json,
    "NA",             NA,
    "empty array",    "[]",
    "empty object",   "{}",
    "json null",      "null",
    "array w/ null",  "[null]",
    "object w/ null", '{"a": null}'
  )

  out <- json_each(df$json)
  idx <- vctrs::vec_match(vctrs::vec_seq_along(df), out$row_id)
  expect_snapshot(vec_cbind(df, vec_slice(out, idx)))
})

# json_flatten ------------------------------------------------------

test_that("json_flatten works for json", {
  xo1 <- json2('{"a":1,"b":2}')
  xo2 <- json2('{"a":3,"b":4}')

  create_array <- function(x) {
    r <- paste0("[", x, "]")
    names(r) <- names(x)
    new_json2(r)
  }

  expect_equal(
    json_flatten(create_array(xo1)),
    xo1
  )

  expect_equal(
    json_flatten(glue("[{xo1},{xo2},null]")),
    c(xo1, xo2)
  )

  expect_equal(
    json_flatten(create_array(c(xo1, xo2))),
    c(xo1, xo2)
  )

  expect_equal(
    json_flatten(create_array(c(a = paste0(xo1, ",", xo1), b = xo2))),
    c(a = xo1, a = xo1, b = xo2)
  )
})

test_that("json_flatten works for scalars", {
  x_real <- "[1.0, 2.0, null]"
  x_int <- "[1, 2, null]"
  x_lgl <- "[true, false, null]"

  expect_equal(
    json_flatten(x_real),
    c(1, 2)
  )

  expect_equal(
    json_flatten(c(a = x_real)),
    c(a = 1, a = 2)
  )

  expect_equal(
    json_flatten(x_int),
    as.integer(c(1, 2))
  )

  expect_equal(
    json_flatten(x_lgl),
    c(TRUE, FALSE)
  )

  x_int_mixed <- "[true, false, null, 2]"
  expect_equal(
    json_flatten(x_int_mixed),
    as.integer(c(1L, 0L, 2L))
  )

  x_real_mixed <- "[2, true, false, 1.1]"
  expect_equal(
    json_flatten(x_real_mixed),
    c(2, 1, 0, 1.1)
  )
})

test_that("json_flatten empty output", {
  expect_null(json_flatten(character()))
  expect_null(json_flatten("null"))
  # compare to purrr::flatten(NULL) -> error
  # but error doesn't feel right

  expect_null(json_flatten("[]"))
  # compare to purrr::flatten(list())

  expect_null(json_flatten("[null]"))
  # compare to `purrr::flatten()`

  expect_null(json_flatten(NA_character_))
  # no equivalent?

  expect_equal(
    json_flatten("[]", ptype = integer()),
    integer()
  )
})

test_that("json_flatten errors for mix of array/object and scalars", {
  expect_error(
    json_flatten('[{"a": 1}, 1]'),
    class = "jsontools_error"
  )
})

test_that("json_flatten can wrap scalars", {
  expect_equal(
    json_flatten("[[1,2], 2]", wrap_scalars = TRUE),
    new_json2(c("[1,2]", "[2]"))
  )
})

test_that("json_flatten handles scalars correctly", {
  expect_error(
    json_flatten(c("[1, 2]", 3)),
    class = "jsontools_error"
  )

  expect_equal(
    json_flatten(c("[1, 2]", 3), allow_scalars = TRUE),
    c(1, 2, 3)
  )

  expect_error(
    json_flatten(c('["a", "b"]', "c")),
    class = "jsontools_error"
  )

  expect_equal(
    json_flatten(c('["a", "b"]', "c"), allow_scalars = TRUE),
    c("a", "b", "c")
  )
})

test_that("json_flatten errors", {
  expect_snapshot_error(
    json_flatten("[1, 2, [1]]")
  )

  expect_snapshot_error(
    json_flatten('[1, 2, {"a": 1}]')
  )

  expect_snapshot_error(
    json_flatten('[1, 2, "a"]')
  )
})


# json_each_df ------------------------------------------------------------

test_that("json_each_df works", {
  expect_equal(
    json_each_df(
      c(
        x = '[1, 2, "a", true]',
        '[false, null, [1, 2], {"x": 1}]'
      )
    ),
    tibble(
      index = c(rep(1, 4), rep(2, 4)),
      value = list(1, 2, "a", TRUE, FALSE, NULL, json2("[1,2]"), json2('{"x":1}')),
      # value = list(1, 2, "a", TRUE, FALSE, NULL, new_json_array("[1,2]"), new_json_object('{"x":1}')),
      type = c("integer", "integer", "text", "true", "false", "null", "array", "object"),
      key = as.character(c(0:3, 0:3)),
      col_type = "array",
      name = c(rep("x", 4), rep("", 4))
    )
  )
})
