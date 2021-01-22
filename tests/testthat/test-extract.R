test_that("json_extract works", {
  # simple path
  expect_equal(
    json_extract(c("[true]", "[false]"), "$[0]"),
    c(TRUE, FALSE)
  )

  # nested path
  expect_equal(json_extract('{"nested": {"int": 1}}', "$.nested.int"), 1)

  # extract json
  expect_equal(
    json_extract(c('[{"a": 1}]', "[[1]]"), "$[0]"),
    json2(c('{"a":1}', "[1]"))
  )
})

test_that("json_extract can convert with ptype", {
  expect_equal(
    json_extract(c("[true]", "[false]"), "$[0]", ptype = numeric()),
    c(1, 0)
  )
})

test_that("json_extract handles mixed types", {
  # errors without explicit ptype list()
  expect_snapshot_error(
    json_extract(c("[1]", '["a"]'), "$[0]"),
    "vctrs_error_incompatible_type"
  )

  # works with ptype list()
  expect_equal(
    json_extract(c("[1]", '["a"]'), "$[0]", ptype = list()),
    list(1, "a")
  )
})

test_that("json_extract handles missing elements", {
  x <- c('{"miss-sometimes": 1}', '{"b": 1}')

  # path does not exist at all
  expect_snapshot_error(
    json_extract(x, "$.does-not-exist"),
    class = "jsontools_error"
  )

  expect_equal(
    json_extract(x, "$.does-not-exist", default = 2),
    c(2, 2)
  )

  # path does not always exist
  expect_snapshot_error(
    json_extract(x, "$.miss-sometimes"),
    class = "jsontools_error"
  )

  expect_equal(
    json_extract(x, "$.miss-sometimes", default = 2),
    c(1, 2)
  )

  # works for arrays
  y <- c('{"miss-sometimes": [1]}', '{"b": 1}')
  expect_equal(
    json_extract(y, "$.miss-sometimes", default = "[2]"),
    json2(c("[1]", "[2]"))
  )
})

test_that("json_extract checks `default`", {
  y <- c('{"miss-sometimes": [1]}', '{"b": 1}')

  # invalid `default` for array
  expect_snapshot_error(
    json_extract(y, "$.miss-sometimes", default = "1]"),
    class = "jsontools_error"
  )

  expect_snapshot_error(
    json_extract(y, "$.miss-sometimes", default = "a"),
    class = "jsontools_error"
  )
})

test_that("json_extract can handle NULL", {
  expect_equal(
    json_extract(c("[1]", "[null]"), "$[0]"),
    c(1, NA)
  )

  expect_equal(
    json_extract(c("[[1]]", "[null]"), "$[0]"),
    json2(c("[1]", NA))
  )
})

test_that("json_extract handles NA", {
  expect_equal(
    json_extract(c("[true]", NA), "$[0]"),
    c(TRUE, NA)
  )

  expect_error(
    json_extract(c("[true]", NA), "$.lgl", na = NULL),
    class = "jsontools_error"
  )

  expect_equal(
    json_extract(c('[{"a": 1}]', NA), "$[0]"),
    json2(c('{"a":1}', NA))
  )

  expect_equal(
    json_extract(c('[{"a": 1}]', NA), "$[0]", na = "{}"),
    json2(c('{"a":1}', "{}"))
  )
})

test_that("json_extract checks path", {
  expect_snapshot_error(json_extract(x, NA))

  expect_snapshot_error(json_extract(x, c()))

  expect_snapshot_error(json_extract(x, c("a", "b")))
})
