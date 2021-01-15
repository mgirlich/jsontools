test_that("json_extract works", {
  # simple path
  expect_equal(
    json_extract(c('[true]', '[false]'), "$[0]"),
    c(TRUE, FALSE)
  )

  # nested path
  expect_equal(json_extract('{"nested": {"int": 1}}', "$.nested.int"), 1)
})

test_that("json_extract can convert with ptype", {
  expect_equal(
    json_extract(c('[true]', '[false]'), "$[0]", ptype = numeric()),
    c(1, 0)
  )
})

test_that("json_extract handles mixed types", {
  # errors without explicit ptype list()
  expect_snapshot_error(
    json_extract(c('[1]', '["a"]'), "$[0]"),
    "vctrs_error_incompatible_type"
  )

  # works with ptype list()
  expect_equal(
    json_extract(c('[1]', '["a"]'), "$[0]", ptype = list()),
    list(1, "a")
  )
})

test_that("json_extract handles long ints", {
  skip("bigint handling not yet implemented")
  # y <- c(
  #   '{"mixed": "a"}',
  #   '{"first_above": 2147483648, "mixed": 9999999999}'
  # )

  expect_equal(
    json_extract('[2147483647]', "$[0]"),
    2147483647L
  )

  expect_equal(
    json_extract('[-2147483647]', "$[0]"),
    -2147483647L
  )

  some_above <- c('[1]', '[9999999999]')
  expect_snapshot(
    expect_equal(
      json_extract(some_above, "$[0]"),
      c("1", "9999999999")
    )
  )

  expect_snapshot(
    expect_equal(
      json_extract(some_above, "$[0]", bigint_as_char = FALSE),
      bit64::as.integer64(c("1", "9999999999"))
    )
  )

  int64 <- bit64::integer64()
  expect_snapshot_error(
    json_extract(some_above, "$[0]", ptype = int64),
    class = "jsontools_error"
  )

  expect_snapshot_output(
    expect_equal(
      json_extract(some_above, "$[0]", ptype = int64, bigint_as_char = FALSE),
      bit64::as.integer64(c("1", "9999999999"))
    )
  )

  char <- character()
  expect_equal(
    json_extract(some_above, "$[0]", ptype = char),
    c("1", "9999999999")
  )

  expect_snapshot_error(
    json_extract(some_above, "$[0]", ptype = char, bigint_as_char = FALSE)
  )
})

test_that("json_extract handles missing and null elements", {
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
  skip("not yet implemented")
  y <- c('{"miss-sometimes": [1]}', '{"b": 1}')

  # invalid `default` for array
  expect_snapshot_error(
    json_extract(y, "$.miss-sometimes", default = "1]"),
    class = "jsontools_error"
  )

  expect_snapshot_error(
    json_extract(y, "$.miss-sometimes", default = 'a'),
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

test_that("json_extract checks path syntax", {
  skip("not yet implemented path check")
  expect_snapshot_error(json_extract(x, ".commit.author"))
})

test_that("json_extract handles objects and arrays", {
  expect_equal(
    json_extract('[{"a": 1}]', "$[0]"),
    json2('{"a":1}')
  )

  expect_equal(
    json_extract('[[1]]', "$[0]"),
    json2('[1]')
  )

  expect_equal(
    json_extract(c('[{"a": 1}]', '[[1]]'), "$[0]"),
    json2(c('{"a":1}', '[1]'))
  )
})

test_that("json_extract handles ptype object and array", {
  expect_snapshot_error(
    json_extract('[{"a": 1}]', "$[0]", new_json_array())
  )

  expect_equal(
    json_extract('[{"a": 1}]', "$[0]", new_json_object()),
    json2('{"a":1}')
  )

  expect_snapshot_error(
    json_extract('[[1]]', "$[0]", new_json_object())
  )

  expect_equal(
    json_extract('[[1]]', "$[0]", new_json_array()),
    json2('[1]')
  )
})
