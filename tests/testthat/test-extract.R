x <- c(
  '{"lgl": true, "array": {"int": 2}, "miss_sometimes": 3, "has_null": 4, "mixed": 1}',
  '{"lgl": false, "array": {"int": 3}, "has_null": null, "mixed": "a"}'
)

test_that("json_get_value works", {
  # simple path
  expect_equal(
    json_get_value(x, "$.lgl"),
    c(TRUE, FALSE)
  )

  # ptype
  expect_equal(
    json_get_value(x, "$.lgl", ptype = numeric()),
    c(1, 0)
  )

  # nested path
  expect_equal(
    json_get_value(x, "$.array.int"),
    c(2, 3)
  )
})

test_that("json_get_value handles mixed types", {
  expect_error(
    json_get_value(x, "$.mixed")
  )

  expect_equal(
    json_get_value(x, "$.mixed", ptype = list()),
    list(1, "a")
  )
})

test_that("json_get_value handles long ints", {
  # TODO issue in jsonlite
  # jsonlite::validate('{"max": 2147483647, "min": 2147483647, "mixed": 1}"')
  # jsonlite::validate('{"mixed": 1}"')

  y <- c(
    '{"max": 2147483647, "min": -2147483647, "some_above": 1, "mixed": "a"}',
    '{"some_above": 9999999999, "first_above": 2147483648, "mixed": 9999999999}'
  )

  expect_equal(
    json_get_value(y[1], "$.max"),
    2147483647L
  )

  expect_equal(
    json_get_value(y[1], "$.min"),
    -2147483647L
  )

  expect_snapshot(
    expect_equal(
      json_get_value(y, "$.some_above"),
      c("1", "9999999999")
    )
  )

  expect_snapshot(
    expect_equal(
      json_get_value(y, "$.some_above", bigint_as_char = FALSE),
      bit64::as.integer64(c("1", "9999999999"))
    )
  )

  int64 <- bit64::integer64()
  expect_snapshot_error(
    json_get_value(y, "$.some_above", ptype = int64),
    class = "jsontools_error"
  )

  expect_snapshot_output(
    expect_equal(
      json_get_value(y, "$.some_above", ptype = int64, bigint_as_char = FALSE),
      bit64::as.integer64(c("1", "9999999999"))
    )
  )

  char <- character()
  expect_equal(
    json_get_value(y, "$.some_above", ptype = char),
    c("1", "9999999999")
  )

  expect_snapshot_error(
    json_get_value(y, "$.some_above", ptype = char, bigint_as_char = FALSE)
  )
})

test_that("json_get_value handles missing and null elements", {
  expect_error(
    json_get_value(x, "$.does-not-exist"),
    class = "jsontools_error"
  )

  expect_error(
    json_get_value(x, "$.miss_sometimes"),
    class = "jsontools_error"
  )

  expect_equal(
    json_get_value(x, "$.miss_sometimes", default = 2),
    c(3, 2)
  )

  expect_equal(
    json_get_value(x, "$.has_null"),
    c(4, NA)
  )
})

test_that("json_get_value handles NA", {
  expect_equal(
    json_get_value(c(x, NA), "$.lgl"),
    c(TRUE, FALSE, NA)
  )

  expect_error(
    json_get_value(c(x, NA), "$.lgl", na = NULL),
    class = "jsontools_error"
  )
})

test_that("json_get_value only extracts scalars", {
  expect_error(
    json_get_value(x, "$.array"),
    class = "jsontools_error"
  )
})


test_that("json_get_value checks input", {
  expect_error(
    json_get_value(x, NA)
  )

  expect_error(
    json_get_value(x, c())
  )

  expect_error(
    json_get_value(x, c("a", "b"))
  )

  skip("not yet implemented path check")
  expect_error(
    json_get_value(x, ".commit.author")
  )
})


# json_get_query ----------------------------------------------------------

x <- c(
  '{"a": 1, "b": {"x": 2}, "c": [3], "d": [4]}',
  '{"a": 2, "b": {"x": 3}, "d": null}'
)

test_that("json_get_query works", {
  # simple path
  expect_equal(
    json_get_query(x, "$.b"),
    json2(c(
      '{"x":2}',
      '{"x":3}'
    ))
  )
})

test_that("json_get_query only extracts objects and arrays", {
  expect_error(
    json_get_query(x, "$.a"),
    class = "jsontools_error"
  )
})


test_that("json_get_query handles missing and null elements", {
  # path does not exist at all
  expect_error(
    json_get_query(x, "$.does-not-exist"),
    class = "jsontools_error"
  )

  # path does not always exist
  expect_error(
    json_get_query(x, "$.c"),
    class = "jsontools_error"
  )

  expect_equal(
    json_get_query(x, "$.c", default = "[1]"),
    json2(c("[3]", "[1]"))
  )

  # invalid `default`
  expect_error(
    json_get_query(x, "$.c", default = "1]"),
    class = "jsontools_error"
  )

  expect_equal(
    json_get_query(x, "$.d"),
    json2(c("[4]", NA))
  )
})

test_that("json_get_query handles NA", {
  expect_equal(
    json_get_query(c(x, NA), "$.b"),
    json2(c(
      '{"x":2}',
      '{"x":3}',
      NA
    ))
  )

  expect_equal(
    json_get_query(c(x, NA), "$.b", na = "{}"),
    json2(c(
      '{"x":2}',
      '{"x":3}',
      "{}"
    ))
  )
})

test_that("json_get_query only extracts scalars", {
  expect_error(
    json_get_query(x, "$.a"),
    class = "jsontools_error"
  )
})


test_that("json_get_query checks input", {
  expect_error(
    json_get_query(x, NA)
  )

  expect_error(
    json_get_query(x, c())
  )

  expect_error(
    json_get_query(x, c("a", "b"))
  )

  skip("not yet implemented path check")
  expect_error(
    json_get_query(x, ".commit.author")
  )
})
