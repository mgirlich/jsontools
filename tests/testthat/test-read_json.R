test_that("parse_json works", {
  expect_equal(
    parse_json('{"a": 1, "b": [1, 2]}'),
    list(a = 1, b = 1:2)
  )

  expect_snapshot_error(parse_json(1))
})

test_that("parse_json works only for scalar character", {
  expect_snapshot_error(parse_json(c("a", "b")))

  expect_snapshot_error(parse_json(1))
})

test_that("parse_json works for NULL", {
  expect_equal(parse_json(NULL), NULL)

  expect_equal(parse_json(NULL, .null = "a"), character())
})

test_that("parse_json works for NA", {
  expect_snapshot_error(parse_json(NA))

  expect_equal(parse_json(NA, .na = "a"), "a")
})

test_that("parse_json_vector works", {
  expect_equal(
    parse_json_vector(
      c(
        '{"a": 1, "b": [1, 2]}',
        NA,
        '[1, "a"]'
      ),
      .na = "missing value"
    ),
    list(
      list(a = 1, b = 1:2),
      "missing value",
      c("1", "a")
    )
  )
})
