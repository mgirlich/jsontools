test_that("json_array_agg works", {
  expect_equal(
    json_array_agg(1:3),
    new_json2("[1,2,3]")
  )

  expect_equal(
    json_array_agg(c(1, 2)),
    new_json2("[1,2]")
  )

  expect_equal(
    json_array_agg(1+2i),
    new_json2('["1+2i"]')
  )

  expect_equal(
    json_array_agg(c(TRUE, FALSE)),
    new_json2("[true,false]")
  )

  expect_equal(
    json_array_agg(factor(c("a", "b"))),
    new_json2('["a","b"]')
  )

  x_posix <- vctrs::new_datetime(c(1, 2), tzone = "UTC")
  expect_equal(
    json_array_agg(x_posix),
    new_json2('["1970-01-01 00:00:01","1970-01-01 00:00:02"]')
  )

  expect_equal(
    json_array_agg(as.POSIXlt(x_posix)),
    new_json2('["1970-01-01 00:00:01","1970-01-01 00:00:02"]')
  )

  expect_equal(
    json_array_agg(vctrs::new_date(c(1, 2))),
    new_json2('["1970-01-02","1970-01-03"]')
  )

  expect_equal(
    json_array_agg(c("a", "b", "c")),
    new_json2('["a","b","c"]')
  )

  expect_equal(
    json_array_agg(c('a"b', "a\nb")),
    new_json2('["a\\"b","a\\nb"]')
  )
})

test_that("json_array_agg works with json2", {
  expect_equal(
    json_array_agg(json2(c('{"a": 1}', '{"b": 2}'))),
    new_json2('[{"a": 1},{"b": 2}]')
  )
})

test_that("is_json_array works", {
  expect_equal(
    is_json_array(c("1", "[1", "1]")),
    c(FALSE, FALSE, FALSE)
  )

  expect_equal(is_json_array(NA_character_, na = TRUE), TRUE)
  expect_equal(is_json_array(NA_character_, na = FALSE), FALSE)

  expect_equal(is_json_array("null", null = TRUE), TRUE)
  expect_equal(is_json_array("null", null = FALSE), FALSE)

  expect_equal(is_json_array("[1]"), TRUE)
})


# json_array_length -------------------------------------------------------

test_that("json_array_length works", {
  expect_equal(
    json_array_length(c("[]", "[1, 2, 3]", '["a", "b"]')),
    c(0, 3, 2)
  )

  expect_equal(
    json_array_length(c("null", "[1, 2]")),
    c(0, 2)
  )

  expect_equal(
    json_array_length(c(NA, "[1, 2]")),
    c(NA, 2)
  )
})

test_that("json_array_length handles scalars", {
  expect_equal(json_array_length(1, wrap_scalars = TRUE), 1)

  expect_equal(
    json_array_length(c("1", "[1, 2]"), wrap_scalars = TRUE),
    c(1, 2)
  )

  expect_snapshot_error(json_array_length(1))
})

# json_array_types --------------------------------------------------------

test_that("json_array_types works", {
  expect_equal(
    json_array_types(c("[1, true]", '["a", [1]]')),
    c("integer", "true", "text", "array")
  )
})

# json_wrap_scalars -------------------------------------------------------

test_that("json_wrap_scalars works", {
  expect_equal(
    json_wrap_scalars(c('["a", "b"]', "c", "d")),
    new_json2(c('["a", "b"]', '["c"]', '["d"]'))
  )

  expect_equal(
    json_wrap_scalars(c(1L, 2L, NA)),
    json2(c("[1]", "[2]", "null"))
  )
})
