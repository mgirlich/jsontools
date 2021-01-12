test_that("json_agg_array works", {
  expect_equal(
    json_agg_array(1:3),
    json2("[1,2,3]")
  )

  expect_equal(
    json_agg_array(c("a", "b", "c")),
    json2('["a","b","c"]')
  )

  expect_equal(
    json_agg_array(c('a"b', 'a\nb')),
    json2('["a\\"b","a\\nb"]')
  )
})

test_that("json_agg_array works with json2", {
  expect_equal(
    json_agg_array(json2(c('{"a": 1}', '{"b": 2}'))),
    json2('[{"a": 1},{"b": 2}]')
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

test_that("json_array_length works", {
  expect_equal(
    json_array_length(c("[]", "[1, 2, 3]", '["a", "b"]')),
    c(0, 3, 2)
  )

  expect_equal(
    json_array_length(c("null", "[1, 2]")),
    c(0, 2)
  )

  skip("not yet decided")
  # what should NA return?
  expect_equal(
    json_array_length(c(NA, "[1, 2]")),
    c(NA, 2)
  )
})

test_that("json_array_length handles scalars", {
  expect_snapshot_error(json_array_length(1))

  expect_equal(json_array_length(1, wrap_scalars = TRUE), 1)

  expect_equal(
    json_array_length(c("1", "[1, 2]"), wrap_scalars = TRUE),
    c(1, 2)
  )
})
