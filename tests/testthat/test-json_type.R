test_that("json_type works", {
  expect_equal(
    json_type(c("1", "[1]", '{"a": 1}')),
    c("integer", "array", "object")
  )

  expect_equal(
    json_type(c("[1]", '{"a": 1}', '{"a": "b"}'), "$.a"),
    c(NA, "integer", "text")
  )
})
