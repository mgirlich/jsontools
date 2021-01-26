test_that("format_json_rowwise works", {
  expect_equal(
    format_json_rowwise(tibble(a = 1:2, b = c("x", "y"))),
    new_json2(c(
      '{"a":1,"b":"x"}',
      '{"a":2,"b":"y"}'
    ))
  )
})
