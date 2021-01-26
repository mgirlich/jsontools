test_that("json_prettify works", {
  x <- c("[1,2,   3]", '{"a": 1, "b": 2}', NA)
  y <- c(
  "[1,
         2,   3]",
  '{"a":     1, "b": 2}',
  NA
  )

  expect_snapshot(json_prettify(x))
  expect_snapshot(json_prettify(y))

  expect_snapshot(json_minify(x))
  expect_snapshot(json_minify(y))
})
