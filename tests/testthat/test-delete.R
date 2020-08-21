x <- json2(c(NA, '{"a":11,"b":{"x":12}}'))

test_that("delete works", {
  expect_equal(
    json_delete(x, "$.a"),
    json2(c(NA, '{"b":{"x":12}}'))
  )

  expect_equal(
    json_delete(x, "$.not_there"),
    x
  )
})
