test_that("format_json works", {
  expect_equal(
    format_json(list(a = 1, b = "x")),
    new_json2('{"a":[1],"b":["x"]}')
  )
})

test_that("format_json_list works", {
  expect_equal(
    format_json_list(list(a = 1, b = "x")),
    new_json2(c(a = "[1]", b = '["x"]'))
  )
})
