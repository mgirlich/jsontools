test_that("construction works", {
  skip("not yet clear how to test")
  json2('{"abc": 1}')
  new_json2(c('{"abc": 1}', NA))
  # TODO that should work as well
  new_json2(c(NA_character_, NA))
})

expect_invalid_json <- function(object, offsets, locations) {
  err <- expect_error(object, class = "jsontools_error_invalid_json")
  expect_equal(err$offsets, offsets)
  expect_equal(err$locations, locations)
}

test_that("input is validated", {
  # TODO should that work??
  expect_invalid_json(
    json2(""),
    offsets = 1,
    locations = 1
  )

  # single error
  expect_invalid_json(
    json2('{"abc"'),
    offsets = 1,
    locations = 1
  )

  # mixed results
  expect_invalid_json(
    json2(c("[1, 2]", '{"abc"')),
    offsets = 1,
    locations = 2
  )

  # multiple errors
  expect_invalid_json(
    json2(c("[1", '{"abc"')),
    offsets = c(1, 1),
    locations = c(1, 2)
  )
})
