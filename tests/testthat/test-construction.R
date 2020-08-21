test_that("low-level constructor", {
  expect_length(new_json2(), 0)
  expect_length(new_json2(character()), 0)

  expect_length(new_json2(NA_character_), 1)

  expect_length(new_json2(x_valid), 1)
  expect_length(new_json2(c(x_valid, NA_character_)), 2)

  result <- new_json2('{"a":1}')
  expect_equal(
    new_json2(x_jqson),
    result
  )

  expect_equal(
    new_json2(x_jsonlite),
    result
  )

  expect_equal(
    new_json2(x_pq_json),
    result
  )
})


test_that("construction works", {
  expect_equal(
    json2(x_valid),
    new_json2(x_valid)
  )

  expect_equal(
    json2(c(x_valid, NA)),
    new_json2(c(x_valid, NA))
  )

  expect_equal(
    json2(c(NA, NA)),
    new_json2(c(NA_character_, NA_character_))
  )
})


test_that("input is validated", {
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
