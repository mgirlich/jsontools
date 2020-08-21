x <- '{"a": 1}'
test_that("logical work", {
  expect_equal(
    json_mutate(x, .a = TRUE),
    json2('{"a":true}')
  )
})

test_that("integer work", {
  expect_equal(
    json_mutate(x, .a = 1L),
    json2('{"a":1}')
  )
})

test_that("numeric work", {
  expect_equal(
    json_mutate(x, .a = 12.34),
    json2('{"a":12.34}')
  )
})

test_that("character work", {
  expect_equal(
    json_mutate(x, .a = "text"),
    json2('{"a":"text"}')
  )
})

test_that("factor work", {
  expect_equal(
    json_mutate(x, .a = factor("male", levels = c("male", "female"))),
    json2('{"a":"male"}')
  )
})

test_that("json2 work", {
  expect_equal(
    json_mutate(x, .a = json2('[1, 2, 3]')),
    json2('{"a":[1,2,3]}')
  )
})


x_na <- c(
  '{"a": 11, "b": {"x": 12}}',
  NA,
  '{"a": 21, "b": {"x": 22}}'
)

test_that("single values work", {
  expect_equal(
    json_mutate(x_na, .a = 0, .b = "a"),
    json2(c(
      '{"a":0.0,"b":"a"}',
      NA,
      '{"a":0.0,"b":"a"}'
    ))
  )

  expect_equal(
    json_mutate(x_na, .b.x = 0L),
    json2(c(
      '{"a":11,"b":{"x":0}}',
      NA,
      '{"a":21,"b":{"x":0}}'
    ))
  )
})


test_that("multi values work", {
  expect_equal(
    json_mutate(x_na, .a = 0:2, .b = c("a", "b", "c")),
    json2(c(
      '{"a":0,"b":"a"}',
      NA,
      '{"a":2,"b":"c"}'
    ))
  )
})


test_that("multi mixed with single values work", {
  expect_equal(
    json_mutate(x_na, .a = 0:2, .b = "a"),
    json2(c(
      '{"a":0,"b":"a"}',
      NA,
      '{"a":2,"b":"a"}'
    ))
  )
})


test_that("NA work", {
  expect_equal(
    json_mutate(x_na, .a = c(NA, NA, NA), .b = NA_integer_),
    json2(c(
      '{"a":null,"b":null}',
      NA,
      '{"a":null,"b":null}'
    ))
  )
})


test_that("json_merge works", {
  expect_equal(
    json_merge(
      '{"a": 1, "c": 3}',
      '{"a": 11, "b": 2}'
    ),
    new_json2('{"a":11,"c":3,"b":2}')
  )

  expect_equal(
    json_merge(
      c('{"a": 1, "c": 3}', '{"a": 1, "c": 4}'),
      '{"a": 11, "b": 2}'
    ),
    new_json2(c('{"a":11,"c":3,"b":2}', '{"a":11,"c":4,"b":2}'))
  )

  expect_equal(
    json_merge(
      c('{"a": 1, "c": 3}', '{"a": 1, "c": 4}'),
      c('{"a": 11}', '{"a": 12}')
    ),
    new_json2(c('{"a":11,"c":3}', '{"a":12,"c":4}'))
  )
})
