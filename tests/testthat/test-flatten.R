
# json_flatten_query ------------------------------------------------------

test_that("json_flatten_query works", {
  xo1 <- json2('{"a":1,"b":2}')
  xo2 <- json2('{"a":3,"b":4}')

  create_array <- function(x) {
    r <- paste0("[", x, "]")
    names(r) <- names(x)
    new_json2(r)
  }

  expect_equal(
    json_flatten_query(create_array(xo1)),
    xo1
  )

  expect_equal(
    json_flatten_query(glue("[{xo1},{xo2},null]")),
    c(xo1, xo2)
  )

  expect_equal(
    json_flatten_query(create_array(c(xo1, xo2))),
    c(xo1, xo2)
  )

  expect_equal(
    json_flatten_query(create_array(c(a = paste0(xo1, ",", xo1), b = xo2))),
    c(a = xo1, a = xo1, b = xo2)
  )
})

test_that("json_flatten_query edge cases", {
  expect_equal(
    json_flatten_query("[]"),
    json2()
  )

  expect_equal(
    json_flatten_query(character()),
    json2()
  )

  expect_equal(
    json_flatten_query("[null]"),
    json2()
  )

  expect_equal(
    json_flatten_query("null"),
    json2()
  )

  expect_equal(
    json_flatten_query(NA_character_),
    json2()
  )
})

test_that("json_flatten_query errors for non-object arrays", {
  expect_error(
    json_flatten_query("[1, 2]"),
    class = "jsontools_error"
  )

  expect_error(
    json_flatten_query('[{"a": 1}, 1]'),
    class = "jsontools_error"
  )

  expect_error(
    json_flatten_query('{"a": 1}'),
    class = "jsontools_error"
  )
})


# json_flatten_value ------------------------------------------------------

test_that("json_flatten_value works", {
  x_real <- "[1.0, 2.0, null]"
  x_int <- "[1, 2, null]"
  x_lgl <- "[true, false, null]"

  expect_equal(
    json_flatten_value(x_real),
    c(1, 2)
  )

  expect_equal(
    json_flatten_value(c(a = x_real)),
    c(a = 1, a = 2)
  )

  expect_equal(
    json_flatten_value(x_int),
    as.integer(c(1, 2))
  )

  expect_equal(
    json_flatten_value(x_lgl),
    c(TRUE, FALSE)
  )

  x_int_mixed <- "[true, false, null, 2]"
  expect_equal(
    json_flatten_value(x_int_mixed),
    as.integer(c(1L, 0L, 2L))
  )

  x_real_mixed <- "[2, true, false, 1.1]"
  expect_equal(
    json_flatten_value(x_real_mixed),
    c(2, 1, 0, 1.1)
  )
})

test_that("json_flatten_value handles scalars correctly", {
  expect_error(
    json_flatten_value(c("[1, 2]", 3)),
    class = "jsontools_error"
  )

  expect_equal(
    json_flatten_value(c("[1, 2]", 3), wrap_scalars = TRUE),
    c(1, 2, 3)
  )

  expect_error(
    json_flatten_value(c('["a", "b"]', "c")),
    class = "jsontools_error"
  )

  expect_equal(
    json_flatten_value(c('["a", "b"]', "c"), wrap_scalars = TRUE),
    c("a", "b", "c")
  )
})

test_that("json_flatten_value edge cases", {
  expect_equal(
    json_flatten_value("[]"),
    NULL
  )

  expect_equal(
    json_flatten_value("[]", ptype = integer()),
    integer()
  )

  expect_equal(
    json_flatten_value("[null]"),
    NULL
  )

  expect_equal(
    json_flatten_value(character()),
    NULL
  )

  expect_snapshot_error(
    json_flatten_value("null")
  )

  expect_equal(
    json_flatten_value(NA_character_),
    NULL
  )
})

test_that("json_flatten_value errors", {
  expect_error(
    json_flatten_value("[1, 2, [1]]"),
    class = "jsontools_error"
  )

  expect_error(
    json_flatten_value('[1, 2, {"a": 1}]'),
    class = "jsontools_error"
  )

  expect_error(
    json_flatten_value('[1, 2, "a"]'),
    class = "jsontools_error"
  )
})


# json_each_df ------------------------------------------------------------

test_that("json_each_df works", {
  expect_equal(
    json_each_df(
      c(
        x = '[1, 2, "a", true]',
        '[false, null, [1, 2], {"x": 1}]'
      )
    ),
    tibble(
      index = c(rep(1, 4), rep(2, 4)),
      value = list(1, 2, "a", TRUE, FALSE, NULL, "[1,2]", '{"x":1}'),
      type = c("integer", "integer", "text", "true", "false", "null", "array", "object"),
      key = as.character(c(0:3, 0:3)),
      col_type = "array",
      name = c(rep("x", 4), rep("", 4))
    )
  )
})


# json_unnest_longer ------------------------------------------------------

test_that("json_unnest_longer works", {
  df <- tibble(
    id = 1:2,
    json = c(
      "[1,2]",
      "[3,5,9]"
    )
  )

  expect_equal(
    json_unnest_longer(df, "json"),
    tibble(
      id = c(1, 1, 2, 2, 2),
      json = c(1, 2, 3, 5, 9)
    )
  )

  df <- tibble(
    id = 1:2,
    json = c(
      "[null]",
      "[3,5,9]"
    )
  )

  expect_equal(
    json_unnest_longer(df, "json"),
    tibble(
      id = c(2, 2, 2),
      json = c(3, 5, 9)
    )
  )

  df <- tibble(
    id = 1:3,
    json = c(
      "[null]",
      '["a", "b"]',
      "c"
    )
  )

  expect_equal(
    json_unnest_longer(df, "json"),
    tibble(
      id = c(2, 2, 3),
      json = c("a", "b", "c")
    )
  )

  skip("not yet decided how to handle")
  df <- tibble(
    id = 1:2,
    json = c(
      NA_character_,
      '["a", "b"]'
    )
  )

  expect_equal(
    json_unnest_longer(df, "json"),
    tibble(
      id = c(2, 2, 3),
      json = c("a", "b", "c")
    )
  )
})

test_that("json_unnest_longer with discog_json", {
  df <- tibble(json = discog_json)

  item_df <- json_unnest_longer(
    df,
    "json",
    values_to = "item"
  )

  expect_snapshot_value(
    item_df,
    style = "json2"
  )

  artists_df <- json_unnest_longer(
    item_df,
    "item",
    path = c("$.basic_information.artists"),
    values_to = "artists",
    indices_to = "component_id"
  )

  expect_snapshot_value(
    artists_df,
    style = "json2"
  )
})
