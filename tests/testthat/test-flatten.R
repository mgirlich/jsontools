
# json_flatten ------------------------------------------------------

test_that("json_flatten works", {
  xo1 <- json2('{"a":1,"b":2}')
  xo2 <- json2('{"a":3,"b":4}')

  create_array <- function(x) {
    r <- paste0("[", x, "]")
    names(r) <- names(x)
    new_json2(r)
  }

  expect_equal(
    json_flatten(create_array(xo1)),
    xo1
  )

  expect_equal(
    json_flatten(glue("[{xo1},{xo2},null]")),
    c(xo1, xo2)
  )

  expect_equal(
    json_flatten(create_array(c(xo1, xo2))),
    c(xo1, xo2)
  )

  expect_equal(
    json_flatten(create_array(c(a = paste0(xo1, ",", xo1), b = xo2))),
    c(a = xo1, a = xo1, b = xo2)
  )
})

test_that("json_flatten edge cases", {
  skip("not yet decided what to return")
  # TODO what should this return???
  expect_equal(
    json_flatten("[]"),
    json2()
  )

  expect_equal(
    json_flatten(character()),
    json2()
  )

  expect_equal(
    json_flatten("[null]"),
    json2()
  )

  expect_equal(
    json_flatten("null"),
    json2()
  )

  expect_equal(
    json_flatten(NA_character_),
    json2()
  )
})

test_that("json_flatten errors for non-object arrays", {
  skip("not yet adapted")
  # only makes sense if ptype is given
  expect_error(
    json_flatten("[1, 2]"),
    class = "jsontools_error"
  )

  expect_error(
    json_flatten('[{"a": 1}, 1]'),
    class = "jsontools_error"
  )

  expect_error(
    json_flatten('{"a": 1}'),
    class = "jsontools_error"
  )
})

test_that("json_flatten can wrap scalars", {
  skip("not yet decided")
  json_flatten('[[1,2], 2]')

  # TODO
  debugonce(json_unnest_longer)
  json_unnest_longer(tibble(x = '[[1,2], 2]'), x)
  json_flatten('[[1,2], 2]')
})

test_that("json_flatten works", {
  x_real <- "[1.0, 2.0, null]"
  x_int <- "[1, 2, null]"
  x_lgl <- "[true, false, null]"

  expect_equal(
    json_flatten(x_real),
    c(1, 2)
  )

  expect_equal(
    json_flatten(c(a = x_real)),
    c(a = 1, a = 2)
  )

  expect_equal(
    json_flatten(x_int),
    as.integer(c(1, 2))
  )

  expect_equal(
    json_flatten(x_lgl),
    c(TRUE, FALSE)
  )

  x_int_mixed <- "[true, false, null, 2]"
  expect_equal(
    json_flatten(x_int_mixed),
    as.integer(c(1L, 0L, 2L))
  )

  x_real_mixed <- "[2, true, false, 1.1]"
  expect_equal(
    json_flatten(x_real_mixed),
    c(2, 1, 0, 1.1)
  )
})

test_that("json_flatten handles scalars correctly", {
  expect_error(
    json_flatten(c("[1, 2]", 3)),
    class = "jsontools_error"
  )

  expect_equal(
    json_flatten(c("[1, 2]", 3), allow_scalars = TRUE),
    c(1, 2, 3)
  )

  expect_error(
    json_flatten(c('["a", "b"]', "c")),
    class = "jsontools_error"
  )

  expect_equal(
    json_flatten(c('["a", "b"]', "c"), allow_scalars = TRUE),
    c("a", "b", "c")
  )
})

test_that("json_flatten edge cases", {
  expect_equal(
    json_flatten("[]"),
    NULL
  )

  expect_equal(
    json_flatten("[]", ptype = integer()),
    integer()
  )

  expect_equal(
    json_flatten("[null]"),
    NULL
  )

  expect_equal(
    json_flatten(character()),
    NULL
  )

  expect_null(
    json_flatten("null"),
    NULL
  )

  expect_equal(
    json_flatten(NA_character_),
    NULL
  )
})

test_that("json_flatten errors", {
  expect_snapshot_error(
    json_flatten("[1, 2, [1]]")
  )

  expect_snapshot_error(
    json_flatten('[1, 2, {"a": 1}]')
  )

  expect_snapshot_error(
    json_flatten('[1, 2, "a"]')
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
      value = list(1, 2, "a", TRUE, FALSE, NULL, json2("[1,2]"), json2('{"x":1}')),
      # value = list(1, 2, "a", TRUE, FALSE, NULL, new_json_array("[1,2]"), new_json_object('{"x":1}')),
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
    id = 1:2,
    json = c(
      NA_character_,
      '["a", "b"]'
    )
  )

  expect_equal(
    json_unnest_longer(df, "json"),
    tibble(
      id = c(1, 2, 2),
      json = c(NA, "a", "b")
    )
  )

  skip("not sure whether to do or not")
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
})

test_that("json_unnest_longer with discog_json", {
  df <- tibble(json = discog_json)

  item_df <- json_unnest_longer(
    df,
    "json",
    values_to = "item"
  )[1:3, ]

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


# json_unnest_wider -------------------------------------------------------

test_that("json_unnest_wider works", {
  df <- tibble(
    id = 1:2,
    json = c(
      '{"a": null, "b": 12}',
      '{"a": 21}'
    )
  )

  expect_equal(
    json_unnest_wider(df, "json"),
    tibble(
      id = c(1, 2),
      b = c(12, NA),
      a = c(NA, 21)
    )
  )
})

test_that("json_unnest_wider handles NA", {
  skip("not yet decided what to do")
  df <- tibble(
    id = 1:2,
    json = c(
      '{"a": 1}',
      'null'
    )
  )

  expect_equal(
    json_unnest_wider(df, "json"),
    tibble(
      id = c(1, 2),
      a = c(1, NA)
    )
  )

  df <- tibble(
    id = 1:2,
    json = c(
      NA,
      '{"a": 1}'
    )
  )

  expect_equal(
    json_unnest_wider(df, "json"),
    tibble(
      id = 2,
      a = 1
    )
  )
})


test_that("json_unnest_wider works", {
  expect_named(
    json_unnest_wider(
      tibble(json = '{"b": 2, "a": 1}'),
      "json",
      names_sort = TRUE
    ),
    c("a", "b")
  )
})

test_that("json_unnest_wider respects ptype", {
  expect_equal(
    tibble(
      json = '{"chr": "a", "dbl": 1, "int": true}'
    ) %>%
      json_unnest_wider(
        "json",
        ptype = list(
          int = double(),
          lgl = integer()
        )
      ),
    tibble(
      chr = "a",
      dbl = 1,
      int = 1
    )
  )
})

test_that("json_unnest_wider errors on non-objects", {
  expect_snapshot_error(
    json_unnest_wider(tibble(json = '[1]'), "json")
  )
})

test_that("json_unnest_wider with discog_json", {
  df <- tibble(json = discog_json)

  item_df <- json_unnest_longer(
    df,
    "json",
    values_to = "item"
  )[1:3, ]

  expect_snapshot_value(
    item_df,
    style = "json2"
  )

  expect_snapshot_value(
    json_unnest_wider(
      item_df,
      "item",
      path = c("$.basic_information")
    ),
    style = "json2"
  )
})
