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
      a = c(NA, 21),
      b = c(12, NA)
    )
  )
})

test_that("json_unnest_wider handles NA/null/empty arrays/arrays of null", {
  df <- tibble(
    id = 1:2,
    json = c(NA_character_, '{"a": 1}')
  )

  out <- tibble(
    id = c(1, 2),
    a = c(NA, 1)
  )

  expect_equal(json_unnest_wider(df, "json"), out)
  # tidyr::unnest_wider(tibble(id = 1:2, l = list(NULL, list(a = 1))), l)

  df$json[1] <- "null"
  expect_equal(json_unnest_wider(df, "json"), out)
  # tidyr::unnest_longer(tibble(id = 1:2, l = list(NULL, 1:2)), l)

  df$json[1] <- "{}"
  expect_equal(json_unnest_wider(df, "json"), out)
  # tidyr::unnest_longer(tibble(id = 1:2, l = list(c(), 1:2)), l)

  # this must not be dropped!
  df$json[1] <- '{"a": null}'
  expect_equal(json_unnest_wider(df, "json"), out)
  # no equivalent?
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
    json_unnest_wider(tibble(json = "[1]"), "json")
  )
})

test_that("json_unnest_wider with discog_json", {
  df <- tibble(json = readChar(repurrrsive::discog_json(), nchars = 1e6))

  item_df <- json_unnest_longer(
    df,
    "json",
    values_to = "item"
  )[1:3, ]

  expect_snapshot_value(
    item_df,
    style = "json2"
  )

  basic_info_df <- tibble(
    basic_info = json_extract(item_df$item, "$.basic_information")
  )

  expect_snapshot_value(
    json_unnest_wider(
      basic_info_df,
      "basic_info"
    ),
    style = "json2"
  )
})

test_that("json_unnest_wider can wrap scalars", {
  df <- tibble(
    id = 1:2,
    json = c(
      '{"a": 1}',
      '{"a": [1]}'
    )
  )

  expect_snapshot_error(json_unnest_wider(df, json))

  expect_equal(
    json_unnest_wider(df, json, wrap_scalars = TRUE),
    tibble(id = 1:2, a = json2(c("[1]", "[1]")))
  )

  expect_equal(
    json_unnest_wider(df, json, wrap_scalars = list(a = TRUE)),
    tibble(id = 1:2, a = json2(c("[1]", "[1]")))
  )
})
