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
})

test_that("json_unnest_longer handles NA/null/empty arrays/arrays of null", {
  df <- tibble(
    id = 1:2,
    json = c(NA_character_, '["a", "b"]')
  )

  out <- tibble(
    id = c(1, 2, 2),
    json = c(NA, "a", "b")
  )

  expect_equal(json_unnest_longer(df, "json"), out)
  # tidyr::unnest_longer(tibble(id = 1:2, l = list(NULL, 1:2)), l)

  df$json[1] <- "null"
  expect_equal(json_unnest_longer(df, "json"), out)
  # tidyr::unnest_longer(tibble(id = 1:2, l = list(NULL, 1:2)), l)

  df$json[1] <- "[]"
  expect_equal(json_unnest_longer(df, "json"), out)
  # tidyr::unnest_longer(tibble(id = 1:2, l = list(c(), 1:2)), l)

  df$json[1] <- "[null]"
  expect_equal(json_unnest_longer(df, "json"), out)
  # no equivalent?
})


test_that("json_unnest_longer with discog_json", {
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

  artist_df <- tibble(
    artists = json_extract(item_df$item, "$.basic_information.artists")
  )

  expect_snapshot_value(
    json_unnest_longer(
      artist_df,
      "artists",
      row_numbers_to = "component_id"
    ),
    style = "json2"
  )
})
