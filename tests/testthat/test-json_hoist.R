test_that("json_hoist works", {
  df <- tibble::tibble(
    id = 1:5,
    json = json_flatten(got_chars_json)
  )

  expect_equal(
    json_hoist(df, json, url = "$.url", "$.name"),
    tibble(
      id = 1:5,
      url = json_extract(df$json, "$.url"),
      name = json_extract(df$json, "$.name")
    )
  )
})

test_that("json_hoist works", {
  expect_snapshot_error(
    json_hoist(tibble(json = '[0,1]'), json, a = "$[0]", a = "$[0]")
  )
})
