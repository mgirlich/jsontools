test_that("json_keys works", {
  expect_equal(
    json_keys(c(
      '{"a": 1, "b": 2}',
      '{"x": 1, "y": 2}',
      "[1, 2]"
    )),
    list(
      c("a", "b"),
      c("x", "y"),
      c(0L, 1L)
    )
  )
})


test_that("json_path_exists works", {
  expect_equal(
    json_path_exists(
      c(
        '{"a": 1}',
        '{"b": 2}',
        "[1, 2]",
        NA_character_
      ),
      "$.a"
    ),
    c(TRUE, FALSE, FALSE, NA)
  )
})
