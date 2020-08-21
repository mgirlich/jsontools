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


# x1 <- c('{"a": {"x": 1}, "b": 2, "c": 3}')
# x2 <- c('{"a": {"x": 11, "y": 22}, "s": 12, "t": [1, 2, 3]}')
# x <- c(x1, x2)
#
# test_that("jsonr_has_keys works", {
#   expect_equal(
#     json_path_exists(x, c("b", "s")),
#     tibble(b = c(TRUE, FALSE), s = c(FALSE, TRUE))
#   )
#
#   expect_equal(
#     jsonr_has_keys(NA_character_, c("b", "s")),
#     tibble(b = NA, s = NA)
#   )
# })
#
# test_that("jsonr_has_keys1 works", {
#   expect_equal(
#     jsonr_has_keys1(x[1], c("b", "s")),
#     c(b = TRUE, s = FALSE)
#   )
#
#   expect_equal(
#     jsonr_has_keys1(NA_character_, c("b", "s")),
#     c(b = NA, s = NA)
#   )
# })
