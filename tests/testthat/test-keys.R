x1 <- c('{"a": {"x": 1}, "b": 2, "c": 3}')
x2 <- c('{"a": {"x": 11, "y": 22}, "s": 12, "t": [1, 2, 3]}')
x <- c(x1, x2)

tibble <- tibble::tibble

list_of_chr <- function(...) {
  list_of(..., .ptype = character())
}

test_that("jsonr_keys works", {
  expect_equal(
    jsonr_keys(x),
    list_of_chr(c("a", "b", "c"), c("a", "s", "t"))
  )

  expect_error(
    jsonr_keys(NA),
    class = "jsontools_error_na_json"
  )

  expect_equal(
    jsonr_keys(NA, .na = c("a", "b")),
    list_of_chr(c("a", "b"))
  )
})


test_that("jsonr_keys1 works", {
  expect_equal(
    jsonr_keys1(x[1]),
    c("a", "b", "c")
  )

  expect_error(
    jsonr_keys1(NA_character_),
    class = "jsontools_error_na_json"
  )

  expect_equal(
    jsonr_keys1(NA_character_, .na = character()),
    character()
  )
})

test_that("jsonr_has_keys works", {
  expect_equal(
    jsonr_has_keys(x, c("b", "s")),
    tibble(b = c(TRUE, FALSE), s = c(FALSE, TRUE))
  )

  expect_equal(
    jsonr_has_keys(NA_character_, c("b", "s")),
    tibble(b = NA, s = NA)
  )
})

test_that("jsonr_has_keys1 works", {
  expect_equal(
    jsonr_has_keys1(x[1], c("b", "s")),
    c(b = TRUE, s = FALSE)
  )

  expect_equal(
    jsonr_has_keys1(NA_character_, c("b", "s")),
    c(b = NA, s = NA)
  )
})
