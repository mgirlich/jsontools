x1 <- c('{"a": {"x": 1}, "b": 2, "c": 3}')
x2 <- c('{"a": {"x": 11, "y": 22}, "s": 12, "t": [1, 2, 3]}')
x <- c(x1, x2, NA)

list_of_chr <- function(...) {
  list_of(..., .ptype = character())
}

test_that("json_keys works", {
  expect_equal(
    json_keys(x),
    list_of_chr(c("a", "b", "c"), c("a", "s", "t"), character())
  )

  expect_equal(
    json_keys(NA_character_),
    list_of_chr(character())
  )
})


test_that("json_keys1 works", {
  expect_equal(
    json_keys1(x[1]),
    c("a", "b", "c")
  )

  expect_equal(
    json_keys1(NA_character_),
    character()
  )
})

test_that("json_has_keys works", {
  expect_equal(
    json_has_keys(x, c("b", "s")),
    matrix(
      c(TRUE, FALSE, FALSE, TRUE, NA, NA),
      nrow = 3,
      byrow = TRUE,
      dimnames = list(NULL, c("b", "s"))
    )
  )

  expect_equal(
    json_has_keys(NA_character_, c("b", "s")),
    matrix(
      c(NA, NA),
      nrow = 1,
      byrow = TRUE,
      dimnames = list(NULL, c("b", "s"))
    )
  )
})

test_that("json_has_keys1 works", {
  expect_equal(
    json_has_keys1(x[1], c("b", "s")),
    c(b = TRUE, s = FALSE)
  )

  expect_equal(
    json_has_keys1(NA_character_, c("b", "s")),
    c(b = NA, s = NA)
  )
})
