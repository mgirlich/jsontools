x1 = c('{"a": {"x": 1}, "b": 2, "c": 3}')
x2 = c('{"a": {"x": 11, "y": 22}, "s": 12, "t": [1, 2, 3]}')
x <- c(x1, x2, NA)

list_of_chr <- function(...) {
  list_of(..., .ptype = character())
}

test_that("json_keys works", {
  expect_equal(
    json_keys(x),
    list_of_chr(c("a", "b", "c"), c("a", "s", "t"), character())
  )

  # TODO should this return NULL or character() or NA?
  # TODO for arrays this returns character but should probably be numeric?
  # TODO test with erroneous input
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
  # TODO should this return list_of<logical>? or a matrix?
  expect_equal(
    json_has_keys(x, c("b", "s")),
    list(c(b = TRUE, s = FALSE), c(b = FALSE, s = TRUE), c(b = NA, s = NA))
  )

  expect_equal(
    json_has_keys(NA_character_, c("b", "s")),
    list(c(b = NA, s = NA))
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
