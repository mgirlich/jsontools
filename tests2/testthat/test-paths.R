x1 <- c('{"a": {"x": 1}, "b": 2, "c": 3}')
x2 <- c('{"a": {"x": 11, "y": 22}, "s": 12, "t": [1, 2, 3]}')
x <- c(x1, x2)

tibble <- tibble::tibble

list_of_chr <- function(...) {
  list_of(..., .ptype = character())
}

list_of_chr2 <- function(...) {
  list_of(..., .ptype = list_of_chr())
}

test_that("json_paths works", {
  skip("not yet decided whether and how to implement")
  expect_equal(
    json_paths(x),
    list_of_chr2(
      list_of_chr("a", c("a", "x"), "b", "c", c("c", "0"), c("c", "1")),
      NULL
    )
  )

  expect_equal(
    json_paths(NA_character_),
    list_of_chr2(NULL)
  )
})


test_that("json_paths1 works", {
  skip("not yet decided whether and how to implement")
  expect_equal(
    json_paths1(x[1]),
    list_of_chr("a", c("a", "x"), "b", "c", c("c", "0"), c("c", "1"))
  )

  expect_equal(
    json_paths1(NA_character_),
    NULL
  )
})

test_that("json_has_paths works", {
  expect_equal(
    jsonr_has_paths(x, c(".a.x", ".a.y")),
    tibble(
      .a.x = c(TRUE, TRUE),
      .a.y = c(FALSE, TRUE)
    )
  )

  expect_equal(
    jsonr_has_paths(NA_character_, c(".a.x", ".a.y")),
    tibble(.a.x = NA, .a.y = NA)
  )
})

test_that("jsonr_has_paths1 works", {
  expect_equal(
    jsonr_has_paths1(x[1], c(".a.x", ".a.y")),
    c(.a.x = TRUE, .a.y = FALSE)
  )

  expect_equal(
    jsonr_has_paths1(NA_character_, c(".a.x", ".a.y")),
    c(.a.x = NA, .a.y = NA)
  )
})
