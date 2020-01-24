x1 <- c('{"a": {"x": 1}, "b": 2, "c": [1, 2]}')
x <- c(x1, NA)

list_of_chr <- function(...) {
  list_of(..., .ptype = character())
}

list_of_chr2 <- function(...) {
  list_of(..., .ptype = list_of_chr())
}

test_that("json_paths works", {
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
    json_has_paths(x, list(c("a", "x"), c("b", "s"))),
    list(
      c(`["a","x"]` = TRUE, `["b","s"]` = FALSE),
      c(`["a","x"]` = NA, `["b","s"]` = NA)
    )
  )

  expect_equal(
    json_has_paths(NA_character_, list(c("a", "x"), c("b", "s"))),
    list(c(`["a","x"]` = NA, `["b","s"]` = NA))
  )
})

test_that("json_has_paths1 works", {
  expect_equal(
    json_has_paths1(x[1], list(c("a", "x"), c("b", "s"))),
    c(`["a","x"]` = TRUE, `["b","s"]` = FALSE)
  )

  expect_equal(
    json_has_paths1(NA_character_, list(c("a", "x"), c("b", "s"))),
    c(`["a","x"]` = NA, `["b","s"]` = NA)
  )
})
