x_valid <- '{"a":1}'
x_invalid <- '{"a": 1'
x_pq_json <- structure(x_valid, class = "pq_jsonb")
x_jqson <- structure(x_valid, class = c("jqson", "character"))
x_jsonlite <- structure(x_valid, class = "json")

expect_invalid_json <- function(object, offsets, locations) {
  err <- expect_error(object, class = "jsontools_error_invalid_json")
  expect_equal(err$offsets, offsets)
  expect_equal(err$locations, locations)
}
