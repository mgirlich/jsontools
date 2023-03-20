test_that("useful error message", {
  input <- rep(c(x_valid, x_invalid), 12)
  expect_snapshot(error = TRUE, {
    json2(input)
  })
})
