test_that("useful error message", {
  input <- rep(c(x_valid, x_invalid), 12)
  verify_output(test_path("error_invalid_json"), json2(input))
})
