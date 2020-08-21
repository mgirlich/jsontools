test_that("useful error message", {
  input <- rep(c(x_valid, x_invalid), 12)
  verify_output(test_path("data/error_invalid_json.txt"), json2(input))
})
