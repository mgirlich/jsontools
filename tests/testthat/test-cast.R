# * The print() and str() methods are defined in terms of format() so you get a pleasant, consistent display as soon as you’ve made your format() method.
# * You can immediately put your new vector class in a data frame because as.data.frame.vctrs_vctr() does the right thing.
# * Subsetting ([, [[, and $), length<-, and rep() methods automatically preserve attributes because they use vec_restore(). A default vec_restore() works for all classes where the attributes are data-independent, and can easily be customised when the attributes do depend on the data.
# * Default subset-assignment methods ([<-, [[<-, and $<-) follow the principle that the new values should be coerced to match the existing vector. This gives predictable behaviour and clear error messages
#
# # low-level constructor: new_json2
#
# When the change happens implicitly (e.g in c()) we call it coercion; when the change happens explicitly (e.g. with as.integer(x)), we call it casting.
#
# vec_ptype2(x, y) defines possible set of coercions. It returns a prototype if x and y can be safely coerced to the same prototype;
#
# vec_cast(x, to) defines the possible sets of casts. It returns x translated to have prototype to, or throws an error if the conversion isn’t possible. The set of possible casts is a superset of possible coercions because they’re requested explicitly.

# vec_ptype2 --------------------------------------------------------------

json <- new_json2()
array <- new_json_array()
object <- new_json_object()

test_that("self ptypes", {
  expect_equal(vec_ptype2(json, json), json)
})

test_that("ptype with character", {
  expect_equal(vec_ptype2(json, character()), character())
  expect_equal(vec_ptype2(character(), json), character())
})

test_that("ptype with json from other classes", {
  result <- new_json2(x_valid)
  expect_equal(as_json2(x_jqson), result)
  expect_equal(as_json2(x_jsonlite), result)
  expect_equal(as_json2(x_pq_json), result)
})

# vec_cast ----------------------------------------------------------------

test_that("cast to json2 works", {
  expect_equal(vec_cast("[1]", json2()), json2("[1]"))
})

test_that("cast to character works", {
  expect_equal(vec_cast(x_json2, character()), x_valid)
})

test_that("cast classes from other packages works", {
  result <- new_json2(x_valid)
  expect_equal(as_json2(x_jqson), result)
  expect_equal(as_json2(x_jsonlite), result)
  expect_equal(as_json2(x_pq_json), result)
})

test_that("some invalid casts", {
  expect_error(
    vec_cast(1, json2()),
    class = "vctrs_error_incompatible_type"
  )

  expect_invalid_json(
    vec_cast(x_invalid, json2()),
    offsets = 1,
    locations = 1
  )
})
