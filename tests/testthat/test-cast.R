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

x_valid <- '{"a": 1}'
x_invalid <- '{"a": 1'
x_pq_json <- structure('{"spa_campaign_id": 351776}', class = "pq_jsonb")
x_jqson <- jqr::jq(x)
x_jsonlite <- jsonlite::toJSON(list(a = 1))


x_json2 <- json2(x_valid)

test_that("low-level constructor", {
  new_json2(x_valid)
  new_json2(x_invalid)
  # new_json2(list())

  # TODO
  skip("not yet decided whether they should work")
  new_json2(x_jqson)
  new_json2(x_jsonlite)
  new_json2(x_pq_json)
})


test_that("friendly constructor", {
  json2(x_valid)
  json2(x_invalid)
  # json2(list())

  # TODO
  skip("not yet decided whether they should work")
  json2(x_jqson)
  json2(x_jsonlite)
  json2(x_pq_json)
})


test_that("casting works", {
  vec_cast(x_valid, json2())
  expect_error(
    vec_cast(x_invalid, json2())
  )

  vec_cast(json2(x_valid), character())
  vec_cast(1, json2())
})
