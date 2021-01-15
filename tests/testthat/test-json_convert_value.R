
# json_ptype_common -------------------------------------------------------

test_that("json_ptype_common works", {
  expect_equal(
    json_ptype_common(c("true", "false", "null")),
    logical()
  )

  expect_equal(
    json_ptype_common(c("integer", "true", "false", "null")),
    integer()
  )

  expect_equal(
    json_ptype_common(c("real", "integer", "true", "false", "null")),
    numeric()
  )

  expect_equal(
    json_ptype_common(c("text", "null")),
    character()
  )

  expect_equal(
    json_ptype_common(c("array", "null")),
    new_json2()
  )

  expect_equal(
    json_ptype_common(c("object", "null")),
    new_json2()
  )

  expect_equal(
    json_ptype_common(c("object", "array", "null")),
    new_json2()
  )
})

test_that("json_ptype_common incompatible types", {
  # incompatible with text
  expect_snapshot_error(json_ptype_common(c("true", "text")))
  expect_snapshot_error(json_ptype_common(c("false", "text")))
  expect_snapshot_error(json_ptype_common(c("integer", "text")))
  expect_snapshot_error(json_ptype_common(c("real", "text")))
  expect_snapshot_error(json_ptype_common(c("object", "text")))
  expect_snapshot_error(json_ptype_common(c("array", "text")))

  # object and array are compatible with text for correct ptype
  expect_equal(
    json_ptype_common(c("object", "text"), ptype = character()),
    character()
  )

  expect_equal(
    json_ptype_common(c("array", "text"), ptype = character()),
    character()
  )

  # incompatible with object/array
  expect_snapshot_error(json_ptype_common(c("object", "true")))
  expect_snapshot_error(json_ptype_common(c("array", "false")))
})

test_that("json_ptype_common incompatible ptype", {
  expect_snapshot_error(
    json_ptype_common("object", ptype = new_json_array())
  )

  expect_snapshot_error(
    json_ptype_common("array", ptype = new_json_object())
  )
})

# json_ptype_common -------------------------------------------------------

test_that("json_vec_c works with `ptype = NULL`", {
  expect_equal(
    json_vec_c(
      list(TRUE, FALSE, NA),
      c("true", "false", "null")
    ),
    c(TRUE, FALSE, NA)
  )

  expect_equal(
    json_vec_c(
      list(1L, TRUE, FALSE, NA),
      c("integer", "true", "false", "null")
    ),
    c(1, 1, 0, NA)
  )

  expect_equal(
    json_vec_c(
      list(1.2, 1L, TRUE, FALSE, NA),
      c("real", "integer", "true", "false", "null")
    ),
    c(1.2, 1, 1, 0, NA)
  )

  expect_equal(
    json_vec_c(
      list("a", NA),
      c("text", "null")
    ),
    c("a", NA)
  )

  expect_equal(
    json_vec_c(
      list(new_json2("[1]"), NA),
      c("array", "null")
    ),
    new_json2(c("[1]", NA))
  )

  expect_equal(
    json_vec_c(
      list(new_json2('{"a": 1}'), NA),
      c("object", "null")
    ),
    new_json2(c('{"a": 1}', NA))
  )

  expect_equal(
    json_vec_c(
      list(new_json2('{"a": 1}'), new_json2("[1]"), NA),
      c("object", "array", "null")
    ),
    new_json2(c('{"a": 1}', "[1]", NA))
  )
})

test_that("json_vec_c handles mix of array/object and text", {
  expect_snapshot_error(
    json_vec_c(
      list(new_json2('{"a": 1}'), "a"),
      c("object", "text")
    )
  )

  expect_snapshot_error(
    json_vec_c(
      list(new_json2("[1]"), "a"),
      c("array", "text")
    )
  )

  expect_equal(
    json_vec_c(
      list(new_json2('{"a": 1}'), "a"),
      c("object", "text"),
      ptype = character()
    ),
    c('{"a": 1}', "a")
  )

  expect_equal(
    json_vec_c(
      list(new_json2("[1]"), "a"),
      c("array", "text"),
      ptype = character()
    ),
    c("[1]", "a")
  )
})

test_that("json_vec_c handles json (array/object) ptype", {
  expect_snapshot_error(
    json_vec_c(
      list(new_json2('{"a": 1}'), new_json2("[1]")),
      c("object", "array"),
      ptype = new_json_array()
    )
  )

  expect_snapshot_error(
    json_vec_c(
      list(new_json2('{"a": 1}'), new_json2("[1]")),
      c("object", "array"),
      ptype = new_json_object()
    )
  )

  expect_equal(
    json_vec_c(
      list(new_json2('{"a": 1}'), new_json2("[1]")),
      c("object", "array"),
      ptype = new_json2()
    ),
    new_json2(c('{"a": 1}', "[1]"))
  )
})

test_that("json_convert_values errors for incompatible types", {
  expect_snapshot_error(
    json_vec_c(
      x = list(1, "a"),
      types = c("integer", "text"),
      ptype = NULL
    )
  )

  expect_snapshot_error(
    json_vec_c(
      x = list(1, 2),
      types = c("integer", "integer"),
      ptype = character()
    )
  )
})

# json_convert_value ------------------------------------------------------

test_that("json_convert_value works", {
  expect_equal(
    json_convert_value(
      x = c("1", "0", "1", "1.5", "a", "[1, 2]", '{"a": 1}'),
      json_types = c("true", "false", "integer", "real", "text", "array", "object"),
      ptype = list()
    ),
    list(TRUE, FALSE, 1, 1.5, "a", new_json2("[1, 2]"), new_json2('{"a": 1}'))
    # list(TRUE, FALSE, 1, 1.5, "a", new_json_array("[1, 2]"), new_json_object('{"a": 1}'))
  )
})

test_that("json_convert_value can wrap scalars", {
  expect_equal(
    json_convert_value(
      x = c("[1, 2]", 'a'),
      json_types = c("array", "text"),
      ptype = NULL,
      wrap_scalars = TRUE
    ),
    # new_json_array(c("[1, 2]", '["a"]'))
    new_json2(c("[1, 2]", '["a"]'))
  )

  expect_equal(
    json_convert_value(
      x = c("[1, 2]", 'a'),
      json_types = c("array", "text"),
      ptype = new_json_array(),
      wrap_scalars = TRUE
    ),
    # new_json_array(c("[1, 2]", '["a"]'))
    new_json2(c("[1, 2]", '["a"]'))
  )
})

test_that("json_convert_value handles big integers", {
  skip("bigint handling not yet implemented")
  expect_equal(
    json_convert_value(
      x = "9999999999",
      json_types = "integer",
      ptype = NULL,
      bigint_as_char = TRUE
    ),
    "9999999999"
  )

  expect_equal(
    json_convert_value(
      x = c("9999999999"),
      json_types = "integer",
      ptype = NULL,
      bigint_as_char = FALSE
    ),
    bit64::as.integer64("9999999999")
  )

  expect_equal(
    json_convert_value(
      x = c("9999999999", "1"),
      json_types = c("integer", "true"),
      ptype = NULL,
      bigint_as_char = FALSE
    ),
    bit64::as.integer64(c("9999999999", 1))
  )

  skip("not yet decided...")
  expect_equal(
    json_convert_value(
      x = c("9999999999", "1"),
      json_types = c("integer", "true"),
      ptype = NULL,
      bigint_as_char = TRUE
    ),
    c("9999999999", "1")
  )

  skip("bigint handling not yet implemented")
  expect_equal(
    json_extract('[2147483647]', "$[0]"),
    2147483647L
  )

  expect_equal(
    json_extract('[-2147483647]', "$[0]"),
    -2147483647L
  )

  some_above <- c('[1]', '[9999999999]')
  expect_snapshot(
    expect_equal(
      json_extract(some_above, "$[0]"),
      c("1", "9999999999")
    )
  )

  expect_snapshot(
    expect_equal(
      json_extract(some_above, "$[0]", bigint_as_char = FALSE),
      bit64::as.integer64(c("1", "9999999999"))
    )
  )

  int64 <- bit64::integer64()
  expect_snapshot_error(
    json_extract(some_above, "$[0]", ptype = int64),
    class = "jsontools_error"
  )

  expect_snapshot_output(
    expect_equal(
      json_extract(some_above, "$[0]", ptype = int64, bigint_as_char = FALSE),
      bit64::as.integer64(c("1", "9999999999"))
    )
  )

  char <- character()
  expect_equal(
    json_extract(some_above, "$[0]", ptype = char),
    c("1", "9999999999")
  )

  expect_snapshot_error(
    json_extract(some_above, "$[0]", ptype = char, bigint_as_char = FALSE)
  )
})
