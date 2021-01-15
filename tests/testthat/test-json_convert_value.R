#
# # json_ptype_common -------------------------------------------------------
#
# test_that("json_ptype_common works", {
#   expect_equal(
#     json_ptype_common(c("true", "false", "null")),
#     logical()
#   )
#
#   expect_equal(
#     json_ptype_common(c("integer", "true", "false", "null")),
#     integer()
#   )
#
#   expect_equal(
#     json_ptype_common(c("real", "integer", "true", "false", "null")),
#     numeric()
#   )
#
#   expect_equal(
#     json_ptype_common(c("text", "null")),
#     character()
#   )
#
#   expect_equal(
#     json_ptype_common(c("array", "null")),
#     new_json_array()
#   )
#
#   expect_equal(
#     json_ptype_common(c("object", "null")),
#     new_json_object()
#   )
#
#   expect_equal(
#     json_ptype_common(c("object", "array", "null")),
#     new_json2()
#   )
# })
#
# test_that("json_ptype_common incompatible types", {
#   # incompatible with text
#   expect_snapshot_error(json_ptype_common(c("true", "text")))
#   expect_snapshot_error(json_ptype_common(c("false", "text")))
#   expect_snapshot_error(json_ptype_common(c("integer", "text")))
#   expect_snapshot_error(json_ptype_common(c("real", "text")))
#   expect_snapshot_error(json_ptype_common(c("object", "text")))
#   expect_snapshot_error(json_ptype_common(c("array", "text")))
#
#   # incompatible with object/array
#   expect_snapshot_error(json_ptype_common(c("object", "true")))
#   expect_snapshot_error(json_ptype_common(c("array", "false")))
# })

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

test_that("json_ptype_common incompatible types", {
  # incompatible with text
  expect_snapshot_error(json_ptype_common(c("true", "text")))
  expect_snapshot_error(json_ptype_common(c("false", "text")))
  expect_snapshot_error(json_ptype_common(c("integer", "text")))
  expect_snapshot_error(json_ptype_common(c("real", "text")))
  expect_snapshot_error(json_ptype_common(c("object", "text")))
  expect_snapshot_error(json_ptype_common(c("array", "text")))

  # incompatible with object/array
  expect_snapshot_error(json_ptype_common(c("object", "true")))
  expect_snapshot_error(json_ptype_common(c("array", "false")))
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

test_that("json_convert_values errors for incompatible types", {
  expect_snapshot_error(
    json_convert_value(
      x = c("1", "a"),
      json_types = c("integer", "text"),
      ptype = NULL
    )
  )

  expect_snapshot_error(
    json_convert_value(
      x = c("1", "a"),
      json_types = c("integer", "text"),
      ptype = character()
    )
  )
})

test_that("json_convert_value works arrays and objects", {
  expect_equal(
    json_convert_value(
      x = c("[1, 2]", '["a", "b"]'),
      json_types = c("array", "array"),
      ptype = NULL
    ),
    new_json2(c("[1, 2]", '["a", "b"]'))
    # new_json_array(c("[1, 2]", '["a", "b"]'))
  )

  expect_equal(
    json_convert_value(
      x = c('{"a": 1}', '{"a": 2}'),
      json_types = c("object", "object"),
      ptype = NULL
    ),
    new_json2(c('{"a": 1}', '{"a": 2}'))
    # new_json_object(c('{"a": 1}', '{"a": 2}'))
  )
})

test_that("json_convert_value doesn't convert arrays to objects", {
  expect_snapshot_error(
    json_convert_value(
      x = "[1, 2]",
      json_types = "array",
      ptype = new_json_object()
    )
  )

  expect_snapshot_error(
    json_convert_value(
      x = '{"a": 1}',
      json_types = "object",
      ptype = new_json_array()
    )
  )
})

test_that("json_convert_value works with mix of arrays and objects", {
  expect_equal(
    json_convert_value(
      x = c("[1, 2]", '{"a": 1}'),
      json_types = c("array", "object"),
      ptype = new_json2()
    ),
    new_json2(c("[1, 2]", '{"a": 1}'))
  )

  expect_equal(
    json_convert_value(
      x = c("[1, 2]", '{"a": 1}'),
      json_types = c("array", "object"),
      ptype = NULL
    ),
    new_json2(c("[1, 2]", '{"a": 1}'))
  )
})

test_that("json_convert_value handles array/object and text without wrap_scalars", {
  expect_snapshot_error(
    json_convert_value(
      x = c("[1, 2]", '["a", "b"]'),
      json_types = c("array", "text"),
      ptype = NULL
    )
  )

  expect_snapshot_error(
    json_convert_value(
      x = c("[1, 2]", '{"a": 1}'),
      json_types = c("array", "text"),
      ptype = new_json_array()
    )
  )

  expect_equal(
    json_convert_value(
      x = c("[1, 2]", '{"a": 1}'),
      json_types = c("array", "text"),
      ptype = character()
    ),
    c("[1, 2]", '{"a": 1}')
  )
})

test_that("json_convert_value handles array/object and text with wrap_scalars", {
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
})
