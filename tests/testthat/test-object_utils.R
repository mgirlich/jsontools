test_that("json_build_object works", {
  x <- json2(c(
    '{"a": 1, "b": {"x": "a"}}',
    NA,
    '{"a": 3, "b": {"x": "c"}}'
  ))
  value <- "test"

  expect_equal(
    json_object_extract(
      x,
      first = .a,
      chr = "chr",
      bangbang = !!value,
      id = 1:3
    ),
    json2(
      c(
        '{"first":1,"chr":"chr","bangbang":"test","id":1}',
        '{"first":null,"chr":"chr","bangbang":"test","id":2}',
        '{"first":3,"chr":"chr","bangbang":"test","id":3}'
      )
    )
  )

  expect_equal(
    json_object_extract(),
    json2()
  )
})
