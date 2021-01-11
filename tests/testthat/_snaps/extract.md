# json_get_value handles long ints

    Code
      expect_equal(json_get_value(y, "$.some_above"), c("1", "9999999999"))
    Message <message>
      bigints found; converted to `character()`.

---

    Code
      expect_equal(json_get_value(y, "$.some_above", bigint_as_char = FALSE), bit64::as.integer64(
        c("1", "9999999999")))
    Message <message>
      bigints found; converted to `bit64::integer64()`.

---

    `bigint_as_char = TRUE` not allowed with `ptype` = `integer64()`.

---

    

---

    Can't combine <character> and <integer64>.

