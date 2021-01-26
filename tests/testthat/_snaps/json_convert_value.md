# json_ptype_common incompatible types

    Can't combine `logical` <logical> and `text` <character>.

---

    Can't combine `logical` <logical> and `text` <character>.

---

    Can't combine `integer` <integer> and `text` <character>.

---

    Can't combine `real` <double> and `text` <character>.

---

    Cannot combine JSON array/object with scalar values.
    i Use `wrap_scalars = TRUE` to wrap scalars in an array.
    i Use `ptype = character()` to return result as text.

---

    Cannot combine JSON array/object with scalar values.
    i Use `wrap_scalars = TRUE` to wrap scalars in an array.
    i Use `ptype = character()` to return result as text.

---

    Cannot combine JSON array/object with scalar values.
    i Use `wrap_scalars = TRUE` to wrap scalars in an array.
    i Use `ptype = character()` to return result as text.

---

    Cannot combine JSON array/object with scalar values.
    i Use `wrap_scalars = TRUE` to wrap scalars in an array.
    i Use `ptype = character()` to return result as text.

# json_ptype_common incompatible ptype

    Not all elements are arrays.

---

    Not all elements are objects.

# json_vec_c handles mix of array/object and text

    Cannot combine JSON array/object with scalar values.
    i Use `wrap_scalars = TRUE` to wrap scalars in an array.
    i Use `ptype = character()` to return result as text.

---

    Cannot combine JSON array/object with scalar values.
    i Use `wrap_scalars = TRUE` to wrap scalars in an array.
    i Use `ptype = character()` to return result as text.

# json_vec_c handles json (array/object) ptype

    Not all elements are arrays.

---

    Not all elements are objects.

# json_convert_values errors for incompatible types

    Can't combine `integer` <integer> and `text` <character>.

---

    Can't convert <double> to <character>.

# json_convert_value can handle objects

    Not all elements are objects.

---

    Not all elements are objects or arrays.

# json_convert_value handles big integers

    Code
      expect_equal(json_convert_value(x = c("9999999999", "1"), json_types = c(
        "integer", "true"), ptype = NULL, bigint_as_char = TRUE), c("9999999999", "1"))
    Message <simpleMessage>
      big integers found and converted to character.

---

    Can't combine <character> and <integer>.

