# json_each works

    Code
      vec_cbind(df, vec_slice(out, idx))
    Output
      # A tibble: 6 x 8
        description    json            row_id value type  key   col_type name 
        <chr>          <chr>            <int> <lgl> <chr> <chr> <chr>    <chr>
      1 NA              <NA>                1 NA    null  <NA>  null     ""   
      2 empty array    "[]"                NA NA    <NA>  <NA>  <NA>     <NA> 
      3 empty object   "{}"                NA NA    <NA>  <NA>  <NA>     <NA> 
      4 json null      "null"               4 NA    null  <NA>  null     ""   
      5 array w/ null  "[null]"             5 NA    null  0     array    ""   
      6 object w/ null "{\"a\": null}"      6 NA    null  a     object   ""   

# json_flatten errors

    x Cannot combine JSON array/object with scalar values.
    i Use `wrap_scalars = TRUE` to wrap scalars in an array.
    i Use `ptype = character()` to return result as text.

---

    x Cannot combine JSON array/object with scalar values.
    i Use `wrap_scalars = TRUE` to wrap scalars in an array.
    i Use `ptype = character()` to return result as text.

---

    Can't combine `integer` <integer> and `text` <character>.

