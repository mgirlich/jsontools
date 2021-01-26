# json_extract handles mixed types

    Can't combine `integer` <integer> and `text` <character>.

# json_extract handles missing elements

    `path` does not always exist.
    i Did you provide an incorrect path?
    i With `default` you can specify a default value for missing elements.

---

    `path` does not always exist.
    i Did you provide an incorrect path?
    i With `default` you can specify a default value for missing elements.

# json_extract checks `default`

    not possible to assign `default`
    x invalid JSON at 1 locations:
      1
    
    1: offset 2
    parse error: trailing garbage
                                          1]
                         (right here) ------^
    

---

    not possible to assign `default`
    x invalid JSON at 1 locations:
      1
    
    1: offset 1
    lexical error: invalid char in json text.
                                           a
                         (right here) ------^
    

---

    `default` must be NULL or have length 1

# json_extract checks path

    `path` must be a character vector of length 1

---

    `path` must be a character vector of length 1

---

    `path` must be a character vector of length 1

