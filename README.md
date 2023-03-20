
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jsontools

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/jsontools)](https://CRAN.R-project.org/package=jsontools)
<!-- badges: end -->

## Overview

With the increasing support of JSON in databases and since 2016 even
support in the SQL standard, JSON data in vectors become more common.
With jsontools one can easily work with JSON vectors in R.

The three main parts are:

1.  parsing from JSON and converting to JSON
2.  extract data from JSON without parsing everything
3.  manipulate JSON directly

## Installation

``` r
install.packages("jsontools")

# Or the the development version from GitHub:
devtools::install_github("mgirlich/jsontools")
```

## Overview

``` r
library(jsontools)
got_json <- got_chars_json
```

First, let’s find out what’s the type of the JSON

``` r
json_type(got_json)
#> [1] "array"
```

With `json_array_length()` and `json_array_types()` we can find out the
number of elements in the array and the type of each element:

``` r
json_array_length(got_json)
#> [1] 5
json_array_types(got_json)
#> [1] "object" "object" "object" "object" "object"
```

Each element of the array is a JSON object. To work with the data in the
array it is often more convenient to unpack it with `json_flatten()`.
This produces a JSON vector whose elements are the elements of the
array:

``` r
json_flatten(got_json)
#> {"url":"https://www.anapioficeandfire.com/api/characters/1022","id":1022,"name":"Theon Greyjoy","alive":true,"titles":["Prince of Winterfell","Captain of Sea Bitch","Lord of the Iron Islands (by law of the green lands)"],"aliases":["Prince of Fools","Theon Turncloak","Reek","Theon Kinslayer"],"allegiances":"House Greyjoy of Pyke"}
#> {"url":"https://www.anapioficeandfire.com/api/characters/1052","id":1052,"name":"Tyrion Lannister","alive":true,"titles":["Acting Hand of the King (former)","Master of Coin (former)"],"aliases":["The Imp","Halfman","The boyman","Giant of Lannister","Lord Tywin's Doom","Lord Tywin's Bane","Yollo","Hugor Hill","No-Nose","Freak","Dwarf"],"allegiances":"House Lannister of Casterly Rock"}
#> {"url":"https://www.anapioficeandfire.com/api/characters/1074","id":1074,"name":"Victarion Greyjoy","alive":true,"titles":["Lord Captain of the Iron Fleet","Master of the Iron Victory"],"aliases":"The Iron Captain","allegiances":"House Greyjoy of Pyke"}
#> {"url":"https://www.anapioficeandfire.com/api/characters/1109","id":1109,"name":"Will","alive":false,"titles":"","aliases":"","allegiances":[]}
#> {"url":"https://www.anapioficeandfire.com/api/characters/1166","id":1166,"name":"Areo Hotah","alive":true,"titles":"Captain of the Guard at Sunspear","aliases":"","allegiances":"House Nymeros Martell of Sunspear"}
```

In non-interactive use it is a good idea to specify the type you expect
via the `ptype` argument. In this case we expect each element to be a
JSON object and therefore use `ptype = new_json_object()`.

``` r
got_chars <- json_flatten(got_json, ptype = new_json_object())
```

An alternative would have been to use `ptype = json2()` when we don’t
care whether the elements are arrays, objects, or a mix of them.

## Extract Data

Let’s have a look at the first Game of Thrones character

``` r
got_chars[1]
#> {"url":"https://www.anapioficeandfire.com/api/characters/1022","id":1022,"name":"Theon Greyjoy","alive":true,"titles":["Prince of Winterfell","Captain of Sea Bitch","Lord of the Iron Islands (by law of the green lands)"],"aliases":["Prince of Fools","Theon Turncloak","Reek","Theon Kinslayer"],"allegiances":"House Greyjoy of Pyke"}
```

This minified JSON is not so easy to read so we prettify it with
`json_prettify()`

``` r
json_prettify(got_chars[1])
#> {
#>    "url": "https://www.anapioficeandfire.com/api/characters/1022",
#>    "id": 1022,
#>    "name": "Theon Greyjoy",
#>    "alive": true,
#>    "titles": [
#>       "Prince of Winterfell",
#>       "Captain of Sea Bitch",
#>       "Lord of the Iron Islands (by law of the green lands)"
#>    ],
#>    "aliases": [
#>       "Prince of Fools",
#>       "Theon Turncloak",
#>       "Reek",
#>       "Theon Kinslayer"
#>    ],
#>    "allegiances": "House Greyjoy of Pyke"
#> }
```

With the help of `json_extract()` we can now easily extract values from
each character:

``` r
json_extract(got_chars, "$.name")
#> [1] "Theon Greyjoy"     "Tyrion Lannister"  "Victarion Greyjoy"
#> [4] "Will"              "Areo Hotah"
```

The second argument `"$.name"` specifies the `path` of the element we
want to extract. The basic syntax is relatively simple:

- `$.name` to extract the attribute `name` of a JSON object.
- `$[0]` to extract the first element of a JSON array. Keep in mind that
  the index here starts with 0 unlike in `R`!
- `$[last]` to extract the last element.

If you wonder about the dollar sign `$`: it stands for the current
element. Simply always start the path with it and you will be fine.

To extract elements of nested objects you simply combine the path. For
example the path to `1` in `{"a": {"b": [1, 2]}}` is `$.a.b[0]`. You can
find more examples in article [JSONpath - XPath for
JSON](https://goessner.net/articles/JsonPath/index.html#e2) by Stefan
Gössner.

We can now construct a tibble with some basic information about the
characters

``` r
tibble::tibble(
  id = json_extract(got_chars, "$.id"),
  name = json_extract(got_chars, "$.name"),
  alive = json_extract(got_chars, "$.alive")
)
#> # A tibble: 5 × 3
#>      id name              alive
#>   <int> <chr>             <lgl>
#> 1  1022 Theon Greyjoy     TRUE 
#> 2  1052 Tyrion Lannister  TRUE 
#> 3  1074 Victarion Greyjoy TRUE 
#> 4  1109 Will              FALSE
#> 5  1166 Areo Hotah        TRUE
```

Oh, we actually also wanted the titles of each character

``` r
tibble::tibble(
  id = json_extract(got_chars, "$.id"),
  name = json_extract(got_chars, "$.name"),
  alive = json_extract(got_chars, "$.alive"),
  titles = json_extract(got_chars, "$.titles")
)
#> Error in `json_ptype_common()`:
#> ! ✖ Cannot combine JSON array/object with scalar values.
#> ℹ Use `wrap_scalars = TRUE` to wrap scalars in an array.
#> ℹ Use `ptype = character()` to return result as text.
```

Unfortunately, we get an error message saying that we cannot combine an
array with text. Let’s try extracting the books as text:

``` r
json_extract(got_chars, "$.titles", ptype = character())
#> [1] "[\"Prince of Winterfell\",\"Captain of Sea Bitch\",\"Lord of the Iron Islands (by law of the green lands)\"]"
#> [2] "[\"Acting Hand of the King (former)\",\"Master of Coin (former)\"]"                                          
#> [3] "[\"Lord Captain of the Iron Fleet\",\"Master of the Iron Victory\"]"                                         
#> [4] ""                                                                                                            
#> [5] "Captain of the Guard at Sunspear"
```

We see that the API actually isn’t type stable: it returns a simple text
value instead of an array if the character only has one title. It is
usually not a good idea to mix characters and arrays in an API. But as
some APIs in the wild do this you can fix it with the argument
`wrap_scalars` and get only JSON arrays back:

``` r
json_extract(got_chars, "$.titles", wrap_scalars = TRUE)
#> ["Prince of Winterfell","Captain of Sea Bitch","Lord of the Iron Islands (by law of the green lands)"]
#> ["Acting Hand of the King (former)","Master of Coin (former)"]
#> ["Lord Captain of the Iron Fleet","Master of the Iron Victory"]
#> [""]
#> ["Captain of the Guard at Sunspear"]
```

## Unnesting

It would be quite tedious to extract every field of a character like
that. Instead you can simply use `json_unnest_wider()` to convert the
keys of a JSON object into columns. Like before, we also have to use
`wrap_scalars = TRUE`:

``` r
got_chars_df <- tibble::tibble(chars_json = got_chars) %>%
  json_unnest_wider(chars_json, wrap_scalars = TRUE)

got_chars_df
#> # A tibble: 5 × 7
#>   url                            id name  alive     titles    aliases allegian…¹
#>   <chr>                       <int> <chr> <lgl>    <json2>    <json2>    <json2>
#> 1 https://www.anapioficeandf…  1022 Theo… TRUE  ["Prince … ["Prince … ["House G…
#> 2 https://www.anapioficeandf…  1052 Tyri… TRUE  ["Acting … ["The Imp… ["House L…
#> 3 https://www.anapioficeandf…  1074 Vict… TRUE  ["Lord Ca… ["The Iro… ["House G…
#> 4 https://www.anapioficeandf…  1109 Will  FALSE       [""]       [""]         []
#> 5 https://www.anapioficeandf…  1166 Areo… TRUE  ["Captain…       [""] ["House N…
#> # … with abbreviated variable name ¹​allegiances
```

We can also unnest arrays with `json_unnest_longer()`. This is basically
a version `json_flatten()` for data frames

``` r
got_chars_df[c("id", "name", "titles")] %>% 
  json_unnest_longer(titles)
#> # A tibble: 9 × 3
#>      id name              titles                                                
#>   <int> <chr>             <chr>                                                 
#> 1  1022 Theon Greyjoy     "Prince of Winterfell"                                
#> 2  1022 Theon Greyjoy     "Captain of Sea Bitch"                                
#> 3  1022 Theon Greyjoy     "Lord of the Iron Islands (by law of the green lands)"
#> 4  1052 Tyrion Lannister  "Acting Hand of the King (former)"                    
#> 5  1052 Tyrion Lannister  "Master of Coin (former)"                             
#> 6  1074 Victarion Greyjoy "Lord Captain of the Iron Fleet"                      
#> 7  1074 Victarion Greyjoy "Master of the Iron Victory"                          
#> 8  1109 Will              ""                                                    
#> 9  1166 Areo Hotah        "Captain of the Guard at Sunspear"
```

## Modify Data

It is also possible to modify the JSON directly. With `json_delete()` we
can delete multiple fields in an object

``` r
got_chars_small <- json_delete(got_chars, "$.url", "$.aliases", "$.allegiances")
```

And with `json_mutate()` we can easily mutate elements

``` r
json_mutate(
  got_chars_small,
  .id = 1:5,
  .alive = !json_extract(got_chars_small, "$.alive")
)
#> {"id":1,"name":"Theon Greyjoy","alive":false,"titles":["Prince of Winterfell","Captain of Sea Bitch","Lord of the Iron Islands (by law of the green lands)"]}
#> {"id":2,"name":"Tyrion Lannister","alive":false,"titles":["Acting Hand of the King (former)","Master of Coin (former)"]}
#> {"id":3,"name":"Victarion Greyjoy","alive":false,"titles":["Lord Captain of the Iron Fleet","Master of the Iron Victory"]}
#> {"id":4,"name":"Will","alive":true,"titles":""}
#> {"id":5,"name":"Areo Hotah","alive":false,"titles":"Captain of the Guard at Sunspear"}
```

or patch with another JSON using `json_merge()`

``` r
x <- c(
  '{"id": 1, "a": 3}',
  '{"id": 2, "a": 4}',
  '{"id": 3}'
)

# remove element at "a"
json_merge(x, '{"a": null}')
#> {"id":1}
#> {"id":2}
#> {"id":3}

# it is vectorised
json_merge(
  x,
  c(
    '{"a": null}',
    '{"a": 5}',
    '{"a": 6}'
  )
)
#> {"id":1}
#> {"id":2,"a":5}
#> {"id":3,"a":6}
```
