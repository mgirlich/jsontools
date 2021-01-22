# json_flatten errors

    Cannot combine JSON array/object with scalar values.
    i Use `wrap_scalars = TRUE` to wrap scalars in an array.
    i Use `ptype = character()` to return result as text.

---

    Cannot combine JSON array/object with scalar values.
    i Use `wrap_scalars = TRUE` to wrap scalars in an array.
    i Use `ptype = character()` to return result as text.

---

    Can't combine `integer` <integer> and `text` <character>.

# json_each works

    Code
      vec_cbind(df, vec_slice(out, idx))
    Output
      # A tibble: 6 x 8
        description    json            row_id value type  key   col_type name 
        <chr>          <chr>            <int> <lgl> <chr> <chr> <chr>    <chr>
      1 NA              <NA>                1 NA    null  <NA>  null     ""   
      2 empty array    "[]"                NA NA    <NA>  <NA>  <NA>      <NA>
      3 empty object   "{}"                NA NA    <NA>  <NA>  <NA>      <NA>
      4 json null      "null"               4 NA    null  <NA>  null     ""   
      5 array w/ null  "[null]"             5 NA    null  0     array    ""   
      6 object w/ null "{\"a\": null}"      6 NA    null  a     object   ""   

# json_unnest_longer with discog_json

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["item"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["json2", "json", "vctrs_vctr", "character"]
            }
          },
          "value": ["{\"instance_id\":354823933,\"date_added\":\"2019-02-16T17:48:59-08:00\",\"basic_information\":{\"labels\":[{\"name\":\"Tobi Records (2)\",\"entity_type\":\"1\",\"catno\":\"TOB-013\",\"resource_url\":\"https://api.discogs.com/labels/633407\",\"id\":633407,\"entity_type_name\":\"Label\"}],\"year\":2015,\"master_url\":null,\"artists\":[{\"join\":\"\",\"name\":\"Mollot\",\"anv\":\"\",\"tracks\":\"\",\"role\":\"\",\"resource_url\":\"https://api.discogs.com/artists/4619796\",\"id\":4619796}],\"id\":7496378,\"thumb\":\"https://img.discogs.com/vEVegHrMNTsP6xG_K6OuFXz4h_U=/fit-in/150x150/filters:strip_icc():format(jpeg):mode_rgb():quality(40)/discogs-images/R-7496378-1442692247-1195.jpeg.jpg\",\"title\":\"Demo\",\"formats\":[{\"descriptions\":[\"Numbered\"],\"text\":\"Black\",\"name\":\"Cassette\",\"qty\":\"1\"}],\"cover_image\":\"https://img.discogs.com/EmbMh7vsElksjRgoXLFSuY1sjRQ=/fit-in/500x499/filters:strip_icc():format(jpeg):mode_rgb():quality(90)/discogs-images/R-7496378-1442692247-1195.jpeg.jpg\",\"resource_url\":\"https://api.discogs.com/releases/7496378\",\"master_id\":0},\"id\":7496378,\"rating\":0}", "{\"instance_id\":354092601,\"date_added\":\"2019-02-13T14:13:11-08:00\",\"basic_information\":{\"labels\":[{\"name\":\"La Vida Es Un Mus\",\"entity_type\":\"1\",\"catno\":\"Mus70\",\"resource_url\":\"https://api.discogs.com/labels/38322\",\"id\":38322,\"entity_type_name\":\"Label\"}],\"year\":2013,\"master_url\":\"https://api.discogs.com/masters/553057\",\"artists\":[{\"join\":\"\",\"name\":\"Una Bèstia Incontrolable\",\"anv\":\"\",\"tracks\":\"\",\"role\":\"\",\"resource_url\":\"https://api.discogs.com/artists/3192745\",\"id\":3192745}],\"id\":4490852,\"thumb\":\"https://img.discogs.com/nRZbcvVNygyNFKg5IjUd5YesF_c=/fit-in/150x150/filters:strip_icc():format(jpeg):mode_rgb():quality(40)/discogs-images/R-4490852-1419726473-4388.jpeg.jpg\",\"title\":\"Observant Com El Mon Es Destrueix\",\"formats\":[{\"descriptions\":[\"LP\"],\"name\":\"Vinyl\",\"qty\":\"1\"}],\"cover_image\":\"https://img.discogs.com/eBbQ9jaiHKFeSXLwBEbxq6nl8Wo=/fit-in/600x600/filters:strip_icc():format(jpeg):mode_rgb():quality(90)/discogs-images/R-4490852-1419726473-4388.jpeg.jpg\",\"resource_url\":\"https://api.discogs.com/releases/4490852\",\"master_id\":553057},\"id\":4490852,\"rating\":0}", "{\"instance_id\":354091476,\"date_added\":\"2019-02-13T14:07:23-08:00\",\"basic_information\":{\"labels\":[{\"name\":\"La Vida Es Un Mus\",\"entity_type\":\"1\",\"catno\":\"MUS118\",\"resource_url\":\"https://api.discogs.com/labels/38322\",\"id\":38322,\"entity_type_name\":\"Label\"}],\"year\":2017,\"master_url\":\"https://api.discogs.com/masters/1109943\",\"artists\":[{\"join\":\"\",\"name\":\"S.H.I.T. (3)\",\"anv\":\"\",\"tracks\":\"\",\"role\":\"\",\"resource_url\":\"https://api.discogs.com/artists/2769828\",\"id\":2769828}],\"id\":9827276,\"thumb\":\"https://img.discogs.com/x6GUri3hXAcfzF2wz5jQloomOoY=/fit-in/150x150/filters:strip_icc():format(jpeg):mode_rgb():quality(40)/discogs-images/R-9827276-1488380364-7942.jpeg.jpg\",\"title\":\"I\",\"formats\":[{\"descriptions\":[\"7\\\"\",\"45 RPM\",\"EP\"],\"name\":\"Vinyl\",\"qty\":\"1\"}],\"cover_image\":\"https://img.discogs.com/7aJPlo2phtFL-T2Kt6MTBc0uftY=/fit-in/600x600/filters:strip_icc():format(jpeg):mode_rgb():quality(90)/discogs-images/R-9827276-1488380364-7942.jpeg.jpg\",\"resource_url\":\"https://api.discogs.com/releases/9827276\",\"master_id\":1109943},\"id\":9827276,\"rating\":0}"]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["artists", "component_id"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["json2", "json", "vctrs_vctr", "character"]
            }
          },
          "value": ["{\"join\":\"\",\"name\":\"Mollot\",\"anv\":\"\",\"tracks\":\"\",\"role\":\"\",\"resource_url\":\"https://api.discogs.com/artists/4619796\",\"id\":4619796}", "{\"join\":\"\",\"name\":\"Una Bèstia Incontrolable\",\"anv\":\"\",\"tracks\":\"\",\"role\":\"\",\"resource_url\":\"https://api.discogs.com/artists/3192745\",\"id\":3192745}", "{\"join\":\"\",\"name\":\"S.H.I.T. (3)\",\"anv\":\"\",\"tracks\":\"\",\"role\":\"\",\"resource_url\":\"https://api.discogs.com/artists/2769828\",\"id\":2769828}"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3]
        }
      ]
    }

# json_unnest_wider errors on non-objects

    every element of `col` must be a json object

# json_unnest_wider with discog_json

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["item"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["json2", "json", "vctrs_vctr", "character"]
            }
          },
          "value": ["{\"instance_id\":354823933,\"date_added\":\"2019-02-16T17:48:59-08:00\",\"basic_information\":{\"labels\":[{\"name\":\"Tobi Records (2)\",\"entity_type\":\"1\",\"catno\":\"TOB-013\",\"resource_url\":\"https://api.discogs.com/labels/633407\",\"id\":633407,\"entity_type_name\":\"Label\"}],\"year\":2015,\"master_url\":null,\"artists\":[{\"join\":\"\",\"name\":\"Mollot\",\"anv\":\"\",\"tracks\":\"\",\"role\":\"\",\"resource_url\":\"https://api.discogs.com/artists/4619796\",\"id\":4619796}],\"id\":7496378,\"thumb\":\"https://img.discogs.com/vEVegHrMNTsP6xG_K6OuFXz4h_U=/fit-in/150x150/filters:strip_icc():format(jpeg):mode_rgb():quality(40)/discogs-images/R-7496378-1442692247-1195.jpeg.jpg\",\"title\":\"Demo\",\"formats\":[{\"descriptions\":[\"Numbered\"],\"text\":\"Black\",\"name\":\"Cassette\",\"qty\":\"1\"}],\"cover_image\":\"https://img.discogs.com/EmbMh7vsElksjRgoXLFSuY1sjRQ=/fit-in/500x499/filters:strip_icc():format(jpeg):mode_rgb():quality(90)/discogs-images/R-7496378-1442692247-1195.jpeg.jpg\",\"resource_url\":\"https://api.discogs.com/releases/7496378\",\"master_id\":0},\"id\":7496378,\"rating\":0}", "{\"instance_id\":354092601,\"date_added\":\"2019-02-13T14:13:11-08:00\",\"basic_information\":{\"labels\":[{\"name\":\"La Vida Es Un Mus\",\"entity_type\":\"1\",\"catno\":\"Mus70\",\"resource_url\":\"https://api.discogs.com/labels/38322\",\"id\":38322,\"entity_type_name\":\"Label\"}],\"year\":2013,\"master_url\":\"https://api.discogs.com/masters/553057\",\"artists\":[{\"join\":\"\",\"name\":\"Una Bèstia Incontrolable\",\"anv\":\"\",\"tracks\":\"\",\"role\":\"\",\"resource_url\":\"https://api.discogs.com/artists/3192745\",\"id\":3192745}],\"id\":4490852,\"thumb\":\"https://img.discogs.com/nRZbcvVNygyNFKg5IjUd5YesF_c=/fit-in/150x150/filters:strip_icc():format(jpeg):mode_rgb():quality(40)/discogs-images/R-4490852-1419726473-4388.jpeg.jpg\",\"title\":\"Observant Com El Mon Es Destrueix\",\"formats\":[{\"descriptions\":[\"LP\"],\"name\":\"Vinyl\",\"qty\":\"1\"}],\"cover_image\":\"https://img.discogs.com/eBbQ9jaiHKFeSXLwBEbxq6nl8Wo=/fit-in/600x600/filters:strip_icc():format(jpeg):mode_rgb():quality(90)/discogs-images/R-4490852-1419726473-4388.jpeg.jpg\",\"resource_url\":\"https://api.discogs.com/releases/4490852\",\"master_id\":553057},\"id\":4490852,\"rating\":0}", "{\"instance_id\":354091476,\"date_added\":\"2019-02-13T14:07:23-08:00\",\"basic_information\":{\"labels\":[{\"name\":\"La Vida Es Un Mus\",\"entity_type\":\"1\",\"catno\":\"MUS118\",\"resource_url\":\"https://api.discogs.com/labels/38322\",\"id\":38322,\"entity_type_name\":\"Label\"}],\"year\":2017,\"master_url\":\"https://api.discogs.com/masters/1109943\",\"artists\":[{\"join\":\"\",\"name\":\"S.H.I.T. (3)\",\"anv\":\"\",\"tracks\":\"\",\"role\":\"\",\"resource_url\":\"https://api.discogs.com/artists/2769828\",\"id\":2769828}],\"id\":9827276,\"thumb\":\"https://img.discogs.com/x6GUri3hXAcfzF2wz5jQloomOoY=/fit-in/150x150/filters:strip_icc():format(jpeg):mode_rgb():quality(40)/discogs-images/R-9827276-1488380364-7942.jpeg.jpg\",\"title\":\"I\",\"formats\":[{\"descriptions\":[\"7\\\"\",\"45 RPM\",\"EP\"],\"name\":\"Vinyl\",\"qty\":\"1\"}],\"cover_image\":\"https://img.discogs.com/7aJPlo2phtFL-T2Kt6MTBc0uftY=/fit-in/600x600/filters:strip_icc():format(jpeg):mode_rgb():quality(90)/discogs-images/R-9827276-1488380364-7942.jpeg.jpg\",\"resource_url\":\"https://api.discogs.com/releases/9827276\",\"master_id\":1109943},\"id\":9827276,\"rating\":0}"]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["labels", "year", "master_url", "artists", "id", "thumb", "title", "formats", "cover_image", "resource_url", "master_id"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["json2", "json", "vctrs_vctr", "character"]
            }
          },
          "value": ["[{\"name\":\"Tobi Records (2)\",\"entity_type\":\"1\",\"catno\":\"TOB-013\",\"resource_url\":\"https://api.discogs.com/labels/633407\",\"id\":633407,\"entity_type_name\":\"Label\"}]", "[{\"name\":\"La Vida Es Un Mus\",\"entity_type\":\"1\",\"catno\":\"Mus70\",\"resource_url\":\"https://api.discogs.com/labels/38322\",\"id\":38322,\"entity_type_name\":\"Label\"}]", "[{\"name\":\"La Vida Es Un Mus\",\"entity_type\":\"1\",\"catno\":\"MUS118\",\"resource_url\":\"https://api.discogs.com/labels/38322\",\"id\":38322,\"entity_type_name\":\"Label\"}]"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [2015, 2013, 2017]
        },
        {
          "type": "character",
          "attributes": {},
          "value": [null, "https://api.discogs.com/masters/553057", "https://api.discogs.com/masters/1109943"]
        },
        {
          "type": "character",
          "attributes": {
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["json2", "json", "vctrs_vctr", "character"]
            }
          },
          "value": ["[{\"join\":\"\",\"name\":\"Mollot\",\"anv\":\"\",\"tracks\":\"\",\"role\":\"\",\"resource_url\":\"https://api.discogs.com/artists/4619796\",\"id\":4619796}]", "[{\"join\":\"\",\"name\":\"Una Bèstia Incontrolable\",\"anv\":\"\",\"tracks\":\"\",\"role\":\"\",\"resource_url\":\"https://api.discogs.com/artists/3192745\",\"id\":3192745}]", "[{\"join\":\"\",\"name\":\"S.H.I.T. (3)\",\"anv\":\"\",\"tracks\":\"\",\"role\":\"\",\"resource_url\":\"https://api.discogs.com/artists/2769828\",\"id\":2769828}]"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [7496378, 4490852, 9827276]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["https://img.discogs.com/vEVegHrMNTsP6xG_K6OuFXz4h_U=/fit-in/150x150/filters:strip_icc():format(jpeg):mode_rgb():quality(40)/discogs-images/R-7496378-1442692247-1195.jpeg.jpg", "https://img.discogs.com/nRZbcvVNygyNFKg5IjUd5YesF_c=/fit-in/150x150/filters:strip_icc():format(jpeg):mode_rgb():quality(40)/discogs-images/R-4490852-1419726473-4388.jpeg.jpg", "https://img.discogs.com/x6GUri3hXAcfzF2wz5jQloomOoY=/fit-in/150x150/filters:strip_icc():format(jpeg):mode_rgb():quality(40)/discogs-images/R-9827276-1488380364-7942.jpeg.jpg"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Demo", "Observant Com El Mon Es Destrueix", "I"]
        },
        {
          "type": "character",
          "attributes": {
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["json2", "json", "vctrs_vctr", "character"]
            }
          },
          "value": ["[{\"descriptions\":[\"Numbered\"],\"text\":\"Black\",\"name\":\"Cassette\",\"qty\":\"1\"}]", "[{\"descriptions\":[\"LP\"],\"name\":\"Vinyl\",\"qty\":\"1\"}]", "[{\"descriptions\":[\"7\\\"\",\"45 RPM\",\"EP\"],\"name\":\"Vinyl\",\"qty\":\"1\"}]"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["https://img.discogs.com/EmbMh7vsElksjRgoXLFSuY1sjRQ=/fit-in/500x499/filters:strip_icc():format(jpeg):mode_rgb():quality(90)/discogs-images/R-7496378-1442692247-1195.jpeg.jpg", "https://img.discogs.com/eBbQ9jaiHKFeSXLwBEbxq6nl8Wo=/fit-in/600x600/filters:strip_icc():format(jpeg):mode_rgb():quality(90)/discogs-images/R-4490852-1419726473-4388.jpeg.jpg", "https://img.discogs.com/7aJPlo2phtFL-T2Kt6MTBc0uftY=/fit-in/600x600/filters:strip_icc():format(jpeg):mode_rgb():quality(90)/discogs-images/R-9827276-1488380364-7942.jpeg.jpg"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["https://api.discogs.com/releases/7496378", "https://api.discogs.com/releases/4490852", "https://api.discogs.com/releases/9827276"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 553057, 1109943]
        }
      ]
    }

# json_unnest_wider can wrap scalars

    Issue when extracting key `a`
    * Cannot combine JSON array/object with scalar values.
    i Use `wrap_scalars = TRUE` to wrap scalars in an array.
    i Use `ptype = character()` to return result as text.

