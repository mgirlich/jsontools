json_convert_value <- function(x, json_types, ptype, bigint_as_char = TRUE) {
  # TODO replace na
  # TODO think about json_object and json_array

  stopifnot(is_bool(bigint_as_char))
  stopifnot(length(x) == length(json_types))
  # TODO check json_types are valid?

  x_parsed <- convert_json_type(x, json_types, bigint_as_char = bigint_as_char)
  bigint_found <- is_true(attr(x_parsed, "bigint"))

  if (identical(ptype, list())) {
    return(x_parsed)
  }

  # replace NULLs with NA
  x_parsed[json_types == "null"] <- NA
  # replace missing paths & values from NA with NA
  x_parsed[is.na(json_types)] <- NA

  if (bigint_as_char && bit64::is.integer64(ptype)) {
    stop_jsontools("`bigint_as_char = TRUE` not allowed with `ptype` = `integer64()`.")
  }

  json_ptype <- json_ptype_common(json_types)

  if (bigint_found) {
    if (is.null(ptype)) {
      if (bigint_as_char) {
        inform("bigints found; converted to `character()`.")
      } else {
        inform("bigints found; converted to `bit64::integer64()`.")
      }
    }

    # TODO what about json_type real?
    # -> probably a bad idea anyway...
    stopifnot(identical(json_ptype, integer()))

    if (bigint_as_char) {
      # convert logicals manually to integer as they otherwise become "true"/"false"
      is_lgl <- json_types %in% c("true", "false")
      x_parsed[is_lgl] <- as.integer(x_parsed[is_lgl])
      json_ptype <- character()
    } else {
      json_ptype <- bit64::integer64()
    }
  }

  if (inherits(json_ptype, "jsontools_json_object") ||
    inherits(json_ptype, "jsontools_json_array")) {
    ptype <- vec_ptype2(ptype, character())
    new_json2(vec_c(!!!x_parsed, .ptype = ptype))
  } else {
    ptype <- vec_ptype2(ptype, json_ptype)
    vec_c(!!!x_parsed, .ptype = ptype)
  }
}

convert_json_type <- function(x, json_types, bigint_as_char = TRUE) {
  x_parsed <- as.list(x)
  not_na <- !is.na(json_types)

  int_idx <- json_types == c("integer") & not_na

  x_parsed[int_idx] <- suppressWarnings(as.integer(x[int_idx]))
  if (any(is.na(x_parsed[int_idx]))) {
    if (bigint_as_char) {
      x_parsed[int_idx] <- x[int_idx]
    } else {
      x_parsed[int_idx] <- vec_chop(bit64::as.integer64(x[int_idx]))
    }

    attr(x_parsed, "bigint") <- TRUE
  }

  real_idx <- json_types == c("real") & not_na
  x_parsed[real_idx] <- as.numeric(x[real_idx])

  real_idx <- json_types == c("null") & not_na
  x_parsed[real_idx] <- list(NULL)

  lgl_idx <- json_types %in% c("true", "false") & not_na
  x_parsed[lgl_idx] <- x[lgl_idx] == "1"

  # leave text, object, and array as text
  x_parsed
}


json_ptype_common <- function(types) {
  type_map <- list(
    object = new_json_object(),
    array = new_json_array(),
    integer = integer(),
    real = numeric(),
    true = logical(),
    false = logical(),
    null = NULL,
    text = character()
  )

  types <- unique(types)
  types <- types[types != "null" & !is.na(types)]
  types[types %in% c("true", "false")] <- "logical"
  types <- unique(types)

  if (length(types) == 1) {
    ptype <- types
  } else if (length(types) == 0) {
    ptype <- "null"
  } else {
    if (all(types %in% c("logical", "integer"))) {
      ptype <- "integer"
    } else if (all(types %in% c("logical", "integer", "real"))) {
      ptype <- "real"
    } else {
      stop_jsontools("incompatible types")
    }
  }

  # types_mapped <- type_map[types]
  #
  # vec_ptype_common(!!!types_mapped)

  type_map[[ptype]]
}

json_vec_c <- function(x, types, ptype = NULL) {
  json_ptype <- json_ptype_common(types)

  if (inherits(json_ptype, "jsontools_json_object") ||
    inherits(json_ptype, "jsontools_json_array")) {
    ptype <- vec_ptype2(ptype, character())
    new_json2(vec_c(!!!x, .ptype = ptype))
  } else {
    ptype <- vec_ptype2(ptype, json_ptype)
    vec_c(!!!x, .ptype = ptype)
  }
}

new_json_object <- function(x = character()) {
  new_vctr(x, class = "jsontools_json_object")
}

new_json_array <- function(x = character()) {
  new_vctr(x, class = "jsontools_json_array")
}
