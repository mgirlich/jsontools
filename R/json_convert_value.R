json_convert_value <- function(x,
                               json_types,
                               ptype,
                               wrap_scalars = FALSE,
                               bigint_as_char = TRUE) {
  # TODO replace na
  x_parsed <- convert_json_type(x, json_types, bigint_as_char = FALSE)

  if (identical(ptype, list())) {
    return(x_parsed)
  }

  # replace NULLs with NA so that `vec_c(!!!x_parsed)` doesn't remove these elements
  x_parsed[json_types == "null"] <- NA
  # replace missing paths & values from NA with NA
  x_parsed[is.na(json_types)] <- NA

  if (should_wrap_scalars(wrap_scalars, ptype, json_types)) {
    # wrap all simple types in an array and change the json_type
    idx <- !json_types %in% c("null", cplx_json_types) & !is.na(json_types)
    x_parsed[idx] <- lapply(x_parsed[idx], json_array_agg)
    json_types[idx] <- "array"
  }

  json_vec_c(x_parsed, json_types, ptype = ptype)
}

should_wrap_scalars <- function(wrap_scalars, ptype, json_types) {
  wrap_scalars &&
    # only necessary if we encounter arrays or objects
    any(json_types %in% c("array", "object"), na.rm = TRUE) &&
    # ptype = NULL -> more user friendly to wrap scalars
    # ptype = json2 -> necessary
    (is_null(ptype) || inherits(ptype, "json2"))
}

#' @noRd
#' @title Convert characters according to json types
#'
#' Convert the values returned by `json_each` according to the types.
#'
#' @param x Character values returned from `json_each`.
#' @param json_types Character vector of json types.
#'
#' @return A list of parsed values.
#'
#' @examples
#' convert_json_type(c("1", "[1]"), c("integer", "array"))
convert_json_type <- function(x, json_types, bigint_as_char = TRUE) {
  stopifnot(length(x) == length(json_types))
  stopifnot(all(json_types %in% all_json_types | is.na(json_types)))
  stopifnot(is_bool(bigint_as_char))

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

  null_idx <- json_types == c("null") & not_na
  x_parsed[null_idx] <- list(NULL)

  lgl_idx <- json_types %in% c("true", "false") & not_na
  x_parsed[lgl_idx] <- x[lgl_idx] == "1"

  json_idx <- json_types %in% c("object", "array") & not_na
  if (any(json_idx)) {
    x_parsed[json_idx] <- vec_chop(new_json2(x[json_idx]))
  }
  # object_idx <- json_types == "object" & not_na
  # if (any(object_idx)) {
  #   x_parsed[object_idx] <- vec_chop(new_json_object(x[object_idx]))
  # }
  # array_idx <- json_types == "array" & not_na
  # if (any(array_idx)) {
  #   x_parsed[array_idx] <- vec_chop(new_json_array(x[array_idx]))
  # }

  x_parsed
}

json_ptype_common <- function(types, ptype = NULL) {
  type_map <- list(
    object = new_json_object(),
    array = new_json_array(),
    integer = integer(),
    real = numeric(),
    logical = logical(),
    null = NULL,
    text = character()
  )

  types <- unique(types)
  # types <- types[!is.na(types)]
  types <- types[!is.na(types) & types != "null"]
  r_types <- types
  r_types[r_types %in% c("true", "false")] <- "logical"

  if (is_null(ptype)) {
    if (any(types %in% cplx_json_types)) {
      # array + object => json
      # array/object + anything else => error
      if (any(scalar_json_types %in% types)) {
        # TODO inform about `wrap_scalars` or `ptype = character()`
        stop_jsontools("Cannot combine JSON array/object with scalar values.")
      }

      return(new_json2())
    } else {
      # no json => normal coercion rules
      return(vec_ptype_common(!!!type_map[r_types]))
    }
  }

  # if ptype is specified as array/object/json => only these are valid
  if (inherits(ptype, "json2_array")) {
    if (any(types != "array")) {
      stop_jsontools("Not all elements are arrays.")
    }
    return(new_json2())
  } else if (inherits(ptype, "json2_object")) {
    if (any(types != "object")) {
      stop_jsontools("Not all elements are objects.")
    }
    return(new_json2())
  } else if (inherits(ptype, "json2")) {
    if (!all(types %in% c(cplx_json_types, "null"))) {
      stop_jsontools("Not all elements are objects.")
    }
    return(new_json2())
  }

  ptype
}

json_vec_c <- function(x, types, ptype = NULL) {
  # call `json_ptype_common()` for its side effect of errors
  json_ptype_common(types, ptype)

  if (inherits(ptype, "json2_object") || inherits(ptype, "json2_array")) {
    ptype <- new_json2()
  }

  vec_c(!!!x, .ptype = ptype)
}

#' Prototype helpers
#'
#' Create a JSON object/array prototype to pass to the `.ptype` argument in
#' some of the functions.
#'
#' @export
#' @rdname new_json_object
new_json_object <- function() {
  new_vctr(character(), class = c("json2_object", "json2"))
}

#' @export
#' @rdname new_json_object
new_json_array <- function() {
  new_vctr(character(), class = c("json2_array", "json2"))
}

scalar_json_types <- c("true", "false", "integer", "real", "text")
cplx_json_types <- c("object", "array")
all_json_types <- c(scalar_json_types, cplx_json_types, "null")
