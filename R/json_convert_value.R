json_convert_value <- function(x,
                               json_types,
                               ptype,
                               wrap_scalars = FALSE,
                               bigint_as_char = FALSE) {
  x_parsed <- convert_json_type(x, json_types, bigint_as_char = bigint_as_char)

  if (is_null(ptype) &&
      bigint_as_char &&
      is_true(attr(x_parsed, "bigint"))) {
    message("big integers found and converted to character.")
  }

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

  json_vec_c(x_parsed, json_types, ptype = ptype, bigint_as_char = bigint_as_char)
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
#' @title Convert characters according to JSON types
#'
#' Convert the values returned by `json_each` according to the types.
#'
#' @param x Character values returned from `json_each`.
#' @param json_types Character vector of JSON types.
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
      x_parsed[int_idx] <- vec_chop(as_integer64(x[int_idx]))
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
        msg <- c(
          x = "Cannot combine JSON array/object with scalar values.",
          i = "Use `wrap_scalars = TRUE` to wrap scalars in an array.",
          i = "Use `ptype = character()` to return result as text."
        )
        stop_jsontools(msg)
      }

      return(new_json2())
    } else {
      # no JSON => normal coercion rules
      return(vec_ptype_common(!!!type_map[r_types]))
    }
  }

  # if ptype is specified as array/object/JSON => only these are valid
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
      stop_jsontools("Not all elements are objects or arrays.")
    }
    return(new_json2())
  }

  ptype
}

json_vec_c <- function(x, types, ptype = NULL, bigint_as_char = FALSE) {
  # call `json_ptype_common()` for its side effect of errors
  json_ptype_common(types, ptype)

  bigint_found <- is_true(attr(x, "bigint"))

  # check that `ptype` is compatible before converting the big ints to character
  if (bigint_found) {
    vec_ptype2(ptype, integer())
  }

  if (bigint_found &&
      bigint_as_char &&
      (is_null(ptype) || is_integer64(ptype))) {
    # manually convert types that can be cast to character
    x[types == "true"] <- "1"
    x[types == "false"] <- "0"
    numeric_types_idx <- types %in% c("integer", "real", "true", "false")
    x[numeric_types_idx] <- as.character(x[numeric_types_idx])

    ptype <- character()
  }

  if (inherits(ptype, "json2_object") || inherits(ptype, "json2_array")) {
    ptype <- new_json2()
  }

  vec_c(!!!x, .ptype = ptype)
}

as_integer64 <- function(x) {
  rlang::check_installed("bit64")
  bit64::as.integer64(x)
}

is_integer64 <- function(x) {
  inherits(x, "integer64")
}

#' Prototype helpers
#'
#' Create a JSON object/array prototype to pass to the `.ptype` argument in
#' some of the functions.
#'
#' @return An empty `json2_object` resp. `json2_array`.
#' @export
#'
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
