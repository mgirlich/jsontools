# json_glimpse_object <- function(x) {
#   each_df <- json_each(x)
#   summary_body <- format_json_body(
#     indices = each_df$key,
#     types = each_df$type,
#     values = each_df$value
#   )
#   summary_header <- c(
#     "object",
#     paste0("Keys: ", nrow(each_df))
#   )
#
#   c(summary_header, summary_body) %>%
#     cat_json_summary()
# }
#
# format_value <- function(value, type, width = 70) {
#   if (type %in% c("array", "object", "mixed")) {
#     # value <- tibble:::format_v(value)
#     value <- paste0(value, collapse = ", ")
#     pillar:::str_trunc(value, width)
#
#   } else {
#     value <- tibble:::format_v(value)
#     value <- paste0(value, collapse = ", ")
#     pillar:::str_trunc(value, width)
#   }
# }
#
# format_json_summary <- function(keys, types, values) {
#   keys <- pillar::align(keys)
#   values <- purrr::map2_chr(values, types, format_value)
#
#   types <- paste0("<", purrr::map_chr(types, abbr_type), ">") %>%
#     pillar::style_subtle() %>%
#     crayon::italic()
#
#   paste0("$ ", keys, " ", types, " ", values)
# }
#
# abbr_type <- function(type) {
#   switch(type,
#     text = "chr",
#     integer = "int",
#     real = "dbl",
#     array = "list",
#     object = "obj",
#     true = "lgl",
#     false = "lgl",
#     null = "null",
#     mixed = "mixed"
#   )
# }
#
# cat_json_summary <- function(x) {
#   cat(paste0(x, collapse = "\n"))
# }
#
#
# json_glimpse_array <- function(x) {
#   array_each_df <- json_each(x)
#   ptype <- json_ptype_common(array_each_df$type)
#
#   if (vec_is(ptype, new_json_object())) {
#     each_df <- json_each(as.character(array_each_df$value))
#
#     # TODO mark if key doesn't occur in every object
#     # TODO print value summary
#     key_each_df <- vec_split(each_df, each_df$key)
#
#     summary_df <- key_each_df %>%
#       dplyr::mutate(
#         n = purrr::map_int(val, nrow),
#         type = purrr::map_chr(
#           val,
#           ~ {
#             types <- unique(.x$type)
#             if (length(types) == 1) {
#               summary <- types
#             } else {
#               summary <- "mixed"
#             }
#           }
#         ),
#         value = purrr::map(
#           val,
#           ~ unlist(.x[["value"]])
#         )
#       )
#
#     json_summary_lines <- format_json_body(
#       indices = summary_df$key,
#       types = summary_df$type,
#       values = summary_df$value
#     )
#
#     header <- c(
#       "array of objects",
#       paste0("Elements: ", nrow(array_each_df)),
#       paste0("Keys: ", nrow(summary_df))
#     )
#
#     c(
#       header,
#       json_summary_lines
#     ) %>%
#       cat_json_summary()
#   } else if (vec_is(ptype, new_json_array())) {
#     header <- c(
#       "array of arrays",
#       paste0("Elements: ", nrow(array_each_df))
#     )
#
#     json_summary_lines <- format_json_body(
#       indices = array_each_df$key,
#       types = array_each_df$type,
#       values = array_each_df$value
#     )
#
#     c(
#       header,
#       json_summary_lines
#     ) %>%
#       cat_json_summary()
#   } else {
#     header <- c(
#       paste0("<list<", vec_ptype_abbr(ptype), ">>"),
#       paste0("Elements: ", nrow(array_each_df))
#     )
#
#     body <- format_json_body(
#       indices = array_each_df$key,
#       types = array_each_df$type,
#       values = array_each_df$value
#     )
#
#     c(
#       header,
#       body
#     ) %>%
#       cat_json_summary()
#   }
# }
#
#
# summarise_array <- function(x) {
#   array_each_df <- json_each(x)
#   ptype <- json_ptype_common(array_each_df$type)
#
#   if (vec_is(ptype, new_json_object())) {
#     each_df <- json_each(as.character(array_each_df$value))
#     key_each_df <- vec_split(each_df, each_df$key)
#
#     list(
#       ptype = ptype,
#       keys = key_each_df$key
#     )
#
#
#     v <- key_each_df$val[[1]]
#     for (v in key_each_df$val) {
#       # common type
#       # * array --> array summary
#       # * object --> object summary
#       # * scalar -->
#       # * mixed
#
#       ptype <- json_ptype_common(v$type)
#
#       # all scalar
#       list(
#         type = ptype,
#         value = unlist(v$value)
#       )
#
#       # all objects
#       list(
#         type = "object",
#         # value = ??
#         keys =
#       )
#
#     }
#
#
#     summary_df <- key_each_df %>%
#       dplyr::mutate(
#         n = purrr::map_int(val, nrow),
#         type = purrr::map_chr(
#           val,
#           ~ {
#             types <- unique(.x$type)
#             if (length(types) == 1) {
#               summary <- types
#             } else {
#               summary <- "mixed"
#             }
#           }
#         ),
#         value = purrr::map(
#           val,
#           ~ unlist(.x[["value"]])
#         )
#       )
#
#     json_summary_lines <- format_json_body(
#       indices = summary_df$key,
#       types = summary_df$type,
#       values = summary_df$value
#     )
#
#     header <- c(
#       "array of objects",
#       paste0("Elements: ", nrow(array_each_df)),
#       paste0("Keys: ", nrow(summary_df))
#     )
#
#     c(
#       header,
#       json_summary_lines
#     ) %>%
#       cat_json_summary()
#   } else if (vec_is(ptype, new_json_array())) {
#     header <- c(
#       "array of arrays",
#       paste0("Elements: ", nrow(array_each_df))
#     )
#
#     json_summary_lines <- format_json_body(
#       indices = array_each_df$key,
#       types = array_each_df$type,
#       values = array_each_df$value
#     )
#
#     c(
#       header,
#       json_summary_lines
#     ) %>%
#       cat_json_summary()
#   } else {
#     header <- c(
#       paste0("<list<", vec_ptype_abbr(ptype), ">>"),
#       paste0("Elements: ", nrow(array_each_df))
#     )
#
#     body <- format_json_body(
#       indices = array_each_df$key,
#       types = array_each_df$type,
#       values = array_each_df$value
#     )
#
#     c(
#       header,
#       body
#     ) %>%
#       cat_json_summary()
#   }
# }
#
# format_json_body <- function(indices, types, values) {
#   # path
#   # * object -> .key
#   # * array -> [index]
#   path <- format_path(indices)
#
#   # type summary
#   # * array_of -> no type needed; just print " : "
#   # * mixed array, object -> type
#
#   # TODO no type for array_of?
#   type_summary <- paste0("<", purrr::map_chr(types, abbr_type), ">") %>%
#     pillar::style_subtle() %>%
#     crayon::italic()
#
#   # always there indicator?
#
#   line_start <- paste0(
#     pillar::align(path), " ",
#     type_summary, " "
#   )
#
#   # value summary
#   # * scalar: truncated value
#   # * object: ?
#   # * array: ?
#
#   # TODO only print the first couple of values?
#   value_summary <- purrr::pmap_chr(
#     list(value = values, type = types, width = 80 - nchar(line_start)),
#     format_value
#   )
#
#   paste0(
#     pillar::align(path), " ",
#     type_summary, " ",
#     value_summary
#   )
# }
#
# format_path <- function(x) {
#   UseMethod("format_path")
# }
#
# format_path.character <- function(x) {
#   paste0("$.", x)
# }
#
# format_path.integer <- function(x) {
#   paste0("$.[", x, "]")
# }
#
# pad <- function(x, n) {
#   whitespaces <- paste0(rep(" ", n), collapse = "")
#   # x <- gsub("\n", paste0("\n", whitespaces), x)
#   paste0(whitespaces, x)
# }
