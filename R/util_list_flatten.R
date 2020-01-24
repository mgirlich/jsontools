# equivalent of jq '[paths]'
# x <- list(
#   abc = list(
#     def = 2,
#     xyz = 10
#   ),
#   new_key = "new value"
# )
# flatten_names(x)
# flatten_names <- function(some_list, pre_name = NULL) {
#   r <- list()
#   for (i in seq_along(some_list)) {
#     nm <- names(some_list)[[i]]
#     item <- some_list[[i]]
#     if (is.list(item)) {
#       tmp <- flatten_names(item, pre_name = c(pre_name, nm))
#     } else {
#       tmp <- list(c(pre_name, nm))
#     }
#     r <- append(r, tmp)
#   }
#
#   r
# }




# create_path(x["abc"])
# create_path(x[2])
# x <- list(
#   abc = 2,
#   def = "new value"
# )
# create_path(x)
# create_path <- function(x, path = NULL) {
#   if (is.list(x)) {
#     r <- purrr::imap(x, create_path)
#     purrr::map(r, ~ {
#       .x$path <- c(path, .x$path)
#       .x
#     })
#   } else {
#     list(path = path, value = x)
#   }
# }
#
#
# flatten <- function(some_list) {
#   values <- flatten_values(some_list)
#   names <- flatten_names(some_list)
#   purrr::map2(values, names, ~ list(key = .y, value = .x))
#   # list(values = values, names = names)
#
#   # values <- list()
#   # names <- list()
#   #
#   # for (i in seq_along(some_list)) {
#   #   nm <- names(some_list)[[i]]
#   #   item <- some_list[[i]]
#   #   if (is.list(item)) {
#   #     tmp <- flatten_values(item)
#   #
#   #     tmp <- flatten_names(item)
#   #     tmp <- purrr::map(tmp, ~ c(nm, .x))
#   #   } else {
#   #     tmp <- item
#   #   }
#   #   r <- append(r, tmp)
#   # }
#   #
#   # for (i in seq_along(some_list)) {
#   #   if (is.list(item)) {
#   #     tmp <- flatten_names(item)
#   #     tmp <- purrr::map(tmp, ~ c(nm, .x))
#   #   } else {
#   #     tmp <- nm
#   #   }
#   #   r <- append(r, tmp)
#   # }
#   #
#   # r
# }
#
#
# flatten_values(x)
# flatten_values <- function(some_list) {
#   r <- list()
#   for (item in some_list) {
#     if (is.list(item)) {
#       tmp <- flatten_values(item)
#     } else {
#       tmp <- item
#     }
#     if (is.null(tmp)) {
#       tmp <- list(NULL)
#     }
#     r <- append(r, tmp)
#   }
#
#   r
# }
#
#
# flatten_names(x)
# xx <- repurrrsive::discog[[1]]
# nms <- flatten_names(xx) %>%
#   purrr::map_chr(~ paste0(.x, collapse = "."))
# blub <- purrr::set_names(flatten_values(xx), nms) %>%
#   purrr::compact()
