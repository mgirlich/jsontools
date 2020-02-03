# jq Cheatsheet:
# https://gist.github.com/olih/f7437fb6962fb3ee9fe95bda8d2c8fa4
# https://www.cheatography.com/orabig/cheat-sheets/jq/

jq_paste <- function(...) {
  paste0(c(...), collapse = "\n\n") %>%
    noquote()
}

get_glimpse_data <- function(x) {
  jq_homo_function <- "def is_homo_array: map(type) | unique | length | . == 1;"
  jq_summarise_function <- 'def summarise: (if type == "number" then
		{"value": .}
	elif type == "string" then
		{"value": .[0:100]}
	elif type == "array" then
		if is_homo_array then
			{"homo": true, "homo_type": (.[0] | type)}
		else
			{"homo": false, "types": map(type)}
		end
	elif type == "object" then
		{"keys": keys_unsorted}
	else
  {"value": .}
	end) as $summary |
		{"type": type, "length": length} + $summary;'

  jq_main <- '{"type": type, "length": length, "summary": map_values(summarise)}'
  jq_cmd <- jq_paste(jq_homo_function, jq_summarise_function, jq_main)

  jq_do2(x, jq_cmd) %>%
    parse_json()
}


cat_line <- function(...) {
  cat(paste0(..., "\n"), sep = "")
}


format_elt_types <- function(y) {
  if (y$type == "object") {
    type_sum <- y$type
  } else if (y$type == "array") {
    if (y$homo) {
      type_sum <- glue("array<{y$homo_type}>")
    } else {
      type_sum <- "array"
    }

  } else {
    type_sum <- y$type
  }

  info <- glue("<{type_sum}>[{y$length}]")
  pillar::style_subtle(info)
}


format_elt_names <- function(nms) {
  # TODO escape names if necessary?
  # TODO truncate to max width?
  nms
}


format_values <- function(y) {
    # TODO need to inform if value was cut off
  if (y$type == "object") {
    keys <- paste0(y$keys, collapse = ", ")
    object_sum <- paste0("{", keys, "}")
  } else if (y$type == "array") {
    if (y$homo) {
      object_sum <- ""
    } else {
      # TODO need to restrict number of types in here?
      object_sum <- paste0("[", paste0(y$types, collapse = ", "), "]")
    }

  } else {
    object_sum <- y$value
  }

  object_sum
}


nchar_width <- function (x) {
  nchar(x, type = "width")
}

# tibble:::str_trunc
str_trunc <- function(x, max_width) {
  width <- nchar(x)

  nchar_ellipsis <- nchar_width(cli::symbol$ellipsis)

  for (i in seq_along(x)) {
    if (width[i] <= max_width[i]) next

    x[i] <- paste0(substr(x[i], 1, max_width[i] - nchar_ellipsis), cli::symbol$ellipsis)
  }

  x
}

justify <- tibble:::justify
big_mark <- pillar:::big_mark


json_glimpse2 <- function(x) {
  # TODO make this work for arrays
  # type: null, boolean, number, string, array or object.

  jq_f_summarise_array_of_objects <- 'def summarise_array_of_objects:
	  def get_types(f):
  		.[] | .[f] | type;

  	def get_keys:
  		map(keys_unsorted) | add | unique;

  	. as $input |
  	get_keys |
  	map(
  		. as $id |
  		$input |
  		[get_types($id)] |
  		{($id): unique}
  	) |
  	add;'

  jq_summarise <- 'def summarise:
  	(if type == "array" then
  		map(select(. != null) | type) as $elt_types |
  		if ($elt_types | all(. == "object")) then
  			{"array_of_objects": true, "elts": summarise_array_of_objects}
  		else
  			{"array_of_objects": false, "elt_types": $elt_types}
  		end
  	elif type == "object" then
  		{"elts": map_values(summarise)}
  	elif type == "string" then
  		{"value": .[0:100]}
  	else
  		{"value": .}
  	end) as $r |

  	(if type == "boolean" then
  		{"type": type}
  	else
  		{"type": type, "length": length}
  	end) + $r;'

  # jq_main <- 'if is_homo_array then
  # 	if (map(type)[0] == "object") then
  # 		summarise_array_of_objects
  # 	else
  # 		"array<" + (.[0] | type) + ">[" + (length | tostring) + "]"
  # 	end
  # else
  # 	"array<mixed>[" + (length | tostring) + "]"
  # end'
  jq_main <- "summarise"

  jq_cmd <- jq_paste(jq_f_summarise_array_of_objects, jq_summarise, jq_main)
  parsed <- jq_do(x, jq_cmd) %>%
    parse_json()

    unclass() %>%
    prettify() %>%
    cat()
}


format_scalar <- function(name, type, value, depth) {
  cat(rep(".", depth * 2), "$", name, " <", type, ">   ", value, "\n", sep = "")
}


format_object <- function(object, depth = 0) {
  type_map <- c(
    null = "null",
    boolean = "lgl",
    number = "dbl",
    string = "chr",
    array = "lst",
    object = "obj"
  )


  purrr::iwalk(
    object$elts,
    function(elt, name) {
      if (elt$type %in% c("string", "number", "boolean", "null")) {
        format_scalar(name, elt$type, elt$value, depth)
      } else if (elt$type == "object") {
        cat(rep(".", depth * 2), "$", name, " <", elt$type, ">", "\n", sep = "")
        format_object(elt, depth = depth + 1)
      } else if (elt$type == "array") {
        if (is_true(elt$array_of_objects)) {
          cat(rep(".", depth * 2), "$", name, " [object]", "\n", sep = "")
          purrr::iwalk(
            elt$elts,
            ~ {
              cat(rep(".", (depth + 1) * 2), "$", .y, " <", .x, ">", "\n", sep = "")
            }
          )
        } else {
          types <- vec_uniqe(elt$elt_types)
          if (length(types) == 1) {
            cat(rep(".", depth * 2), "$", name, " [", types, "]", "\n", sep = "")
          } else {
            cat(rep(".", depth * 2), "$", name, " [", paste0(types, collapse = ", "), "]", "\n", sep = "")
          }
        }
      } else {
        abort("something went wrong")
      }
    }
  )
}


print_it <- function() {
  if (parsed$type == "object") {
    cat("JSON object with", parsed$length, "keys")

    i <- 2
    elt <- parsed$elts[[i]]
    name <- names(parsed$elts)[[i]]

    format_object(parsed)

  }
}


# json_glimpse2 <- function(x) {
#   # type: null, boolean, number, string, array or object.
#
#   # TODO make this work for arrays
#   jq_f_summarise_array_of_objects <- 'def summarise_array_of_objects:
#   	def get_types(f):
#   		.[] | .[f] | type;
#
#   	def get_keys:
#   		map(keys) | add | unique;
#
#   	. as $input |
#   	get_keys |
#   	map(
#   		. as $id |
#   		$input |
#   		[get_types($id)] |
#   		{($id): unique}
#   	) |
#   	add;'
#
#   # returns true if all elements of an array have the same type
#   jq_f_is_homo_array <- 'def is_homo_array: map(type) | unique | length | . == 1;'
#
#   jq_summarise2 <- 'def summarise:
#     (if type == "array" then
#   		if is_homo_array then
#   			if (map(type)[0] == "object") then
#   				summarise_array_of_objects
#   			else
#   				"array<" + (.[0] | type) + ">[" + (length | tostring) + "]"
#   			end
#   		else
#   			"array<mixed>[" + (length | tostring) + "]"
#   		end
#   	elif type == "object" then
#   		map_values(summarise)
#   	else
#   		type
#   	end) as $summary |
#   		$summary;'
#
#   # jq_main <- 'map_values(summarise)'
#   jq_main <- 'if is_homo_array then
#   	if (map(type)[0] == "object") then
#   		summarise_array_of_objects
#   	else
#   		"array<" + (.[0] | type) + ">[" + (length | tostring) + "]"
#   	end
#   else
#   	"array<mixed>[" + (length | tostring) + "]"
#   end'
#
#   jq_cmd <- jq_paste(jq_f_summarise_array_of_objects, jq_f_is_homo_array, jq_summarise2, jq_main)
#   jq_do(x, jq_cmd) %>%
#     unclass() %>%
#     prettify() %>%
#     cat()
# }


json_glimpse <- function(x) {
  # TODO handle json of length > 1
  # --> maybe by applying a slurp operation? or show a shorter summary?

  # 1. object: <#keys>
  structure <- get_glimpse_data(x)

  # get print width?
  # width <- tibble_glimpse_width(width)
  width <- getOption("width")
  # cat_line("Observations: ", big_mark(nrow(x)))
  cat_line("# A JSON ", structure$type, " with ", big_mark(structure$length), " elements")

  # var_types <- map_chr(map(df, new_pillar_type), format)
  elt_types <- purrr::map_chr(structure$summary, format_elt_types)

  # ticked_names <- format(new_pillar_title(tick_if_needed(names(df))))
  elt_names <- format_elt_names(names(structure$summary))
  elt_names <- paste0("$ ", justify(elt_names, right = FALSE), " ", elt_types, " ")

  data_width <- width - crayon::col_nchar(elt_names) - 2
  # formatted <- map_chr(df, function(x) collapse(format_v(x)))
  values_formatted <- purrr::map_chr(structure$summary, format_values)

  truncated <- str_trunc(values_formatted, data_width)
  cat_line(elt_names, truncated)

  invisible(x)


  # if (structure$type == "object") {
  #   s <- structure$summary
  #   summary_list <- lapply(s, format_summary)
  #   body <- glue("$ {justify(names(summary_list), right = FALSE)}: {summary_list}")
  # }
}
