.onLoad <- function(...) {
  s3_register("pillar::pillar_shaft", "json2")
  s3_register("pillar::is_vector_s3", "json2")

  invisible()
}
