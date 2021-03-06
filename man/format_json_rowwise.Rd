% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_json_rowwise.R
\name{format_json_rowwise}
\alias{format_json_rowwise}
\title{Convert each tbl row to JSON}
\usage{
format_json_rowwise(
  df,
  null = c("list", "null"),
  na = c("null", "string"),
  auto_unbox = TRUE,
  matrix = c("rowmajor", "columnmajor"),
  Date = c("ISO8601", "epoch"),
  POSIXt = c("string", "ISO8601", "epoch", "mongo"),
  factor = c("string", "integer"),
  complex = c("string", "list"),
  raw = c("base64", "hex", "mongo"),
  digits = 4,
  json_verbatim = TRUE,
  force = FALSE,
  rownames = FALSE,
  ...
)
}
\arguments{
\item{df}{A dataframe.}

\item{null, na, POSIXt, factor, complex, matrix, Date}{passed on to \code{\link[jsonlite:stream_in]{jsonlite::stream_out}}.}

\item{auto_unbox, raw, digits, force, ...}{passed on to \code{\link[jsonlite:stream_in]{jsonlite::stream_out}}.}

\item{json_verbatim}{Leave JSON as it is and do not encode it again?}

\item{rownames}{For data.frames add a field \verb{_row} with the row name?}
}
\value{
A \code{json2} vector of JSON objects.
}
\description{
Convert each row of a data frame to a JSON object (this basically produces
\href{http://ndjson.org/}{ndjson}).
}
\details{
Under the hood \code{\link[jsonlite:stream_in]{jsonlite::stream_out}} is used for the conversion.
}
\examples{
format_json_rowwise(mtcars[1:3, ])

# use a dataframe column for nested objects
df <- data.frame(x = 1:2)
df$y <- data.frame(z = c("a", "b"))
format_json_rowwise(df)

if (require("dplyr", quietly = TRUE, warn.conflicts = FALSE)) {
  tibble <- tibble::tibble
  # often useful in mutate/transmute
  mtcars \%>\%
    transmute(json = format_json_rowwise(tibble(mpg, cyl, extra = tibble(disp, hp))))
}
}
\seealso{
\code{\link[=write_json]{write_json()}}, \code{\link[=format_json]{format_json()}}
}
