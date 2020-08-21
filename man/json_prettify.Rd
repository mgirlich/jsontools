% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{json_prettify}
\alias{json_prettify}
\alias{json_minify}
\title{Prettify/Minify a JSON vector}
\usage{
json_prettify(x)

json_minify(x)
}
\arguments{
\item{x}{A JSON vector.}
}
\value{
A json2 vector
}
\description{
Prettify/Minify a JSON vector
}
\examples{
x <- c('[1,2,   3]', '{"a": 1, "b": 2}')
json_prettify(x)
json_minify(x)
}