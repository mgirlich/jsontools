% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify.R
\name{json_merge}
\alias{json_merge}
\title{Merge two JSON objects}
\usage{
json_merge(x, y)
}
\arguments{
\item{x}{A JSON vector to update.}

\item{y}{A JSON vector with updated values.}
}
\value{
A \code{json2} vector.
}
\description{
By merging two objects you can add, modify, or remove elements of an object.
Arrays cannot be modified but only replaced as a whole. It is mostly a small
wrapper around the SQLite function
\href{https://www.sqlite.org/json1.html#jpatch}{\code{json_patch()}}.
}
\examples{
# update element with key "a"
json_merge('{"a": 1, "c": 3}', '{"a": 11}')

# you can also add elements
json_merge('{"a": 1, "c": 3}', '{"b": 2}')

# remove elements with `null`
json_merge('{"a": 1, "c": 3}', '{"c": null}')
}
