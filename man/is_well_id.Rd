% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{is_well_id}
\alias{is_well_id}
\title{Logical test for well ID format.}
\usage{
is_well_id(x)
}
\arguments{
\item{x}{A string vector.}
}
\value{
A logical vector.
}
\description{
Logical test for well ID format.
}
\examples{
is_well_id(c("a12", "a2", "a02", "foo1"))
}
