% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rl_functions.R
\name{df_arrange}
\alias{df_arrange}
\title{Reorder a DatFrame}
\usage{
df_arrange(data, time = "time", well = "well")
}
\arguments{
\item{data}{A dataframe with a time column and a well column.}

\item{time}{The name of the column with the time points.}

\item{well}{The name of the column with the well ID information.}
}
\value{
The input dataframe reordered.
}
\description{
Reorders a dataframe by the time points, then by the rows then by the
columns.
}
