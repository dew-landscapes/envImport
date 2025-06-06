% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/choose_names.R
\name{choose_names}
\alias{choose_names}
\title{Choose columns to keep from a dataframe, based on a data map}
\usage{
choose_names(
  df = NULL,
  data_map = NULL,
  this_name,
  excludes = NULL,
  final_select = FALSE,
  final_select_col = "bio_all"
)
}
\arguments{
\item{df}{Dataframe to select columns from. Only needed if \code{is.null(data_map)}}

\item{data_map}{Dataframe or \code{NULL.} Mapping of fields to retrieve. See example
\code{envImport::data_map} or \code{envImport::data_map_old}. If \code{NULL} all columns are
returned. Optional if \code{df} provided.}

\item{this_name}{Character. \code{data_name} value in \code{data_map}. Required if
\code{data_map} is not \code{NULL}}

\item{final_select}{Logical. Is this the final select prior to writing to
disk?}

\item{final_select_col}{Character. If \code{final_select}, which column contains
the logical values to filter on?}
}
\description{
The behaviour when \code{df} provided and \code{is.null(data_map)} is essentially
\code{df <- df} (although any colnames matching \code{excludes} will be removed)
}
\keyword{internal}
