% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/on_error_on_warning.R
\name{on_error}
\alias{on_error}
\alias{on_warning}
\title{Control what happens when an error/warning occurs}
\usage{
on_error(..., .return = NULL, .message = FALSE, .warn = FALSE)

on_warning(..., .return = NULL, .message = FALSE, .stop = FALSE)
}
\arguments{
\item{...}{Something to run and capture errors/warnings (if any).}

\item{.return}{What is to be returned if an error/warning occurs.
Default is \code{NULL}}

\item{.message}{A logical value indicating if the error message should be displayed as a message instead.
Default is \code{FALSE}}

\item{.warn, .stop}{A logical value indicating if the error/warning message should be displayed as a warning/error instead.
Default is \code{FALSE}}
}
\value{
the output of \code{...} unless an error/warning occurs, then \code{invisible(.return)} instead.
}
\description{
\code{on_error}/\code{on_warning} provide a simple way to handle errors/warnings by specifying a value to be returned instead as well as if a message or warning/error should be displayed instead.
}
\examples{
on_error(stop("test"), .return = -1, .message = TRUE)
on_error(read.csv("not_A_fil3.123"), .return = NULL)
on_warning(warning("test"), .return = -1, .message = TRUE)
on_warning(base::max(NA, na.rm = TRUE), .return = NULL)
}
