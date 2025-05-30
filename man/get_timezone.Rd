% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_timezone.R
\name{get_timezone}
\alias{get_timezone}
\title{Lookup timezones of latitude/longitude pairs}
\usage{
get_timezone(lng, lat, method = "accurate", quiet = FALSE, ...)
}
\arguments{
\item{lng, lat}{Numeric longitude/latitude pairs (decimal degrees).}

\item{method}{either "accurate" or "fast",  indicating the lookup method to use.
See \link[lutz:tz_lookup_coords]{lutz::tz_lookup_coords} for more details.
Set \code{.quiet} to \code{TRUE} to suppress warning produced when \code{method = "fast"}.}

\item{quiet}{A logical value indicating if messages and warnings should be suppressed.}

\item{...}{Additional parameters to pass to \link[lutz:tz_lookup_coords]{lutz::tz_lookup_coords} (currently unused)}
}
\value{
a character vector with the same length as \code{lat} and \code{lng} indicating each locations likely timezone.
See \link[lutz:tz_lookup_coords]{lutz::tz_lookup_coords} for more details.
}
\description{
\code{get_timezone} uses the \code{lutz} package to lookup the timezone of locations from lat/lng coords. See \link[lutz:tz_lookup_coords]{lutz::tz_lookup_coords} for more details.
}
\examples{
get_timezone(lng = -105.053144, lat = 69.116178, method = "accurate")
get_timezone(lng = c(-105.053144, -106.053144), lat = c(69.116178, 49.116178), method = "fast")
}
