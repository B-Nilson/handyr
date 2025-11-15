#' Check if URLs exist and can be reached
#' 
#' Performs a HEAD request on each URL and returns `TRUE` if the status code is 200, `FALSE` otherwise.
#'
#' @param urls A character vector of URLs to check.
#' @param quiet A logical value indicating if warnings should be suppressed when not able to resolve host names for a URL. Default is `FALSE`.
#' @param ... Additional arguments passed to [httr::HEAD()].
#'
#' @return A logical vector indicating if the corresponding URL exists.
#' @export
check_urls_exist <- function(urls, quiet = FALSE, ...) {
  stopifnot("At least one URL must be provided." = length(urls) >= 1)
  stopifnot("`urls` must be a character vector." = identical(class(urls), "character"))
  stopifnot("`urls` must not contain missing values." = !any(is.na(urls)))
  stopifnot("`quiet` must be a single logical value." = is.logical(quiet) & length(quiet) == 1)
  
  # Make HEAD requests for each url and return status codes (or -1 if failed to make request)
  status_codes <- urls |> 
    sapply(
      \(url) {
        httr::HEAD(url, ...) |>
          httr::status_code() |> 
          on_error(.return = -1, .warn = !quiet)
      }
    )
  
  # Return TRUE where status code is 200
  status_codes == 200
}