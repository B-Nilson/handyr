# Check if URLs exist and can be reached

Performs a HEAD request on each URL and returns `TRUE` if the status
code is 200, `FALSE` otherwise.

## Usage

``` r
check_urls_exist(urls, quiet = FALSE, ...)
```

## Arguments

- urls:

  A character vector of URLs to check.

- quiet:

  A logical value indicating if warnings should be suppressed when not
  able to resolve host names for a URL. Default is `FALSE`.

- ...:

  Additional arguments passed to
  [`httr::HEAD()`](https://httr.r-lib.org/reference/HEAD.html).

## Value

A logical vector indicating if the corresponding URL exists.
