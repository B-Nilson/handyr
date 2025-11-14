# Control what happens when an error/warning occurs

`on_error`/`on_warning` provide a simple way to handle errors/warnings
by specifying a value to be returned instead as well as if a message or
warning/error should be displayed instead.

## Usage

``` r
on_error(..., .return = NULL, .message = FALSE, .warn = FALSE)

on_warning(..., .return = NULL, .message = FALSE, .stop = FALSE)
```

## Arguments

- ...:

  Something to run and capture errors/warnings (if any).

- .return:

  What is to be returned if an error/warning occurs. Default is `NULL`

- .message:

  A logical value indicating if the error message should be displayed as
  a message instead. If a single character string is provided, it will
  be used as the message instead. Default is `FALSE`

- .warn, .stop:

  A logical value indicating if the error/warning message should be
  displayed as a warning/error instead. If a single character string is
  provided, it will be used as the warning/error message instead.
  Default is `FALSE`

## Value

the output of `...` unless an error/warning occurs, then
`invisible(.return)` instead.

## Examples

``` r
on_error(stop("test"), .return = -1, .message = TRUE)
#> Error in doTryCatch(return(expr), name, parentenv, handler): test
on_error(read.csv("not_A_fil3.123"), .return = NULL)
#> Warning: cannot open file 'not_A_fil3.123': No such file or directory
on_warning(warning("test"), .return = -1, .message = TRUE)
#> simpleWarning in doTryCatch(return(expr), name, parentenv, handler): test
on_warning(base::max(NA, na.rm = TRUE), .return = NULL)
```
