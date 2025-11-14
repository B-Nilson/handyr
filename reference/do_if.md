# Apply a function if a condition is met

Apply a function if a condition is met

## Usage

``` r
do_if(x, .do, .if = TRUE, ..., .return = NA)
```

## Arguments

- x:

  Something that `.do` can be applied to.

- .do:

  A function to be applied to `x`.

- .if:

  A logical value indicating if `.do` should be applied or not.

- ...:

  Additional arguments to be passed to `.do`.

- .return:

  What to return instead of the output of `.do` if `.if == FALSE`.

## Value

The output of `.do` applied to `x` if `.if == TRUE`, `.return` otherwise

## Examples

``` r
c(1, 2, 3) |> do_if(.do = mean, .if = TRUE)
#> [1] 2
c(1, 2, 3) |> do_if(.do = mean, .if = FALSE)
#> [1] NA
c(1, 2, 3) |> do_if(.do = mean, .if = FALSE, .return = -1)
#> [1] -1
```
