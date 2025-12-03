# Convert a sequence of numbers to a human-readable string

This function takes a vector of numbers and returns a human-readable
string representing the range of numbers. For example, the input c(1, 2,
3, 4, 5) would return the string "1 - 5". If the input is c(1, 2, 3, 5,
6), the output would be "1 - 3 and 5 - 6".

## Usage

``` r
sentence_range(x, reverse = FALSE)
```

## Arguments

- x:

  A vector of numbers to convert to a human-readable string.

- reverse:

  Should the reverse be done instead? i.e "1 - 3 and 5 - 6" -\> "1, 2,
  3, 5, 6".

## Value

A human-readable string representing the range of numbers in x.
