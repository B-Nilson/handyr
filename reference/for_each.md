# Loop over a vector-like object and apply a function

`for_each` provides a simple way to loop over a vector and apply a
function with useful postprocessing.

## Usage

``` r
for_each(
  x,
  FUN,
  ...,
  .enumerate = FALSE,
  .bind = FALSE,
  .bind_id = NULL,
  .join = FALSE,
  .join_by = NULL,
  .join_mode = "full",
  .name = FALSE,
  .as_list = NULL,
  .parallel = FALSE,
  .workers = NULL,
  .plan = "multisession",
  .parallel_cleanup = TRUE,
  .show_progress = !.quiet,
  .quiet = FALSE
)
```

## Arguments

- x:

  Something iterable (a vector, list, etc).

- FUN:

  A function to be applied to each entry in `x`.

- ...:

  Additional arguments to be passed to `FUN` or to
  [`future.apply::future_lapply()`](https://future.apply.futureverse.org/reference/future_lapply.html)
  if `.parallel = TRUE`.

- .enumerate:

  A logical value indicating if `i` should be passed to `FUN` alongside
  `x`. Default is `FALSE`. If `TRUE`, `FUN` is run as
  `FUN(x[[i]], i, ...)`, where `i` is the index of values in `x`.

- .bind:

  A logical value indicating whether to apply
  [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html).
  Default is `FALSE`

- .bind_id:

  A single character string indicating the column name to use for the
  row index if `.bind = TRUE`. Default is `NULL` (don't add a row index
  column).

- .join:

  A logical value indicating if the output should be joined using
  [`join_list()`](https://b-nilson.github.io/handyr/reference/join_list.md).
  Default is `FALSE`.

- .join_by:

  One or more character values indicating the column name(s) to join by
  if `.join = TRUE`. Default is `NULL`, which joins on all matching
  columns.

- .join_mode:

  A character string specifying the type of join to perform if
  `.join = TRUE`. Must be one of "full", "inner", "left", "right", ...
  (see
  [`join_list()`](https://b-nilson.github.io/handyr/reference/join_list.md)
  for all options). Default is "full", which uses
  [`dplyr::full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html).

- .name:

  A logical value indicating if the output should be named after `x`.
  (i.e `names(out) <- x`) Default is `FALSE`.

- .as_list:

  A logical value indicating if the output should be a list (see
  [`lapply()`](https://rdrr.io/r/base/lapply.html) /
  [`future.apply::future_lapply()`](https://future.apply.futureverse.org/reference/future_lapply.html))
  or a vector (see[`sapply()`](https://rdrr.io/r/base/lapply.html) /
  [`future.apply::future_sapply()`](https://future.apply.futureverse.org/reference/future_lapply.html)).
  Default is `NULL`, which is `FALSE` if `x` is a vector, `TRUE`
  otherwise.

- .parallel:

  A logical value indicating if the function should be run in parallel
  (see
  [`future::multisession()`](https://future.futureverse.org/reference/multisession.html)).
  Default is `FALSE`.

- .workers:

  A single numeric value indicating the number of workers to run in
  parallel if `.parallel = TRUE`. Default is `NULL` which uses all
  available cores (see
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html)).
  If an an existing plan is set of the same type as `.plan`, this
  argument is ignored.

- .plan:

  A string indicating the strategy to use if `.parallel = TRUE`. Default
  is `"multisession"` (see
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)).
  If `.parallel = FALSE`, this argument is ignored. If an an existing
  plan is set of the same type, this argument is ignored.

- .parallel_cleanup:

  A logical value indicating if the parallel plan should be reset to
  sequential using `future::plan("sequential")` if `.parallel = TRUE`.
  Default is `TRUE`.

- .show_progress:

  A logical value indicating if the progress bar (see
  [`pbapply::pbsapply()`](https://peter.solymos.org/pbapply/reference/pbapply.html))
  should be shown if `.parallel = TRUE`. Default is `!.quiet`.

- .quiet:

  A logical value indicating if the output should be invisible (no
  messages/warnings). Default is `FALSE`.

## Value

a list of the output of `FUN` iterated over `x` which: if
`.bind = TRUE`: is bound rowwise into a data frame using
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
if `.name = TRUE`: has names set to `x` using `names(out) <- x` if
`.quiet = TRUE`: is invisible

## Examples

``` r
1:3 |> for_each(\(value) value + 1)
#> [1] 2 3 4
c(7, 7, 7) |> for_each(\(value, i) value + i, .enumerate = TRUE)
#> [1]  8  9 10

list(
  data.frame(x = 1:3),
  data.frame(x = 4:6)
) |>
  for_each(
    \(dat) dat |> dplyr::mutate(y = x + 1),
    .bind = TRUE
  )
#>   x y
#> 1 1 2
#> 2 2 3
#> 3 3 4
#> 4 4 5
#> 5 5 6
#> 6 6 7

c("bread", "jam") |>
  for_each(
    \(value) paste("eat", value),
    .name = TRUE
  )
#>       bread         jam 
#> "eat bread"   "eat jam" 

values <- 1:3 |>
  for_each(\(value) value + 1, .parallel = TRUE, .workers = 2, .as_list = TRUE)

values <- 1:3 |>
  for_each(\(value) message(value + 1), .quiet = TRUE)
```
