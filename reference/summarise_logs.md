# Log overall and individual run time for repeated [`log_step()`](https://b-nilson.github.io/handyr/reference/log_step.md) calls

`summarise_logs` takes a list of log entries from
[`log_step()`](https://b-nilson.github.io/handyr/reference/log_step.md)
and outpus a message with a summary of the time taken between each log
call.

## Usage

``` r
summarise_logs(logs, save_to = NULL)
```

## Arguments

- logs:

  A list of
  [`log_step()`](https://b-nilson.github.io/handyr/reference/log_step.md)
  return values

- save_to:

  A character value indicating the path to a text file to save the
  summary of the logs to. Default is `NULL` (do not save a log file).

## Value

a character vector indicating the summary of the time taken between each
log call

## See also

Other Utilities:
[`swap()`](https://b-nilson.github.io/handyr/reference/swap.md)

## Examples

``` r
logs <- log_step("Example usage of log_summarise", header = TRUE)
#> |--------------------   Example usage of log_summarise   --------------------|
logs$step_one <- log_step("Step-1...")
#> 2025-11-17 17:30:30: Step-1...
# Do something
logs$step_two <- log_step("Step-2...")
#> 2025-11-17 17:30:30: Step-2...
# Do something else
logs$step_three <- log_step("Step-3...")
#> 2025-11-17 17:30:30: Step-3...
# Do something else
logs$done <- log_step("Complete")
#> 2025-11-17 17:30:30: Complete
# Summarise run times and write to file
summarise_logs(logs, save_to = tempfile())
#> 
#> Total time: 0ms 
#> --> Step-1...: 0ms
#> --> Step-2...: 0ms
#> --> Step-3...: 0ms
```
