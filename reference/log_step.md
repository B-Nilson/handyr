# Log a message for script progress tracking

Log a message for script progress tracking

## Usage

``` r
log_step(
  ...,
  header = FALSE,
  time = !header,
  time_format = "%Y-%m-%d %H:%M:%S",
  tz = Sys.timezone(),
  quiet = FALSE,
  sep = " "
)
```

## Arguments

- ...:

  One or more character (or coercible to) values or vectors of messages.
  If more than one message is provided, they will be combined with
  `paste(collapse = sep)`

- header:

  A logical value indicating if the message should be formatted as a
  header ("\|— message —\|") Default is `FALSE`

- time:

  A logical value indicating if the current time should be prepended to
  the log message. Default is the opposite of `header`

- time_format:

  A character value indicating the format of the timestamp. See
  [`base::strptime()`](https://rdrr.io/r/base/strptime.html) for
  formating details. Default is "%Y-%m-%d %H:%M:%S" (YYYY-MM-DD
  HH:MM:SS).

- tz:

  A character value indicating the time zone to use for the timestamp.
  Default is
  [`base::Sys.timezone()`](https://rdrr.io/r/base/timezones.html).

- quiet:

  A logical value indicating if the message should not be printed using
  [`base::message()`](https://rdrr.io/r/base/message.html). Default is
  `FALSE`.

- sep:

  A character value indicating the separator to use between messages
  when combined if multiple messages are provided. Default is " ".

## Value

an invisible list with the timestamp (POSIXct), output message
(character), and original message (character). If `header == TRUE` the
return is wrapped with `list(.log_init = {...})` to aid in tracking for
[`summarise_logs()`](https://b-nilson.github.io/handyr/reference/summarise_logs.md)

## Examples

``` r
logs <- log_step("My Awesome Script", time = FALSE, header = TRUE)
#> |--------------------   My Awesome Script   --------------------|
logs$step_1 <- log_step("Step 1...")
#> 2025-11-14 23:54:43: Step 1...
# Do something
logs$step_two <- log_step("Step-", 2, "...", sep = "")
#> 2025-11-14 23:54:43: Step-2...
# Do something else
logs$done <- log_step("Complete")
#> 2025-11-14 23:54:43: Complete
# Summarise run times and save log to file
summarise_logs(logs, save_to = tempfile())
#> 
#> Total time: 0ms 
#> --> Step 1...: 0ms
#> --> Step-2...: 0ms
```
