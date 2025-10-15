#' Silence unwanted output
#'
#' This function wraps `tryCatch` and suppresses some or all of the following:
#' - errors
#' - warnings
#' - messages
#' - output from `print()`
#' - output from `cat()`
#' - output from `writeLines()`
#'
#' @param this_please Some code to run silently.
#'   If running multiple lines use `{....}`
#' @param errors A logical value indicating if errors should be silenced.
#'   Default is `TRUE`.
#' @param warnings A logical value indicating if warnings should be silenced.
#'   Default is `TRUE`.
#' @param messages A logical value indicating if messages should be silenced.
#'   Default is `TRUE`.
#' @param output A logical value indicating if output from `print()`, `cat()`, and `writeLines()` should be silenced.
#'   Default is `TRUE`.
#'
#' @return The output from `this_please`
#' @export
#'
#' @examples
#' silence(1 / 0)
#' silence(warning("test"))
#' silence(message("test"))
#' silence(print("test"))
#' silence(cat("test"))
#' silence(1 + "a")
silence <- function(
  this_please,
  errors = TRUE,
  warnings = TRUE,
  messages = TRUE,
  output = FALSE
) {
  # Functions for each level of silence
  silencers <- list(
    errors = suppressErrors,
    warnings = suppressWarnings,
    messages = suppressMessages,
    output = utils::capture.output
  )
  # Filter to desired silencers
  enabled_silencers <- silencers[
    which(c(errors, warnings, messages, output))
  ]
  # Run invisibly if no silencers
  if (length(enabled_silencers) == 0) {
    return(invisible(eval(this_please)))
  }
  # Build a string that wraps each silencer around `this_please`
  closing_parens <- ")" |>
    rep(length(enabled_silencers)) |>
    paste(collapse = "")
  silencer <- paste0(
    "enabled_silencers[[",
    seq_along(enabled_silencers),
    collapse = "",
    "]]("
  ) |>
    paste0("this_please", closing_parens)
  # Evaluate that string invisibly
  invisible(eval(parse(text = silencer)))
}

suppressErrors <- function(...) try(..., silent = TRUE)
