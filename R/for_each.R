for_each = function(input, FUN, ..., .bind = FALSE, .name = FALSE, .parallel = FALSE, .workers = parallel::detectCores(), .invisible = FALSE) {
  if (.parallel) {
    future::plan(future::multisession, workers = .workers) # Run in parallel if desired
    lapply_fun <- future.apply::future_lapply
  } else {
    lapply_fun <- lapply
  }

  # Run input through function
  if (.invisible) {
    out <- suppressWarnings(suppressMessages(invisible(input |>
      lapply_fun(FUN, ...)
    )))
  } else {
    out <- input |> lapply_fun(FUN, ...)
  }

  if (.parallel) future::plan(future::sequential) # Stop running in parallel
  
  if (.name) names(out) <- input # Use input as names if desired

  if (.bind) out <- out |> dplyr::bind_rows() # Bind rowise if desired
  
  if (.invisible) {
    invisible(out)
  } else {
    return(out)
  }
}