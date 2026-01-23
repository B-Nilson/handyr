#' Get a zip file and unzip it
#'
#' @param zip_url URL of zip file to download
#' @param local_path Where to save the zip file
#' @param unzip_dir Directory to unzip the contents to
#' @param quiet suppress messages?
#' @param cache Skip downloading the zip if it already exists?
#' @param ... Additional arguments passed to `utils::download.file()`
#' @return The path(s) to the unzipped file(s)
#' @export
get_and_unzip <- function(
  zip_url,
  local_path = file.path(".", basename(zip_url)),
  unzip_dir = dirname(local_path),
  quiet = FALSE,
  cache = TRUE,
  ...
) {
  need_zip <- !cache || (cache & !file.exists(local_path))
  if (need_zip) {
    zip_url |>
      utils::download.file(destfile = local_path, mode = "wb", quiet = quiet)
  }
  local_path |> utils::unzip(exdir = unzip_dir, ...)
}