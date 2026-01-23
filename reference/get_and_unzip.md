# Get a zip file and unzip it

Get a zip file and unzip it

## Usage

``` r
get_and_unzip(
  zip_url,
  local_path = file.path(".", basename(zip_url)),
  unzip_dir = dirname(local_path),
  quiet = FALSE,
  cache = TRUE,
  ...
)
```

## Arguments

- zip_url:

  URL of zip file to download

- local_path:

  Where to save the zip file

- unzip_dir:

  Directory to unzip the contents to

- quiet:

  suppress messages?

- cache:

  Skip downloading the zip if it already exists?

- ...:

  Additional arguments passed to
  [`utils::download.file()`](https://rdrr.io/r/utils/download.file.html)

## Value

The path(s) to the unzipped file(s)
