download_metadata <- function(repo, path = "data-truth/anomalies/anomalies.csv", branch) {
  sfread <- data.table::fread
  if (!is.null(getOption("useMemoise"))) {
    if (getOption("useMemoise")) {
      ch <- memoise::cache_filesystem(getOption("cache_path"))
      sfread <- memoise::memoise(sfread, cache = ch)
    }
  }
  sfread <- purrr::safely(sfread)
  url <- glue::glue("https://raw.githubusercontent.com/{repo}/{branch}/{path}")
  anomalies <- suppressMessages(sfread(url))
  if (!is.null(anomalies$error)) {
    warning("Forecast with the following path could not be downloaded: ", path)
    print(anomalies$error)
  }
  return(anomalies$result[])
}

