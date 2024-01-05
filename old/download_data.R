library(here)
library(data.table)
source(here("R", "get-hub-forecasts.R"))

DT <- `[`
repo <- "covid19-forecast-hub-europe/covid19-forecast-hub-europe"

forecasts_unprocessed <- get_hub_forecasts(repo)
data.table::fwrite(forecasts_unprocessed, here("data", "forecasts_unprocessed.csv"))



anomalies_unprocessed <- download_metadata(repo, branch = "main")

data.table::fwrite(anomalies_unprocessed, here("data", "anomalies_unprocessed.csv"))
s
