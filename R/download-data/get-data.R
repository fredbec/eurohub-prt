library(here)
library(data.table)
source(here("R", "download-data", "get-hub-forecasts.R"))

DT <- `[`
repo <- "covid19-forecast-hub-europe/covid19-forecast-hub-europe"

forecasts_unprocessed <- get_hub_forecasts(repo)
data.table::fwrite(forecasts_unprocessed, here("data", "forecasts_unprocessed.csv"))

forecasts_unprocessed <- fread(here("data", "forecasts_unprocessed.csv"))
#reorganize columns
forecasts <- data.table::copy(forecasts_unprocessed) |>
  DT(, c("horizon", "target_type") := tstrsplit(target, " wk ahead inc ", fixed=TRUE)) |>
  DT(, target := NULL) |>
  DT(, horizon := as.numeric(horizon)) |>
  DT(, target_type := fcase(target_type == "case", "Cases",
                            target_type == "death", "Deaths",
                            target_type == "hosp", "Hosp")) |>
  DT()


#filter data
forecasts <- forecasts |>
  DT(!target_type == "Hosp",) |> #remove hospitalisation
  DT(!type == "point") |> #remove point forecasts
  DT(!model == "data-processed") |> #remove evaluation data
  DT(, type := NULL) |>
  DT(, n_models := NULL) |>
  #only keep models that submit full forecasts
  DT(,  n := .N, by = c("model", "forecast_date", "target_type", "location")) |>
  DT(n == 92) |>
  DT(, n := NULL) |>
  DT()

data.table::fwrite(forecasts, here("data", "forecasts.csv"))


#anomalies <- download_metadata(repo, branch = "main")

#data.table::fwrite(anomalies, here("data", "anomalies.csv"))
