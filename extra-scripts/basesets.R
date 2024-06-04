library(data.table)
library(dplyr)
library(here)
DT <- `[`

source(here("ensvssize", "specs.R"))

start_date <- enscomb_specs$start_date
end_date <- enscomb_specs$end_date

fcdat <- fread(here("data", "depldat.csv")) |>
  DT(forecast_date >= start_date)  |>
  DT(forecast_date <= end_date) |>
  DT(, c("location", "target_type", "forecast_date", "model")) |>
  unique() |>
  DT(, prediction := 10)


ensemble_basesets <-
  expand.grid(forecast_date = unique(fcdat$forecast_date),
              model = unique(fcdat$model),
              location = unique(fcdat$location),
              target_type = unique(fcdat$target_type)) |>
  left_join(fcdat, by = c("forecast_date", "model", "location", "target_type")) |>
  setDT() |>
  DT(, avail := ifelse(is.na(prediction), 0, 1)) |>
  DT(, propavail := mean(avail), by = c("model", "location", "target_type")) |>
  DT(propavail >= 0.9) |>
  DT(, c("model", "location", "target_type", "propavail")) |>
  unique() |>
  DT(order(-propavail)) |>
  DT(, ord := 1:.N, by = c("location", "target_type")) |>
  DT(ord <= 3) |>
  DT(order(location, target_type)) |>
  DT(, c("location", "target_type", "model", "propavail"))
