library(data.table)
library(dplyr)
library(here)
library(ggplot2)
library(arrow)
DT <- `[`

nfcsts <- 50
plength <- 10
model_avail <- 0.7

source(here("R", "utils.R"))
source(here("specs", "specs.R"))


fcdat <- fread(here("data", "raw-downloads", "forecasts.csv"))
truth <- fread(here("data", "raw-downloads", "truth.csv"))

loctargets <- specs$loctargets
locs <- unique(substr(loctargets, 1, 2))

# Load observations defined as anomalies
anomalies <- fread(here("data", "raw-downloads",  "anomalies.csv")) |>
  DT(, location_name := NULL)

# Load population data
population <- fread(here("data", "raw-downloads", "population.csv"))

period_cats <- fread(here("data", "auxiliary", "period_cats.csv"))

combdat <- fcdat |>
  filter(location %in% locs) |>
  rename(prediction = value) |>
  mutate(forecast_date =
           lubridate::ceiling_date(forecast_date,
                                   change_on_boundary = FALSE,
                                   unit = "week",
                                   week_start = getOption("lubridate.week.start", 1)) #round down to Saturday
  ) |>
  filter(!model == "EuroCOVIDhub-ensemble") |>
  filter(forecast_date >= as.Date("2021-03-11")) |> #before: 2021-03-20
  filter(forecast_date <= as.Date("2023-03-11")) |>
  DT(prediction<0, prediction := 0) |>
  mutate(forecast_date = as.IDate(forecast_date)) |>
  merge_forecasts_with_truth(truth) |>
  DT(, location_name := NULL) |>
  DT(, scenario_id := NULL) |>
  DT(anomalies, on = c("location", "target_end_date", "target_type"), anomaly_code := i.anomaly) |>
  DT(, anomaly := !is.na(anomaly_code)) |>
  DT(, anomaly_code := NULL) |>
  rescale_to_incidence_rate(population) |>
  DT(order(location, target_type, forecast_date, horizon, model, quantile)) |>
  setcolorder(c("model", "location", "target_type",
                "forecast_date", "horizon", "target_end_date",
                "quantile", "prediction", "true_value",
                "prediction_pop", "true_value_pop",
                "anomaly")) |>
  DT(period_cats, on = c("forecast_date"))

dir.create(here("data", "processed"), recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(combdat, here("data", "processed", "fcdat.parquet"))



